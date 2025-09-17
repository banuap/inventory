using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;
using DealerService.Data;
using DealerService.Models;
using System.Net.Http.Json;
using System.Net;
using System.Text.Json;
using Xunit;

namespace DealerService.Tests
{
    public class DealersControllerTests : IClassFixture<WebApplicationFactory<Program>>
    {
        private readonly WebApplicationFactory<Program> _factory;
        private readonly HttpClient _client;

        public DealersControllerTests(WebApplicationFactory<Program> factory)
        {
            _factory = factory.WithWebHostBuilder(builder =>
            {
                builder.ConfigureServices(services =>
                {
                    // Remove the real database context
                    var descriptor = services.SingleOrDefault(
                        d => d.ServiceType == typeof(DbContextOptions<DealerContext>));
                    if (descriptor != null)
                        services.Remove(descriptor);

                    // Add in-memory database for testing
                    services.AddDbContext<DealerContext>(options =>
                    {
                        options.UseInMemoryDatabase("TestDb");
                    });
                });
            });

            _client = _factory.CreateClient();
        }

        [Fact]
        public async Task GetDealers_EmptyDatabase_ReturnsEmptyList()
        {
            // Act
            var response = await _client.GetAsync("/api/dealers");
            
            // Assert
            response.EnsureSuccessStatusCode();
            var dealers = await response.Content.ReadFromJsonAsync<List<Dealer>>();
            Assert.NotNull(dealers);
            Assert.Empty(dealers);
        }

        [Fact]
        public async Task CreateDealer_ValidData_ReturnsCreatedDealer()
        {
            // Arrange
            var dealer = new Dealer
            {
                AccountNumber = "ACC-001",
                Address = "123 Main St, City, State"
            };

            // Act
            var response = await _client.PostAsJsonAsync("/api/dealers", dealer);
            
            // Assert
            Assert.Equal(HttpStatusCode.Created, response.StatusCode);
            var createdDealer = await response.Content.ReadFromJsonAsync<Dealer>();
            Assert.NotNull(createdDealer);
            Assert.Equal(dealer.AccountNumber, createdDealer.AccountNumber);
            Assert.Equal(dealer.Address, createdDealer.Address);
            Assert.True(createdDealer.Id > 0);
        }

        [Fact]
        public async Task CreateDealer_DuplicateAccountNumber_ReturnsConflict()
        {
            // Arrange
            var dealer1 = new Dealer
            {
                AccountNumber = "ACC-002",
                Address = "123 Main St"
            };
            var dealer2 = new Dealer
            {
                AccountNumber = "ACC-002", // Same account number
                Address = "456 Oak Ave"
            };

            // Act
            await _client.PostAsJsonAsync("/api/dealers", dealer1);
            var response = await _client.PostAsJsonAsync("/api/dealers", dealer2);
            
            // Assert
            Assert.Equal(HttpStatusCode.Conflict, response.StatusCode);
        }

        [Fact]
        public async Task CreateDealer_InvalidData_ReturnsBadRequest()
        {
            // Arrange
            var dealer = new Dealer
            {
                AccountNumber = "", // Invalid - empty
                Address = "123 Main St"
            };

            // Act
            var response = await _client.PostAsJsonAsync("/api/dealers", dealer);
            
            // Assert
            Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode);
        }

        [Fact]
        public async Task GetDealer_ExistingId_ReturnsDealer()
        {
            // Arrange
            var dealer = new Dealer
            {
                AccountNumber = "ACC-003",
                Address = "789 Pine St"
            };
            
            var createResponse = await _client.PostAsJsonAsync("/api/dealers", dealer);
            var createdDealer = await createResponse.Content.ReadFromJsonAsync<Dealer>();

            // Act
            var response = await _client.GetAsync($"/api/dealers/{createdDealer!.Id}");
            
            // Assert
            response.EnsureSuccessStatusCode();
            var retrievedDealer = await response.Content.ReadFromJsonAsync<Dealer>();
            Assert.NotNull(retrievedDealer);
            Assert.Equal(createdDealer.Id, retrievedDealer.Id);
            Assert.Equal(dealer.AccountNumber, retrievedDealer.AccountNumber);
        }

        [Fact]
        public async Task GetDealer_NonExistentId_ReturnsNotFound()
        {
            // Act
            var response = await _client.GetAsync("/api/dealers/999");
            
            // Assert
            Assert.Equal(HttpStatusCode.NotFound, response.StatusCode);
        }

        [Fact]
        public async Task UpdateDealer_ValidData_ReturnsUpdatedDealer()
        {
            // Arrange
            var dealer = new Dealer
            {
                AccountNumber = "ACC-004",
                Address = "123 Main St"
            };
            
            var createResponse = await _client.PostAsJsonAsync("/api/dealers", dealer);
            var createdDealer = await createResponse.Content.ReadFromJsonAsync<Dealer>();

            // Update the dealer
            createdDealer!.Address = "456 Updated St";

            // Act
            var response = await _client.PutAsJsonAsync($"/api/dealers/{createdDealer.Id}", createdDealer);
            
            // Assert
            response.EnsureSuccessStatusCode();
            var updatedDealer = await response.Content.ReadFromJsonAsync<Dealer>();
            Assert.NotNull(updatedDealer);
            Assert.Equal("456 Updated St", updatedDealer.Address);
        }

        [Fact]
        public async Task UpdateDealer_IdMismatch_ReturnsBadRequest()
        {
            // Arrange
            var dealer = new Dealer
            {
                Id = 999,
                AccountNumber = "ACC-005",
                Address = "123 Main St"
            };

            // Act
            var response = await _client.PutAsJsonAsync("/api/dealers/1", dealer);
            
            // Assert
            Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode);
        }

        [Fact]
        public async Task DeleteDealer_ExistingId_ReturnsSuccess()
        {
            // Arrange
            var dealer = new Dealer
            {
                AccountNumber = "ACC-006",
                Address = "123 Main St"
            };
            
            var createResponse = await _client.PostAsJsonAsync("/api/dealers", dealer);
            var createdDealer = await createResponse.Content.ReadFromJsonAsync<Dealer>();

            // Act
            var response = await _client.DeleteAsync($"/api/dealers/{createdDealer!.Id}");
            
            // Assert
            response.EnsureSuccessStatusCode();
        }

        [Fact]
        public async Task DeleteDealer_NonExistentId_ReturnsNotFound()
        {
            // Act
            var response = await _client.DeleteAsync("/api/dealers/999");
            
            // Assert
            Assert.Equal(HttpStatusCode.NotFound, response.StatusCode);
        }

        [Fact]
        public async Task SearchDealers_WithQuery_ReturnsMatchingDealers()
        {
            // Arrange
            var dealer1 = new Dealer { AccountNumber = "SEARCH-001", Address = "123 Main St" };
            var dealer2 = new Dealer { AccountNumber = "TEST-002", Address = "456 Main Ave" };
            var dealer3 = new Dealer { AccountNumber = "OTHER-003", Address = "789 Pine St" };

            await _client.PostAsJsonAsync("/api/dealers", dealer1);
            await _client.PostAsJsonAsync("/api/dealers", dealer2);
            await _client.PostAsJsonAsync("/api/dealers", dealer3);

            // Act
            var response = await _client.GetAsync("/api/dealers/search?q=Main");
            
            // Assert
            response.EnsureSuccessStatusCode();
            var dealers = await response.Content.ReadFromJsonAsync<List<Dealer>>();
            Assert.NotNull(dealers);
            Assert.Equal(2, dealers.Count);
        }

        [Fact]
        public async Task SearchDealers_EmptyQuery_ReturnsBadRequest()
        {
            // Act
            var response = await _client.GetAsync("/api/dealers/search?q=");
            
            // Assert
            Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode);
        }

        [Fact]
        public async Task HomeEndpoint_ReturnsWelcomeMessage()
        {
            // Act
            var response = await _client.GetAsync("/");
            
            // Assert
            response.EnsureSuccessStatusCode();
            var content = await response.Content.ReadAsStringAsync();
            Assert.Contains("Dealer Management Microservice", content);
        }
    }
}