using Microsoft.EntityFrameworkCore;
using DealerService.Data;

var builder = WebApplication.CreateBuilder(args);

// Add services to the container
builder.Services.AddControllers();

// Add Entity Framework with SQLite
builder.Services.AddDbContext<DealerContext>(options =>
    options.UseSqlite(builder.Configuration.GetConnectionString("DefaultConnection") 
        ?? "Data Source=dealers.db"));

// Learn more about configuring Swagger/OpenAPI at https://aka.ms/aspnetcore/swashbuckle
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();

var app = builder.Build();

// Create database and apply migrations
using (var scope = app.Services.CreateScope())
{
    var context = scope.ServiceProvider.GetRequiredService<DealerContext>();
    context.Database.EnsureCreated();
}

// Configure the HTTP request pipeline.
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}

app.UseHttpsRedirection();

app.UseAuthorization();

app.MapControllers();

// Welcome endpoint
app.MapGet("/", () => new
{
    message = "Dealer Management Microservice",
    version = "1.0",
    endpoints = new
    {
        dealers = "/api/dealers",
        dealer_detail = "/api/dealers/{id}",
        search = "/api/dealers/search?q={query}",
        swagger = "/swagger"
    }
});

app.Run();

// Make the implicit Program class public for testing
public partial class Program { }
