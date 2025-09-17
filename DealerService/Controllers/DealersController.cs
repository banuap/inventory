using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;
using DealerService.Data;
using DealerService.Models;

namespace DealerService.Controllers
{
    /// <summary>
    /// API Controller for dealer management operations
    /// </summary>
    [ApiController]
    [Route("api/[controller]")]
    public class DealersController : ControllerBase
    {
        private readonly DealerContext _context;
        private readonly ILogger<DealersController> _logger;

        public DealersController(DealerContext context, ILogger<DealersController> logger)
        {
            _context = context;
            _logger = logger;
        }

        /// <summary>
        /// Get all dealers
        /// </summary>
        /// <returns>List of all dealers</returns>
        [HttpGet]
        public async Task<ActionResult<IEnumerable<Dealer>>> GetDealers()
        {
            try
            {
                var dealers = await _context.Dealers.ToListAsync();
                return Ok(dealers);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error retrieving dealers");
                return StatusCode(500, new { error = "Failed to retrieve dealers" });
            }
        }

        /// <summary>
        /// Get a specific dealer by ID
        /// </summary>
        /// <param name="id">Dealer ID</param>
        /// <returns>Dealer details</returns>
        [HttpGet("{id}")]
        public async Task<ActionResult<Dealer>> GetDealer(int id)
        {
            try
            {
                var dealer = await _context.Dealers.FindAsync(id);
                if (dealer == null)
                {
                    return NotFound(new { error = "Dealer not found" });
                }
                return Ok(dealer);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error retrieving dealer {Id}", id);
                return StatusCode(500, new { error = "Failed to retrieve dealer" });
            }
        }

        /// <summary>
        /// Create a new dealer
        /// </summary>
        /// <param name="dealer">Dealer data</param>
        /// <returns>Created dealer</returns>
        [HttpPost]
        public async Task<ActionResult<Dealer>> CreateDealer(Dealer dealer)
        {
            try
            {
                // Check if account number already exists
                var existingDealer = await _context.Dealers
                    .FirstOrDefaultAsync(d => d.AccountNumber == dealer.AccountNumber);
                
                if (existingDealer != null)
                {
                    return Conflict(new { error = "Dealer with this account number already exists" });
                }

                dealer.CreatedAt = DateTime.UtcNow;
                dealer.UpdatedAt = DateTime.UtcNow;

                _context.Dealers.Add(dealer);
                await _context.SaveChangesAsync();

                return CreatedAtAction(nameof(GetDealer), new { id = dealer.Id }, dealer);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error creating dealer");
                return StatusCode(500, new { error = "Failed to create dealer" });
            }
        }

        /// <summary>
        /// Update an existing dealer
        /// </summary>
        /// <param name="id">Dealer ID</param>
        /// <param name="dealer">Updated dealer data</param>
        /// <returns>Updated dealer</returns>
        [HttpPut("{id}")]
        public async Task<ActionResult<Dealer>> UpdateDealer(int id, Dealer dealer)
        {
            try
            {
                if (id != dealer.Id)
                {
                    return BadRequest(new { error = "ID mismatch" });
                }

                var existingDealer = await _context.Dealers.FindAsync(id);
                if (existingDealer == null)
                {
                    return NotFound(new { error = "Dealer not found" });
                }

                // Check if account number already exists for a different dealer
                var duplicateDealer = await _context.Dealers
                    .FirstOrDefaultAsync(d => d.AccountNumber == dealer.AccountNumber && d.Id != id);
                
                if (duplicateDealer != null)
                {
                    return Conflict(new { error = "Dealer with this account number already exists" });
                }

                existingDealer.AccountNumber = dealer.AccountNumber;
                existingDealer.Address = dealer.Address;
                existingDealer.UpdatedAt = DateTime.UtcNow;

                await _context.SaveChangesAsync();

                return Ok(existingDealer);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error updating dealer {Id}", id);
                return StatusCode(500, new { error = "Failed to update dealer" });
            }
        }

        /// <summary>
        /// Delete a dealer
        /// </summary>
        /// <param name="id">Dealer ID</param>
        /// <returns>Success message</returns>
        [HttpDelete("{id}")]
        public async Task<ActionResult> DeleteDealer(int id)
        {
            try
            {
                var dealer = await _context.Dealers.FindAsync(id);
                if (dealer == null)
                {
                    return NotFound(new { error = "Dealer not found" });
                }

                _context.Dealers.Remove(dealer);
                await _context.SaveChangesAsync();

                return Ok(new { message = "Dealer deleted successfully" });
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error deleting dealer {Id}", id);
                return StatusCode(500, new { error = "Failed to delete dealer" });
            }
        }

        /// <summary>
        /// Search dealers by account number or address
        /// </summary>
        /// <param name="q">Search query</param>
        /// <returns>Matching dealers</returns>
        [HttpGet("search")]
        public async Task<ActionResult<IEnumerable<Dealer>>> SearchDealers([FromQuery] string q)
        {
            try
            {
                if (string.IsNullOrWhiteSpace(q))
                {
                    return BadRequest(new { error = "Search query is required" });
                }

                var dealers = await _context.Dealers
                    .Where(d => d.AccountNumber.Contains(q) || d.Address.Contains(q))
                    .ToListAsync();

                return Ok(dealers);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error searching dealers with query: {Query}", q);
                return StatusCode(500, new { error = "Failed to search dealers" });
            }
        }
    }
}