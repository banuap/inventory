using System.ComponentModel.DataAnnotations;

namespace DealerService.Models
{
    /// <summary>
    /// Dealer model for inventory management
    /// </summary>
    public class Dealer
    {
        /// <summary>
        /// Unique identifier for the dealer
        /// </summary>
        public int Id { get; set; }

        /// <summary>
        /// Unique dealer account number
        /// </summary>
        [Required]
        [StringLength(50)]
        public string AccountNumber { get; set; } = string.Empty;

        /// <summary>
        /// Dealer address
        /// </summary>
        [Required]
        [StringLength(500)]
        public string Address { get; set; } = string.Empty;

        /// <summary>
        /// When the dealer was created
        /// </summary>
        public DateTime CreatedAt { get; set; } = DateTime.UtcNow;

        /// <summary>
        /// When the dealer was last updated
        /// </summary>
        public DateTime UpdatedAt { get; set; } = DateTime.UtcNow;
    }
}