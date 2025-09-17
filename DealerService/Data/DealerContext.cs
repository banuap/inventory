using Microsoft.EntityFrameworkCore;
using DealerService.Models;

namespace DealerService.Data
{
    /// <summary>
    /// Database context for dealer management
    /// </summary>
    public class DealerContext : DbContext
    {
        public DealerContext(DbContextOptions<DealerContext> options) : base(options)
        {
        }

        public DbSet<Dealer> Dealers { get; set; }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            base.OnModelCreating(modelBuilder);

            // Configure Dealer entity
            modelBuilder.Entity<Dealer>(entity =>
            {
                entity.HasKey(e => e.Id);
                entity.HasIndex(e => e.AccountNumber).IsUnique();
                entity.Property(e => e.AccountNumber).IsRequired().HasMaxLength(50);
                entity.Property(e => e.Address).IsRequired().HasMaxLength(500);
                entity.Property(e => e.CreatedAt).IsRequired();
                entity.Property(e => e.UpdatedAt).IsRequired();
            });
        }
    }
}