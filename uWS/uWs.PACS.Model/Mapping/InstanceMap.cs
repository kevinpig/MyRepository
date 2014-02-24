using System.Data.Entity.ModelConfiguration;

namespace uWs.PACS.Model.Mapping
{
    public class InstanceMap : EntityTypeConfiguration<Instance>
    {
         public InstanceMap()
         {
             // Primary Key
             this.HasKey(t => t.Id);

             // Properties 
             this.Property(t => t.SopInstanceUid)
                 .HasMaxLength(64)
                 .IsRequired();

             // Table & Columns
             ToTable("Instance");

             // RelationShip 
             this.HasRequired(t => t.Series)
                 .WithMany(s => s.Instances)
                 .HasForeignKey(t => t.SeriesForeignKey);

             this.HasRequired(t => t.File)
                 .WithRequiredPrincipal(t => t.Instance);
         }
    }
}