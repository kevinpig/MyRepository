using System.Data.Entity.ModelConfiguration;

namespace uWs.PACS.Model.Mapping
{
    public class StudyMap : EntityTypeConfiguration<Study>
    {
         public StudyMap()
         {
             // Primary Key
             HasKey(t => t.Id);

             // Properties 
             Property(t => t.StudyUid).HasMaxLength(64)
                                      .IsRequired();

             // Table & Column Mappings
             ToTable("Study");

             // RelationShip

             this.HasRequired(s => s.Patient)
                 .WithMany(p => p.Studies)
                 .HasForeignKey(s => s.PatientForeignKey);
         }
    }
}