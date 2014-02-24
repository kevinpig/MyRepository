using System.Data.Entity.ModelConfiguration;

namespace uWs.PACS.Model.Mapping
{
    public class PatientMap : EntityTypeConfiguration<Patient>
    {
         public PatientMap()
         {
             HasKey(t => t.Id);

             Property(t => t.PatientId)
                 .HasMaxLength(64);

             // Table & Column Mappings
             ToTable("Patient");
         }
    }
}