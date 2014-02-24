using System.Data.Entity.ModelConfiguration;

namespace uWs.PACS.Model.Mapping
{
    public class SupportedSopClassMap : EntityTypeConfiguration<SupportedSopClass>
    {
         public SupportedSopClassMap()
         {
             HasKey(t => t.Id);

             Property(t => t.SopClassUid)
                 .HasMaxLength(64);

             ToTable("SupportedSopClass");
         }
    }
}