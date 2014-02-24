using System.Data.Entity.ModelConfiguration;

namespace uWs.PACS.Model.Mapping
{
    public class FileSystemMap : EntityTypeConfiguration<FileSystem>
    {
         public FileSystemMap()
         {
             HasKey(t => t.Id);

             ToTable("FileSystem");

             // Relationship 
             this.HasOptional(t => t.NextFileSystem)
                 .WithMany(t => t.PriviouseFileSystems)
                 .HasForeignKey(t => t.NextFileSytemFK);
         }
    }
}