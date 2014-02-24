using System.Data.Entity.ModelConfiguration;

namespace uWs.PACS.Model.Mapping
{
    public class ServerPartitionMap : EntityTypeConfiguration<ServerPartition>
    {
         public ServerPartitionMap()
         {
             // Primary Key 
             HasKey(t => t.Id);

             // Columns 

             // Tables 
             ToTable("ServerPartition");

             // relationship
        }
    }
}