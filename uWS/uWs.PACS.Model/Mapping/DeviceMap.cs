﻿using System.Data.Entity.ModelConfiguration;

namespace uWs.PACS.Model.Mapping
{
    public class DeviceMap : EntityTypeConfiguration<Device>
    {
         // Primary Key 
        public DeviceMap()
        {
            HasKey(t => t.Id);

            // Properties 

            // Table & Columes 
            ToTable("Device");

            // RelationShips 
            this.HasRequired(t => t.ServerPartition)
                .WithMany(s => s.Devices)
                .HasForeignKey(t => t.ServerPartitionPK);
        }
    }
}