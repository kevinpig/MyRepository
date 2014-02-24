#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

﻿using System.Data.Entity.ModelConfiguration;

namespace uWs.PACS.Model.Mapping
{
    public class SeriesMap : EntityTypeConfiguration<Series>
    {
        public SeriesMap()
        {
            // Primary Key 
            HasKey(t => t.Id);

            // Properties
            Property(t => t.SeriesUid)
                .HasMaxLength(64)
                .IsRequired();

            // Table & Column Mappings
            ToTable("Series");

            // RelationShip
            this.HasRequired(t => t.Study)
                .WithMany(t => t.Series)
                .HasForeignKey(s => s.StudyForeignKey);
        }
    }
}