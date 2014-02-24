#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

﻿using System.Data.Entity.ModelConfiguration;

namespace uWs.PACS.Model.Mapping
{
    public class FileMap : EntityTypeConfiguration<File>
    {
        public FileMap()
        {
            // Primary Key 
            HasKey(t => t.Id);

            // Properties 

            // Table & Columns 
            ToTable("File");

            // Relationship
            this.HasRequired(t => t.Instance)
                .WithRequiredPrincipal(t => t.File);

        }
    }
}