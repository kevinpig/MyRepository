#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

﻿using System.Data.Entity.ModelConfiguration;

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