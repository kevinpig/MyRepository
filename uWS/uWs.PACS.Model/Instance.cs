#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

﻿using System;

namespace uWs.PACS.Model
{
    public partial class Instance
    {
        public int Id { get; set; }

        public int SeriesForeignKey { get; set; }

        public int FileFk { get; set; }

        public string SopInstanceUid { get; set; }

        public string SopClassUid { get; set; }

        public int InstanceNumber { get; set; }

        public DateTime ContentDatetime { get; set; }

        public DateTime InsertTime { get; set;}

        public DateTime LastUpdateTime { get; set; }

        public virtual Series Series { get; set; }

        public virtual File File { get; set; }

    }
}