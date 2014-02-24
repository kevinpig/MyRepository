#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

﻿using System;
using System.Collections.Generic;

namespace uWs.PACS.Model
{
    public partial class Instance
    {
        public int Id { get; set; }

        public int SeriesForeignKey { get; set; }

        public string SopInstanceUid { get; set; }

        public string SopClassUid { get; set; }

        public string InstanceNumber { get; set; }

        public string ContentDate { get; set; }

        public string ContentTime { get; set; }

        public DateTime InsertTime { get; set;}

        public DateTime LastUpdateTime { get; set; }

        public virtual Series Series { get; set; }

        public virtual ICollection<File> Files { get; set; }
    }
}