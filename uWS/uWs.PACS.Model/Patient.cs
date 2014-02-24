﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace uWs.PACS.Model
{
    public enum PatientSex
    {
        Male,
        Female,
        Other
    }

    public partial class Patient
    {
        public int Id { get; set; }

        public string PatientId { get; set; }

        public string PatientName { get; set; }

        public string Issuer { get; set; }

        //public PatientSex PatientSex { get; set; }

        public DateTime PatientBirthDate { get; set; }

        public int NumberOfRelatedStudies { get; set; }

        public int NumberOfRelatedSeries { get; set; }

        public int NumberOfRelatedInstances { get; set; }

        public DateTime InsertTime { get; set; }

        public DateTime LastUpdateTime { get; set; }

        public virtual ICollection<Study> Studies { get; set; } 
        
    }
}
