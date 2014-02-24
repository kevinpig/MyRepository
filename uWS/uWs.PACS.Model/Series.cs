using System;
using System.Collections.Generic;

namespace uWs.PACS.Model
{
    public partial class Series
    {
        public int Id { get; set; }

        public int StudyForeignKey { get; set; }

        public string SeriesUid { get; set; }

        public int SeriesNumber { get; set; }

        public string Modality { get; set; }

        public string BodyPart { get; set; }

        public string Institution { get; set; }

        public string StationName { get; set; }

        public string Department { get; set; }

        public string PerfPhysician { get; set; }

        public DateTime SeriesDate { get; set; }

        public DateTime SeriesTime { get; set; }

        public string SourceAet { get; set; }

        public string SeriesDescription { get; set; }

        public DateTime InsertTime { get; set; }

        public DateTime LastUpdateTime { get; set; }
        
        public int NumberOfRelatedImage { get; set; }

        public virtual Study Study { get; set; }

        public virtual ICollection<Instance> Instances { get; set; } 
    }
}