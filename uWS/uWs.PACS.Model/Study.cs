using System;
using System.Collections.Generic;

namespace uWs.PACS.Model
{
    public partial class Study
    {
        public int Id { get; set; }

        // Foreign Key to Patient Primary Key 
        public int PatientForeignKey { get; set; }

        public string StudyUid { get; set; }

        public string StudyId { get; set; }

        public string AccessionNumber { get; set; }

        public DateTime StudyDateTime { get; set; }

        public string ModaliyInStudy { get; set; }

        public string RefPhysician { get; set; }

        public string StudyDecription { get; set; }

        #region Study-Patient 
        
        public int PatientAge { get; set; }

        public int PatientSize { get; set; }

        public int PatientWeight { get; set; }

        #endregion

        public int NumberOfRelatedSeries { get; set; }

        public int NumberOfRelatedImage { get; set; }

        public DateTime InsertTime { get; set; }

        public DateTime LastUpdateTime { get; set; }

        public virtual Patient Patient { get; set; }

        public virtual ICollection<Series> Series { get; set; }  
        
    }
}