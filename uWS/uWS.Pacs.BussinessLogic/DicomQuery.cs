using System;
using System.Collections.Generic;
using System.Data.Entity.Core.Objects;
using System.Data.Entity.Infrastructure;
using System.Linq;
using uWS.Dicom;
using uWs.PACS.Model;

namespace uWS.Pacs.BussinessLogic
{
    public class StduyQueryFilter
    {
        [DicomField(DicomTags.PatientId)]
        public string PatientId { get; set; }

        [DicomField(DicomTags.PatientsName)]
        public string PatientName { get; set; }

        [DicomField(DicomTags.StudyDate)]
        public string StudyDate { get; set; }

        [DicomField(DicomTags.StudyTime)]
        public string StudyTime { get; set; }

        [DicomField(DicomTags.AccessionNumber)]
        public string AccessionNumber { get; set; }

        [DicomField(DicomTags.StudyId)]
        public string StudyId { get; set; }

        [DicomField(DicomTags.ModalitiesInStudy)]
        public string ModalitiesInStudy { get; set; }

        [DicomField(DicomTags.StudyDescription)]
        public string StduyDescription { get; set; }

        [DicomField(DicomTags.ReferringPhysiciansName)]
        public string ReferPhysicianName { get; set; }
    }

    public class PatientQueryFilter
    {
        [DicomField(DicomTags.PatientsName)]
        public string PatientName { get; set; }

        [DicomField(DicomTags.PatientId)]
        public string PatientId { get; set; }

        [DicomField(DicomTags.IssuerOfPatientId)]
        public string IssureOfPatientId { get; set; }

        [DicomField(DicomTags.PatientsSex)]
        public string PatientSex { get; set; }

        [DicomField(DicomTags.PatientsBirthDate)]
        public string PatientBirthDate { get; set; }
    }

    public class DicomQuery
    {
        public List<DicomAttributeCollection> PatientQuery(DicomAttributeCollection queryCriera)
        {
            throw new NotImplementedException();
        }

        private List<DicomAttributeCollection> StudyQuery(DicomAttributeCollection queryCriteria)
        {
            var filter = new StduyQueryFilter();
            queryCriteria.LoadDicomFields(filter);
            using (var pacsContext = new PacsContext())
            {
                var adapter = (IObjectContextAdapter) pacsContext;
                var query = new ObjectQuery<Study>("Study", adapter.ObjectContext); 

                if (string.IsNullOrEmpty(filter.PatientId))
                {
                    query = query.Where(string.Format(@"it.PatientId = '{0}'", filter.PatientId));
                }

                if (string.IsNullOrEmpty(filter.PatientName))
                {
                    query = query.Where(QueryHelper.SetStringCondition("PatientName", filter.PatientName));
                }

                if (string.IsNullOrEmpty(filter.StudyDate))
                {
                    query = query.Where(QueryHelper.SetRangeCondition("StudyDate", filter.StudyDate));
                }

                var result = query.ToList();
            }

            throw new NotImplementedException();
        }

        private void PopulateStudy(DicomMessageBase response, IEnumerable<DicomTag> tagList, Study study)
        {
            DicomAttributeCollection dataset = response.DataSet;

            dataset[DicomTags.RetrieveAeTitle].SetStringValue("");


        }
    }
}