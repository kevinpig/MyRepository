﻿using System;
using System.IO;
using System.Linq;
using uWS.Dicom;
using uWs.PACS.Model;

namespace uWS.Pacs.BussinessLogic
{
    /// <summary>
    /// Helper class to import a DICOM image into the system.
    /// </summary>
    public class SopInstanceImporter
    {
        #region Private Members
        private readonly SopInstanceImporterContext _context;
        #endregion

        #region Constructors

        /// <summary>
        /// Creates an instance of <see cref="SopInstanceImporter"/> to import DICOM object(s)
        /// into the system.
        /// </summary>
        /// <param name="context">The context of the operation.</param>
        public SopInstanceImporter(SopInstanceImporterContext context)
        {
            _context = context;
        }

        #endregion

        public bool Import(DicomMessage dicomMessage)
        {
            if (dicomMessage.DataSet == null)
            {
                throw new ArgumentException();
            }

            var patient = new Patient
            {
                PatientId = dicomMessage.DataSet[DicomTags.PatientId].GetString(0, string.Empty),
                PatientName = dicomMessage.DataSet[DicomTags.PatientsName].GetString(0, string.Empty),
                PatientBirthDate = dicomMessage.DataSet[DicomTags.PatientsBirthDate].GetString(0, string.Empty),
                Issuer = dicomMessage.DataSet[DicomTags.IssuerOfPatientId].GetString(0, string.Empty),
            };

            var study = new Study()
            {
                StudyId = dicomMessage.DataSet[DicomTags.StudyId].GetString(0, string.Empty),
                StudyUid = dicomMessage.DataSet[DicomTags.StudyInstanceUid].GetString(0, string.Empty),
                AccessionNumber = dicomMessage.DataSet[DicomTags.AccessionNumber].GetString(0, string.Empty),
                StudyDate = dicomMessage.DataSet[DicomTags.StudyDate].GetString(0, string.Empty),
                StudyTime = dicomMessage.DataSet[DicomTags.StudyTime].GetString(0, string.Empty),
                RefPhysician = dicomMessage.DataSet[DicomTags.ReferringPhysiciansName].GetString(0, string.Empty),
                StudyDescription = dicomMessage.DataSet[DicomTags.StudyDescription].GetString(0, string.Empty),

                PatientId = patient.PatientId,
                PatientName = patient.PatientName,
                PatientBirthday = patient.PatientBirthDate,
                PatientAge = dicomMessage.DataSet[DicomTags.PatientsAge].GetString(0, string.Empty),
                PatientSize = dicomMessage.DataSet[DicomTags.PatientsSize].GetString(0, string.Empty),
                PatientWeight = dicomMessage.DataSet[DicomTags.PatientsWeight].GetString(0, string.Empty),
                PatientSex = dicomMessage.DataSet[DicomTags.PatientsSex].GetString(0, string.Empty)
            };

            var series = new Series()
            {
                SeriesUid = dicomMessage.DataSet[DicomTags.SeriesInstanceUid].GetString(0, string.Empty),
                SeriesNumber = dicomMessage.DataSet[DicomTags.SeriesNumber].GetString(0, string.Empty),
                Modality = dicomMessage.DataSet[DicomTags.Modality].GetString(0, string.Empty),
                BodyPart = dicomMessage.DataSet[DicomTags.BodyPartExamined].GetString(0, string.Empty),
                Institution = dicomMessage.DataSet[DicomTags.InstitutionName].GetString(0, string.Empty),
                StationName = dicomMessage.DataSet[DicomTags.StationName].GetString(0, string.Empty),
                Department = dicomMessage.DataSet[DicomTags.InstitutionalDepartmentName].GetString(0, string.Empty),
                PerfPhysician = dicomMessage.DataSet[DicomTags.PerformingPhysiciansName].GetString(0, string.Empty),
                SeriesDate = dicomMessage.DataSet[DicomTags.SeriesDate].GetString(0, string.Empty),
                SeriesTime = dicomMessage.DataSet[DicomTags.SeriesTime].GetString(0, string.Empty),
                SeriesDescription = dicomMessage.DataSet[DicomTags.SeriesDescription].GetString(0, string.Empty),
                PerformedProcedureStepStartDate = dicomMessage.DataSet[DicomTags.PerformedProcedureStepStartDate].GetString(0, string.Empty),
                PerformedProcedureStepStartTime = dicomMessage.DataSet[DicomTags.PerformedProcedureStepStartTime].GetString(0, string.Empty),
            };

            var instance = new Instance()
            {
                SopInstanceUid = dicomMessage.DataSet[DicomTags.SopInstanceUid].GetString(0, string.Empty),
                SopClassUid = dicomMessage.DataSet[DicomTags.SopClassUid].GetString(0, string.Empty),
                InstanceNumber = dicomMessage.DataSet[DicomTags.InstanceNumber].GetString(0, string.Empty),
                ContentDate = dicomMessage.DataSet[DicomTags.ContentDate].GetString(0, string.Empty),
                ContentTime = dicomMessage.DataSet[DicomTags.ContentTime].GetString(0, string.Empty)
            };

            if (string.IsNullOrEmpty(study.StudyUid)
                || string.IsNullOrEmpty(series.SeriesUid)
                || string.IsNullOrEmpty(instance.SopInstanceUid))
            {
                throw new ArgumentException();
            }

            // Get Patient Db Object 
            using (var context = new PacsContext())
            {
                Patient dbPatient = null;

                var dbStudy = InsertStudy(context, study, patient, out dbPatient);

                // Patient and study is exist in db now
                var dbSeries = InsertSeries(context, series, dbStudy, dbPatient);

                // insert instance 
                var dbInstance = InsertImage(context, instance, dbSeries, dbStudy, dbPatient);

                var dbFile = InsertFile(context, dbInstance, dbSeries, dbStudy, dbPatient);

                var dicomFile = new DicomFile(dicomMessage, dbFile.FilePath);
                
                if (!Directory.Exists(Path.GetDirectoryName(dbFile.FilePath)))
                {
                    Directory.CreateDirectory(Path.GetDirectoryName(dbFile.FilePath));
                }

                dicomFile.Save();

                context.SaveChanges();
            }

            return true;
        }

        #region Private method

        private Study InsertStudy(PacsContext context, Study study, Patient patient, out Patient dbPatient)
        {
            var dbStudy = context.Studies.FirstOrDefault(s => s.StudyUid.Equals(study.StudyUid));
            if (dbStudy == null)
            {
                // check is patient exist by patient id, patient name and so on. 
                var tempResult = (context.Patients.Where(p => p.PatientId.Equals(patient.PatientId))).ToList();

                // TODO Patient strategy 
                dbPatient = tempResult.FirstOrDefault();
                if (dbPatient == null)
                {
                    patient.LastUpdateTime = DateTime.Now;
                    patient.InsertTime = DateTime.Now;
                    context.Patients.Add(patient);
                    dbPatient = patient;
                }

                // Insert Study info 
                dbStudy = study;
                dbStudy.PatientForeignKey = dbPatient.Id;
                dbStudy.InsertTime = DateTime.Now;
                context.Studies.Add(dbStudy);

                dbPatient.NumberOfRelatedStudies += 1;
            }
            else
            {
                dbPatient = dbStudy.Patient;
            }

            return dbStudy;
        }

        private Series InsertSeries(PacsContext context, Series series, Study dbStudy, Patient dbPatient)
        {
            var dbSeries = context.Series.FirstOrDefault(s => s.SeriesUid.Equals(series.SeriesUid));
            if (dbSeries == null)
            {
                dbSeries = series;
                dbSeries.StudyForeignKey = dbStudy.Id;
                dbSeries.InsertTime = DateTime.Now;
                context.Series.Add(dbSeries);

                dbStudy.NumberOfRelatedSeries += 1;
                dbPatient.NumberOfRelatedSeries += 1;
            }

            return dbSeries;
        }

        private Instance InsertImage(PacsContext context, Instance instance, Series dbSeries, Study dbStudy,
                                        Patient dbPatient)
        {
            var dbInstance = context.Instances.FirstOrDefault(i => i.SopInstanceUid.Equals(instance.SopInstanceUid));
            if (dbInstance == null)
            {
                dbInstance = instance;
                dbInstance.SeriesForeignKey = dbSeries.Id;
                dbInstance.LastUpdateTime = DateTime.Now;
                dbInstance.InsertTime = DateTime.Now;
                context.Instances.Add(dbInstance);

                dbSeries.NumberOfRelatedImage += 1;
                dbStudy.NumberOfRelatedImage += 1;
                dbPatient.NumberOfRelatedInstances += 1;
            }

            dbInstance.LastUpdateTime = DateTime.Now;
            dbSeries.LastUpdateTime = DateTime.Now;
            dbStudy.LastUpdateTime = DateTime.Now;
            dbPatient.LastUpdateTime = DateTime.Now;

            return dbInstance;
        }

        private uWs.PACS.Model.File InsertFile(PacsContext context, Instance instance, Series dbSeries, Study dbStudy,
            Patient dbPatient)
        {
            var partition = context.ServerPartitions.First(p => p.Id == _context.Partition.Id);

            string fsDir = partition.FileSystem.DirPath;

            var filepath = Path.Combine(fsDir, dbPatient.PatientId, dbStudy.StudyUid, dbSeries.SeriesUid,
                                        instance.SopInstanceUid);

            var dbFile = context.Files.FirstOrDefault(f => f.InstanceFk.Equals(instance.Id));
            if (dbFile == null)
            {
                dbFile = new uWs.PACS.Model.File
                    {
                        CreateTime = DateTime.Now
                    };
                context.Files.Add(dbFile);
            }

            dbFile.InstanceFk = instance.Id;
            dbFile.FilePath = filepath;
            dbFile.FileSystemFk = partition.FileSystem.Id;

            return dbFile;
        }

        #endregion
    }
}