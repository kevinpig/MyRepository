#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Data.Entity.Core.Objects;
using System.Data.Entity.Infrastructure;
using System.Linq;
using System.Threading;
using uWS.Common;
using uWS.Dicom;
using uWS.Dicom.Network;
using uWS.Dicom.Network.Scp;
using uWS.Pacs.BussinessLogic;
using uWs.PACS.Model;
using Instance = uWs.PACS.Model.Instance;

namespace uWS.Pacs.DicomService
{
    [Export(typeof(IDicomScp<DicomScpContext>))]
    public class CFindScp : BaseScp
    {
        #region Private members

        private readonly List<SupportedSop> _list = new List<SupportedSop>();
        private bool _cancelReceived;
        private readonly object _syncLock = new object();

        private readonly Queue<DicomMessage> _responseQueue = new Queue<DicomMessage>(5);

        #endregion

        public CFindScp()
        {
            var sop = new SupportedSop
            {
                SopClass = SopClass.PatientRootQueryRetrieveInformationModelFind
            };
            sop.SyntaxList.Add(TransferSyntax.ExplicitVrLittleEndian);
            sop.SyntaxList.Add(TransferSyntax.ImplicitVrLittleEndian);
            _list.Add(sop);

            sop = new SupportedSop
            {
                SopClass = SopClass.StudyRootQueryRetrieveInformationModelFind
            };
            sop.SyntaxList.Add(TransferSyntax.ExplicitVrLittleEndian);
            sop.SyntaxList.Add(TransferSyntax.ImplicitVrLittleEndian);
            _list.Add(sop);
        }

        #region Private Method

        /// <summary>
        ///  Populate data from a <see cref="Patient"/> entity into a DICOM C-FIND-RSP message
        /// </summary>
        /// <param name="response"></param>
        /// <param name="tagList"></param>
        /// <param name="row"></param>
        private void PopulatePatient(DicomMessage response, IEnumerable<DicomTag> tagList, Patient row)
        {
            DicomAttributeCollection dataset = response.DataSet;

            dataset[DicomTags.RetrieveAeTitle].SetStringValue(base.Partition.AeTitle);

            // character set
            var characterSet = GetPreferredCharacterSet();
            if (!string.IsNullOrEmpty(characterSet))
            {
                dataset[DicomTags.SpecificCharacterSet].SetStringValue(characterSet);
                dataset.SpecificCharacterSet = characterSet;
            }

            var relatedStudies = row.Studies;
            Study study = null;
            if (relatedStudies.Count > 0)
                study = relatedStudies.First();

            foreach (DicomTag tag in tagList)
            {
                try
                {
                    switch (tag.TagValue)
                    {
                        case DicomTags.PatientsName:
                            dataset[DicomTags.PatientsName].SetStringValue(row.PatientName);
                            break;
                        case DicomTags.PatientId:
                            dataset[DicomTags.PatientId].SetStringValue(row.PatientId);
                            break;
                        case DicomTags.IssuerOfPatientId:
                            dataset[DicomTags.IssuerOfPatientId].SetStringValue(row.Issuer);
                            break;
                        case DicomTags.NumberOfPatientRelatedStudies:
                            dataset[DicomTags.NumberOfPatientRelatedStudies].AppendInt32(row.NumberOfRelatedStudies);
                            break;
                        case DicomTags.NumberOfPatientRelatedSeries:
                            dataset[DicomTags.NumberOfPatientRelatedSeries].AppendInt32(row.NumberOfRelatedSeries);
                            break;
                        case DicomTags.NumberOfPatientRelatedInstances:
                            dataset[DicomTags.NumberOfPatientRelatedInstances].AppendInt32(row.NumberOfRelatedInstances);
                            break;
                        case DicomTags.QueryRetrieveLevel:
                            dataset[DicomTags.QueryRetrieveLevel].SetStringValue("PATIENT");
                            break;
                        case DicomTags.PatientsSex:
                            if (study != null)
                            {
                                dataset[DicomTags.PatientsSex].SetStringValue(study.PatientSex);
                            }
                            else
                            {
                                dataset[DicomTags.PatientsSex].SetNullValue();
                            }
                            break;
                        case DicomTags.PatientsBirthDate:
                            dataset[DicomTags.PatientsBirthDate].SetStringValue(row.PatientBirthDate);
                            break;

                        // Meta tags that should have not been in the RQ, but we've already set
                        case DicomTags.RetrieveAeTitle:
                        case DicomTags.InstanceAvailability:
                        case DicomTags.SpecificCharacterSet:
                            break;

                        default:
                            if (!tag.IsPrivate)
                                dataset[tag].SetNullValue();

                            // extension ? 
                            break;
                    }
                }
                catch (Exception e)
                {
                    Platform.Log(LogLevel.Warn, e, "Unexpected error setting tag {0} in C-FIND-RSP",
                                dataset[tag].Tag.ToString());
                    if (!tag.IsPrivate)
                        dataset[tag].SetNullValue();
                }
            }

        }

        private void PopulateStudy(DicomMessage response, IEnumerable<DicomTag> tagList, Study row)
        {
            DicomAttributeCollection dataSet = response.DataSet;

            dataSet[DicomTags.RetrieveAeTitle].SetStringValue(base.Partition.AeTitle);
            dataSet[DicomTags.InstanceAvailability].SetStringValue("ONLINE");

            // character set
            var characterSet = GetPreferredCharacterSet();
            if (!string.IsNullOrEmpty(characterSet))
            {
                dataSet[DicomTags.SpecificCharacterSet].SetStringValue(characterSet);
                dataSet.SpecificCharacterSet = characterSet;
            }

            foreach (DicomTag tag in tagList)
            {
                try
                {
                    switch (tag.TagValue)
                    {
                        case DicomTags.StudyInstanceUid:
                            dataSet[DicomTags.StudyInstanceUid].SetStringValue(row.StudyUid);
                            break;
                        case DicomTags.PatientsName:
                            dataSet[DicomTags.PatientsName].SetStringValue(row.Patient.PatientName);
                            break;
                        case DicomTags.PatientId:
                            dataSet[DicomTags.PatientId].SetStringValue(row.Patient.PatientId);
                            break;
                        case DicomTags.PatientsSex:
                            dataSet[DicomTags.PatientsSex].SetStringValue(row.PatientSex);
                            break;
                        case DicomTags.PatientsAge:
                            dataSet[DicomTags.PatientsAge].SetStringValue(row.PatientAge);
                            break;
                        case DicomTags.PatientsBirthDate:
                            dataSet[DicomTags.PatientsBirthDate].SetStringValue(row.Patient.PatientBirthDate);
                            break;
                        case DicomTags.StudyDate:
                            dataSet[DicomTags.StudyDate].SetStringValue(row.StudyDate);
                            break;
                        case DicomTags.StudyTime:
                            dataSet[DicomTags.StudyTime].SetStringValue(row.StudyTime);
                            break;
                        case DicomTags.AccessionNumber:
                            dataSet[DicomTags.AccessionNumber].SetStringValue(row.AccessionNumber);
                            break;
                        case DicomTags.StudyId:
                            dataSet[DicomTags.StudyId].SetStringValue(row.StudyId);
                            break;
                        case DicomTags.StudyDescription:
                            dataSet[DicomTags.StudyDescription].SetStringValue(row.StudyDescription);
                            break;
                        case DicomTags.ReferringPhysiciansName:
                            dataSet[DicomTags.ReferringPhysiciansName].SetStringValue(row.RefPhysician);
                            break;
                        case DicomTags.NumberOfStudyRelatedSeries:
                            dataSet[DicomTags.NumberOfStudyRelatedSeries].AppendInt32(row.NumberOfRelatedSeries);
                            break;
                        case DicomTags.NumberOfStudyRelatedInstances:
                            dataSet[DicomTags.NumberOfStudyRelatedInstances].AppendInt32(row.NumberOfRelatedImage);
                            break;
                        case DicomTags.ModalitiesInStudy:
                            break;

                        case DicomTags.QueryRetrieveLevel:
                            dataSet[DicomTags.QueryRetrieveLevel].SetStringValue("STUDY");
                            break;

                        // Meta tags that should have not been in the RQ, but we've already set
                        case DicomTags.RetrieveAeTitle:
                        case DicomTags.InstanceAvailability:
                        case DicomTags.SpecificCharacterSet:
                            break;

                        default:
                            if (!tag.IsPrivate)
                                dataSet[tag].SetNullValue();
                            break;
                    }
                }
                catch (Exception e)
                {
                    Platform.Log(LogLevel.Warn, e, "Unexpected error setting tag {0} in C-FIND-RSP",
                                 dataSet[tag].Tag.ToString());
                    if (!tag.IsPrivate)
                        dataSet[tag].SetNullValue();
                }
            }
        }

        private void PopulateSeries(DicomMessageBase request, DicomMessage response,
            IEnumerable<DicomTag> tagList, Series row)
        {
            DicomAttributeCollection dataSet = response.DataSet;

            dataSet[DicomTags.RetrieveAeTitle].SetStringValue(base.Partition.AeTitle);
            dataSet[DicomTags.InstanceAvailability].SetStringValue("ONLINE");

            // character set
            var characterSet = GetPreferredCharacterSet();
            if (!string.IsNullOrEmpty(characterSet))
            {
                dataSet[DicomTags.SpecificCharacterSet].SetStringValue(characterSet);
                dataSet.SpecificCharacterSet = characterSet;
            }

            foreach (DicomTag tag in tagList)
            {
                try
                {
                    switch (tag.TagValue)
                    {
                        case DicomTags.PatientId:
                            dataSet[DicomTags.PatientId].SetStringValue(request.DataSet[DicomTags.PatientId].ToString());
                            break;
                        case DicomTags.StudyInstanceUid:
                            dataSet[DicomTags.StudyInstanceUid].SetStringValue(
                                request.DataSet[DicomTags.StudyInstanceUid].ToString());
                            break;
                        case DicomTags.SeriesInstanceUid:
                            dataSet[DicomTags.SeriesInstanceUid].SetStringValue(row.SeriesUid);
                            break;
                        case DicomTags.Modality:
                            dataSet[DicomTags.Modality].SetStringValue(row.Modality);
                            break;
                        case DicomTags.SeriesNumber:
                            dataSet[DicomTags.SeriesNumber].SetStringValue(row.SeriesNumber);
                            break;
                        case DicomTags.SeriesDescription:
                            dataSet[DicomTags.SeriesDescription].SetStringValue(row.SeriesDescription);
                            break;
                        case DicomTags.PerformedProcedureStepStartDate:
                            dataSet[DicomTags.PerformedProcedureStepStartDate].SetStringValue(
                                row.PerformedProcedureStepStartDate);
                            break;
                        case DicomTags.PerformedProcedureStepStartTime:
                            dataSet[DicomTags.PerformedProcedureStepStartTime].SetStringValue(
                                row.PerformedProcedureStepStartTime);
                            break;
                        case DicomTags.NumberOfSeriesRelatedInstances:
                            dataSet[DicomTags.NumberOfSeriesRelatedInstances].AppendInt32(row.NumberOfRelatedImage);
                            break;
                        case DicomTags.RequestAttributesSequence:
                            //LoadRequestAttributes(read, response, row);
                            break;
                        case DicomTags.QueryRetrieveLevel:
                            dataSet[DicomTags.QueryRetrieveLevel].SetStringValue("SERIES");
                            break;
                        // Meta tags that should have not been in the RQ, but we've already set
                        case DicomTags.RetrieveAeTitle:
                        case DicomTags.InstanceAvailability:
                        case DicomTags.SpecificCharacterSet:
                            break;
                        default:
                            if (!tag.IsPrivate)
                                dataSet[tag].SetNullValue();

                            break;
                    }
                }
                catch (Exception e)
                {
                    Platform.Log(LogLevel.Warn, e, "Unexpected error setting tag {0} in C-FIND-RSP",
                                 dataSet[tag].Tag.ToString());
                    if (!tag.IsPrivate)
                        dataSet[tag].SetNullValue();
                }
            }
        }

        private void PopuldateInstance(DicomMessageBase request, DicomMessage response, IEnumerable<DicomTag> tagList,
            Instance row)
        {
            DicomAttributeCollection dataSet = response.DataSet;

            dataSet[DicomTags.RetrieveAeTitle].SetStringValue(base.Partition.AeTitle);
            dataSet[DicomTags.InstanceAvailability].SetStringValue("ONLINE");

            // character set
            var characterSet = GetPreferredCharacterSet();
            if (!string.IsNullOrEmpty(characterSet))
            {
                dataSet[DicomTags.SpecificCharacterSet].SetStringValue(characterSet);
                dataSet.SpecificCharacterSet = characterSet;
            }

            foreach (DicomTag tag in tagList)
            {
                try
                {
                    switch (tag.TagValue)
                    {
                        case DicomTags.PatientId:
                            dataSet[DicomTags.PatientId].SetStringValue(request.DataSet[DicomTags.PatientId].ToString());
                            break;
                        case DicomTags.StudyInstanceUid:
                            dataSet[DicomTags.StudyInstanceUid].SetStringValue(
                                request.DataSet[DicomTags.StudyInstanceUid].ToString());
                            break;
                        case DicomTags.SeriesInstanceUid:
                            dataSet[DicomTags.SeriesInstanceUid].SetStringValue(
                                request.DataSet[DicomTags.SeriesInstanceUid].ToString());
                            break;
                        case DicomTags.QueryRetrieveLevel:
                            dataSet[DicomTags.QueryRetrieveLevel].SetStringValue("IMAGE");
                            break;
                        case DicomTags.SopInstanceUid:
                            dataSet[DicomTags.SopInstanceUid].SetStringValue(row.SopInstanceUid);
                            break;
                        case DicomTags.SopClassUid:
                            dataSet[DicomTags.SopClassUid].SetStringValue(row.SopClassUid);
                            break;
                        case DicomTags.InstanceNumber:
                            dataSet[DicomTags.InstanceNumber].SetStringValue(row.InstanceNumber);
                            break;
                        // Meta tags that should have not been in the RQ, but we've already set
                        case DicomTags.RetrieveAeTitle:
                        case DicomTags.InstanceAvailability:
                        case DicomTags.SpecificCharacterSet:
                            break;
                        default:
                            //if (sourceDataSet.Contains(tag))
                            //    dataSet[tag] = sourceDataSet[tag].Copy();
                            //else if (!tag.IsPrivate)
                            //    dataSet[tag].SetNullValue();
                            break;
                    }
                }
                catch (Exception e)
                {
                    Platform.Log(LogLevel.Warn, e, "Unexpected error setting tag {0} in C-FIND-RSP",
                                 dataSet[tag].Tag.ToString());
                    if (!tag.IsPrivate)
                        dataSet[tag].SetNullValue();
                }
            }
        }

        /// <summary>
        /// Get the preferred character set for the C-FIND-RSP.
        /// </summary>
        /// <returns></returns>
        private static string GetPreferredCharacterSet()
        {
            // TODO: In the future, should be device-dependent

            if (DicomSetting.Default.CFINDRspAlwaysInUnicode)
                return "ISO_IR 192";
            else
                return null;
        }

        private void OnReceivePatientQuery(DicomServer server, byte presentationId, DicomMessage message)
        {
            var tagList = new List<DicomTag>();

            using (var pacsContext = new PacsContext())
            {
                var adapter = (IObjectContextAdapter)pacsContext;
                var query = new ObjectQuery<Patient>("Patients", adapter.ObjectContext);

                DicomAttributeCollection data = message.DataSet;

                foreach (DicomAttribute attribute in data)
                {
                    tagList.Add(attribute.Tag);
                    if (!attribute.IsNull)
                    {
                        switch (attribute.Tag.TagValue)
                        {
                            case DicomTags.PatientsName:
                                {
                                    var cond = QueryHelper.SetStringCondition("PatientName",
                                                                   data[DicomTags.PatientsName].GetString(0, string.Empty));
                                    query = query.Where(cond);
                                    break;
                                }

                            case DicomTags.PatientId:
                                {
                                    var cond = QueryHelper.SetStringCondition("PatientId",
                                                               data[DicomTags.PatientId].GetString(0, string.Empty));
                                    query = query.Where(cond);
                                    break;
                                }

                            case DicomTags.IssuerOfPatientId:
                                {
                                    var cond = QueryHelper.SetStringCondition("Issuer",
                                                                              data[DicomTags.IssuerOfPatientId]
                                                                                  .GetString(0, string.Empty));
                                }
                                break;
                            case DicomTags.PatientsSex:
                                break;
                            case DicomTags.PatientsBirthDate:
                                {
                                    var cond = QueryHelper.SetStringArrayCondition("PatientBirthDate",
                                                                                   (string[])
                                                                                   data[DicomTags.PatientsBirthDate]
                                                                                       .Values);
                                    query = query.Where(cond);
                                }
                                break;
                            default:
                                break;
                        }
                    }
                }

                int resultCount = 0;
                try
                {
                    foreach (Patient patient in query)
                    {
                        if (_cancelReceived)
                        {
                            throw new DicomException("DICOM C-Cancel Received");
                        }

                        resultCount++;
                        if (DicomSetting.Default.MaxQueryResponses != -1 &&
                            DicomSetting.Default.MaxQueryResponses < resultCount)
                        {
                            SendBufferedResponses(server, presentationId, message);
                            throw new DicomException("Maximum Configured Query Responses Exceeded: " + resultCount);
                        }

                        var response = new DicomMessage();
                        PopulatePatient(response, tagList, patient);
                        _responseQueue.Enqueue(response);

                        if (_responseQueue.Count >= DicomSetting.Default.BufferedQueryResponses)
                        {
                            SendBufferedResponses(server, presentationId, message);
                        }
                    }

                    SendBufferedResponses(server, presentationId, message);
                }
                catch (Exception e)
                {
                    if (_cancelReceived)
                    {
                        var errorResponse = new DicomMessage();
                        server.SendCFindResponse(presentationId, message.MessageId, errorResponse,
                                                 DicomStatuses.Cancel);
                    }
                }
            }

            var finalResponse = new DicomMessage();
            server.SendCFindResponse(presentationId, message.MessageId, finalResponse, DicomStatuses.Success);
        }

        private void OnReceiveStudyLevelQuery(DicomServer server, byte presentationId, DicomMessage message)
        {
            var tagList = new List<DicomTag>();

            using (var pacsContext = new PacsContext())
            {
                var adapter = (IObjectContextAdapter)pacsContext;
                var query = new ObjectQuery<Study>("Studies", adapter.ObjectContext);

                DicomAttributeCollection data = message.DataSet;
                foreach (DicomAttribute attrib in message.DataSet)
                {
                    tagList.Add(attrib.Tag);

                    if (!attrib.IsNull)
                    {
                        switch (attrib.Tag.TagValue)
                        {
                            case DicomTags.StudyInstanceUid:
                                {
                                    var cond = QueryHelper.SetStringArrayCondition("StudyInstanceUid",
                                                                                   (string[])data[DicomTags.StudyInstanceUid].Values);
                                    if (!string.IsNullOrEmpty(cond))
                                    {
                                        query = query.Where(cond);
                                    }    
                                }
                                
                                break;
                            case DicomTags.PatientsName:
                                {
                                    var cond = QueryHelper.SetStringCondition("PatientName",
                                                                              data[DicomTags.PatientsName].GetString(0,
                                                                                                                     string
                                                                                                                         .Empty));
                                    if (!string.IsNullOrEmpty(cond))
                                    {
                                        query = query.Where(cond);
                                    }
                                    
                                    break;
                                }
                            case DicomTags.PatientId:
                                {
                                    var cond = QueryHelper.SetStringCondition("PatientId",
                                                                              data[DicomTags.PatientId].GetString(0,
                                                                                                                  string
                                                                                                                      .Empty));
                                    break;
                                }
                            case DicomTags.PatientsBirthDate:
                                {
                                    var cond = QueryHelper.SetRangeCondition("PatientBirthDate",
                                                                             data[DicomTags.PatientsBirthDate].GetString
                                                                                 (0, string.Empty));
                                    query = query.Where(cond);
                                    break;
                                }

                            case DicomTags.PatientsSex:
                                {
                                    var cond = QueryHelper.SetStringCondition("PatientSex",
                                                                              data[DicomTags.PatientsSex].GetString(0,
                                                                                                                    string
                                                                                                                        .Empty));
                                    query = query.Where(cond);
                                }
                                break;
                            case DicomTags.StudyDate:
                                {
                                    var cond = QueryHelper.SetRangeCondition("StudyDate",
                                                                              data[DicomTags.StudyDate].GetString(0,
                                                                                                                  string
                                                                                                                      .Empty));
                                    query = query.Where(cond);
                                }
                                break;
                            case DicomTags.StudyTime:
                                {
                                    var cond = QueryHelper.SetRangeCondition("StudyTime",
                                                                              data[DicomTags.StudyTime].GetString(0,
                                                                                                                  string
                                                                                                                      .Empty));
                                    query = query.Where(cond);
                                }
                                break;
                            case DicomTags.AccessionNumber:
                                {
                                    var cond = QueryHelper.SetStringCondition("AccessionNumber",
                                                                              data[DicomTags.AccessionNumber].GetString(
                                                                                  0, string.Empty));
                                    query = query.Where(cond);
                                }
                                break;
                            case DicomTags.StudyId:
                                {
                                    var cond = QueryHelper.SetStringCondition("StudyId",
                                        data[DicomTags.StudyId].GetString(0, string.Empty));

                                    query = query.Where(cond);
                                }
                                break;
                            case DicomTags.StudyDescription:
                                {
                                    var cond = QueryHelper.SetStringCondition("StudyDescription",
                                        data[DicomTags.StudyDescription].GetString(0, string.Empty));
                                    query = query.Where(cond);
                                }
                                break;
                            case DicomTags.ReferringPhysiciansName:
                                {
                                    var cond = QueryHelper.SetStringCondition("RefPhysician",
                                                                              data[DicomTags.ReferringPhysiciansName]
                                                                                  .GetString(0, string.Empty));
                                    query = query.Where(cond);
                                }
                                break;
                            case DicomTags.ModalitiesInStudy:
                                break;
                            default:
                                break;
                        }
                    }
                }
                int resultCount = 0;
                try
                {
                    foreach (Study study in query)
                    {
                        if (_cancelReceived)
                        {
                            throw new DicomException("DICOM C-Cancel Received");
                        }

                        resultCount++;
                        if (DicomSetting.Default.MaxQueryResponses != -1 &&
                            DicomSetting.Default.MaxQueryResponses < resultCount)
                        {
                            SendBufferedResponses(server, presentationId, message);
                            throw new DicomException("Maximum Configured Query Responses Exceeded: " + resultCount);
                        }

                        var response = new DicomMessage();
                        PopulateStudy(response, tagList, study);
                        _responseQueue.Enqueue(response);

                        if (_responseQueue.Count >= DicomSetting.Default.BufferedQueryResponses)
                        {
                            SendBufferedResponses(server, presentationId, message);
                        }
                    }

                    SendBufferedResponses(server, presentationId, message);
                }
                catch (Exception e)
                {
                    if (_cancelReceived)
                    {
                        var errorResponse = new DicomMessage();
                        server.SendCFindResponse(presentationId, message.MessageId, errorResponse,
                                                 DicomStatuses.Cancel);
                    }

                    return;
                }
            }

            var finalResponse = new DicomMessage();
            server.SendCFindResponse(presentationId, message.MessageId, finalResponse, DicomStatuses.Success);
        }

        private void OnReceiveSeriesLevelQuery(DicomServer server, byte presentationId, DicomMessage message)
        {
            var tagList = new List<DicomTag>();

            using (var pacsContext = new PacsContext())
            {
                var adapter = (IObjectContextAdapter)pacsContext;
                var query = new ObjectQuery<Series>("Series", adapter.ObjectContext);

                DicomAttributeCollection data = message.DataSet;
                foreach (DicomAttribute attrib in data)
                {
                    tagList.Add(attrib.Tag);
                    if (!attrib.IsNull)
                    {
                        switch (attrib.Tag.TagValue)
                        {
                            case DicomTags.StudyInstanceUid:
                                {
                                    var uids = data[DicomTags.StudyInstanceUid].GetString(0, string.Empty);
                                    IQueryable<int> studyQuery = from s in pacsContext.Studies
                                                                 where uids.Contains(s.StudyUid)
                                                                 select s.Id;
                                    var result = studyQuery.ToArray();
                                    if (result.Length == 0)
                                    {
                                        server.SendCFindResponse(presentationId, message.MessageId, new DicomMessage(),
                                                             DicomStatuses.Success);
                                        return;
                                    }

                                    query = query.Where(QueryHelper.SetIntArrayCondition("StudyForeignKey", result));
                                }
                                break;
                            case DicomTags.SeriesInstanceUid:
                                {
                                    var cond = QueryHelper.SetStringArrayCondition("SeriesUid",
                                                                                   (string[])
                                                                                   data[DicomTags.SeriesInstanceUid]
                                                                                       .Values);
                                    query = query.Where(cond);
                                }
                                break;
                            case DicomTags.Modality:
                                {
                                    var cond = QueryHelper.SetStringCondition("Modality",
                                                                              data[DicomTags.Modality].GetString(0,
                                                                                                                 string
                                                                                                                     .Empty));
                                    query = query.Where(cond);
                                }
                                break;
                            case DicomTags.SeriesNumber:
                                {
                                    var cond = QueryHelper.SetStringCondition("SeriesNumber",
                                                                              data[DicomTags.SeriesNumber].GetString(0,
                                                                                                                     string
                                                                                                                         .Empty));
                                    query = query.Where(cond);
                                }
                                break;
                            case DicomTags.SeriesDescription:
                                {
                                    var cond = QueryHelper.SetStringCondition("SeriesDescription",
                                                                              data[DicomTags.StudyDescription].GetString
                                                                                  (0, string.Empty));
                                    query = query.Where(cond);
                                }
                                break;
                            case DicomTags.PerformedProcedureStepStartDate:
                                {
                                    var cond = QueryHelper.SetRangeCondition("PerformedProcedureStepStartDate",
                                                                             data[
                                                                                 DicomTags
                                                                                     .PerformedProcedureStepStartDate]
                                                                                 .GetString(0, string.Empty));
                                    query = query.Where(cond);
                                }
                                break;
                            case DicomTags.PerformedProcedureStepStartTime:
                                {
                                    var cond = QueryHelper.SetRangeCondition("PerformedProcedureStepStartTime",
                                                                             data[
                                                                                 DicomTags
                                                                                     .PerformedProcedureStepStartTime]
                                                                                 .GetString(0, string.Empty));
                                    query = query.Where(cond);
                                }
                                break;
                            case DicomTags.RequestAttributesSequence: // todo
                                break;
                            default:
                                break;
                        }
                    }
                }

                int resultCount = 0;
                try
                {
                    foreach (Series series in query)
                    {
                        if (_cancelReceived)
                        {
                            throw new DicomException("DICOM C-Cancel Received");
                        }

                        resultCount++;
                        if (DicomSetting.Default.MaxQueryResponses != -1 &&
                            DicomSetting.Default.MaxQueryResponses < resultCount)
                        {
                            SendBufferedResponses(server, presentationId, message);
                            throw new DicomException("Maximum Configured Query Responses Exceeded: " + resultCount);
                        }

                        var response = new DicomMessage();
                        PopulateSeries(message, response, tagList, series);
                        _responseQueue.Enqueue(response);

                        if (_responseQueue.Count >= DicomSetting.Default.BufferedQueryResponses)
                        {
                            SendBufferedResponses(server, presentationId, message);
                        }
                    }

                    SendBufferedResponses(server, presentationId, message);
                }
                catch (Exception e)
                {
                    if (_cancelReceived)
                    {
                        var errorResponse = new DicomMessage();
                        server.SendCFindResponse(presentationId, message.MessageId, errorResponse,
                                                 DicomStatuses.Cancel);
                    }
                }
            }

            var finalResponse = new DicomMessage();
            server.SendCFindResponse(presentationId, message.MessageId, finalResponse, DicomStatuses.Success);
        }

        // what is the scenario about image level query 
        // TODO add support in Image Level Query 
        private void OnReceiveImageLevelQuery(DicomServer server, byte presentationId, DicomMessage message)
        {
            var tagList = new List<DicomTag>();

            using (var pacsContext = new PacsContext())
            {
                var adapter = (IObjectContextAdapter)pacsContext;
                var query = new ObjectQuery<Instance>("Instances", adapter.ObjectContext);

                DicomAttributeCollection data = message.DataSet;
                string studyInstanceUid = data[DicomTags.StudyInstanceUid].GetString(0, String.Empty);
                string seriesInstanceUid = data[DicomTags.SeriesInstanceUid].GetString(0, String.Empty);

                foreach (DicomAttribute attrib in data)
                {
                    if (attrib.Tag.TagValue.Equals(DicomTags.SpecificCharacterSet)
                        || attrib.Tag.TagValue.Equals(DicomTags.QueryRetrieveLevel))
                        continue;

                    tagList.Add(attrib.Tag);
                    if (!attrib.IsNull)
                    {
                        switch (attrib.Tag.TagValue)
                        {

                        }
                    }
                }

                int resultCount = 0;
                try
                {
                    foreach (Instance instance in query)
                    {
                        if (_cancelReceived)
                        {
                            throw new DicomException("DICOM C-Cancel Received");
                        }

                        resultCount++;
                        if (DicomSetting.Default.MaxQueryResponses != -1 &&
                            DicomSetting.Default.MaxQueryResponses < resultCount)
                        {
                            SendBufferedResponses(server, presentationId, message);
                            throw new DicomException("Maximum Configured Query Responses Exceeded: " + resultCount);
                        }

                        var response = new DicomMessage();
                        PopuldateInstance(message, response, tagList, instance);
                        _responseQueue.Enqueue(response);

                        if (_responseQueue.Count >= DicomSetting.Default.BufferedQueryResponses)
                        {
                            SendBufferedResponses(server, presentationId, message);
                        }
                    }

                    SendBufferedResponses(server, presentationId, message);
                }
                catch (Exception e)
                {
                    if (_cancelReceived)
                    {
                        var errorResponse = new DicomMessage();
                        server.SendCFindResponse(presentationId, message.MessageId, errorResponse,
                                                 DicomStatuses.Cancel);
                    }
                }
            }

            var finalResponse = new DicomMessage();
            server.SendCFindResponse(presentationId, message.MessageId, finalResponse, DicomStatuses.Success);
        }

        private void SendBufferedResponses(DicomServer server, byte presentationId, DicomMessage requestMessage)
        {
            while (_responseQueue.Count > 0)
            {
                DicomMessage response = _responseQueue.Dequeue();

                server.SendCFindResponse(presentationId, requestMessage.MessageId, response, DicomStatuses.Pending);

                if (_cancelReceived)
                {
                    throw new DicomException("DICOM C-Cancel Received");
                }

                if (!server.NetworkActive)
                    throw new DicomException("Association is no longer valid.");
            }
        }

        #endregion

        public override bool OnReceiveRequest(DicomServer server, ServerAssociationParameters association,
            byte presentationId, DicomMessage message)
        {
            string level = message.DataSet[DicomTags.QueryRetrieveLevel].GetString(0, string.Empty);

            if (message.CommandField == DicomCommandField.CCancelRequest)
            {
                Platform.Log(LogLevel.Info, "Received C-FIND-CANCEL-RQ message.");
                _cancelReceived = true;
                return true;
            }

            if (message.AffectedSopClassUid.Equals(SopClass.StudyRootQueryRetrieveInformationModelFindUid))
            {
                switch (level)
                {
                    case "STUDY":
                        // We use the ThreadPool to process the thread requests. This is so that we return back
                        // to the main message loop, and continue to look for cancel request messages coming
                        // in.  There's a small chance this may cause delays in responding to query requests if
                        // the .NET Thread pool fills up.
                        ThreadPool.QueueUserWorkItem(delegate
                        {
                            try
                            {
                                OnReceiveStudyLevelQuery(server, presentationId, message);
                            }
                            catch (Exception x)
                            {
                                Platform.Log(LogLevel.Error, x, "Unexpected exception in OnReceiveStudyLevelQuery.");
                            }
                        });
                        return true;

                    case "SERIES":
                        ThreadPool.QueueUserWorkItem(delegate
                        {
                            try
                            {
                                OnReceiveSeriesLevelQuery(server, presentationId, message);
                            }
                            catch (Exception x)
                            {
                                Platform.Log(LogLevel.Error, x,
                                             "Unexpected exception in OnReceiveSeriesLevelQuery.");
                            }
                        });
                        return true;

                    case "IMAGE":
                        ThreadPool.QueueUserWorkItem(delegate
                        {
                            try
                            {
                                OnReceiveImageLevelQuery(server, presentationId, message);
                            }
                            catch (Exception x)
                            {
                                Platform.Log(LogLevel.Error, x,
                                             "Unexpected exception in OnReceiveImageLevelQuery.");
                            }
                        });
                        return true;

                    default:
                        {
                            Platform.Log(LogLevel.Error, "Unexpected Study Root Query/Retrieve level: {0}", level);

                            server.SendCFindResponse(presentationId, message.MessageId, new DicomMessage(),
                                                     DicomStatuses.QueryRetrieveIdentifierDoesNotMatchSOPClass);
                            return true;
                        }
                }
            }

            if (message.AffectedSopClassUid.Equals(SopClass.PatientRootQueryRetrieveInformationModelFindUid))
            {
                switch (level)
                {
                    case "PATIENT":
                        ThreadPool.QueueUserWorkItem(delegate
                        {
                            try
                            {
                                OnReceivePatientQuery(server, presentationId, message);
                            }
                            catch (Exception x)
                            {
                                Platform.Log(LogLevel.Error, x,
                                             "Unexpected exception in OnReceivePatientQuery.");
                            }
                        });

                        return true;
                    case "STUDY":
                        ThreadPool.QueueUserWorkItem(delegate
                                                    {
                                                        try
                                                        {
                                                            OnReceiveStudyLevelQuery(server, presentationId, message);
                                                        }
                                                        catch (Exception x)
                                                        {
                                                            Platform.Log(LogLevel.Error, x,
                                                                         "Unexpected exception in OnReceiveStudyLevelQuery.");
                                                        }
                                                    });
                        return true;
                    case "SERIES":
                        ThreadPool.QueueUserWorkItem(delegate
                                                    {
                                                        try
                                                        {
                                                            OnReceiveSeriesLevelQuery(server, presentationId, message);
                                                        }
                                                        catch (Exception x)
                                                        {
                                                            Platform.Log(LogLevel.Error, x,
                                                                         "Unexpected exception in OnReceiveSeriesLevelQuery.");
                                                        }
                                                    });
                        return true;
                    case "IMAGE":
                        ThreadPool.QueueUserWorkItem(delegate
                                                     {
                                                         try
                                                         {
                                                             OnReceiveImageLevelQuery(server, presentationId, message);
                                                         }
                                                         catch (Exception x)
                                                         {
                                                             Platform.Log(LogLevel.Error, x,
                                                                          "Unexpected exception in OnReceiveImageLevelQuery.");
                                                         }
                                                     });
                        return true;

                    default:
                        {
                            Platform.Log(LogLevel.Error, "Unexpected Patient Root Query/Retrieve level: {0}", level);

                            server.SendCFindResponse(presentationId, message.MessageId, new DicomMessage(),
                                                     DicomStatuses.QueryRetrieveIdentifierDoesNotMatchSOPClass);
                            return true;
                        }
                }
            }

            // no supported message type, send a failure status 
            server.SendCFindResponse(presentationId, message.MessageId, new DicomMessage(),
                DicomStatuses.QueryRetrieveIdentifierDoesNotMatchSOPClass);

            return true;

        }

        protected override DicomPresContextResult OnVerifyAssociation(AssociationParameters association, byte pcid)
        {
            if (!Device.AllowQuery)
            {
                return DicomPresContextResult.RejectUser;
            }

            return DicomPresContextResult.Accept;
        }

        public override IList<SupportedSop> GetSupportedSopClasses()
        {
            return _list;
        }
    }
}