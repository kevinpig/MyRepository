#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

﻿using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using uWS.Common;
using uWS.Dicom;
using uWS.Dicom.Network;
using uWS.Dicom.Network.Scp;
using uWS.Dicom.Network.Scu;
using uWs.PACS.Model;

namespace uWS.Pacs.DicomService
{
    [Export(typeof(IDicomScp<DicomScpContext>))]
    public class CMoveScp : BaseScp
    {
        #region Private Member

        private readonly List<SupportedSop> _list = new List<SupportedSop>();

        private PacsStorageScu _theScu;

        #endregion

        #region Constructor

        public CMoveScp()
        {
            var sop = new SupportedSop()
                {
                    SopClass = SopClass.PatientRootQueryRetrieveInformationModelMove
                };
            sop.SyntaxList.Add(TransferSyntax.ExplicitVrLittleEndian);
            sop.SyntaxList.Add(TransferSyntax.ImplicitVrLittleEndian);
            _list.Add(sop);

            sop = new SupportedSop()
                {
                    SopClass = SopClass.StudyRootQueryRetrieveInformationModelMove
                };
            sop.SyntaxList.Add(TransferSyntax.ExplicitVrLittleEndian);
            sop.SyntaxList.Add(TransferSyntax.ImplicitVrLittleEndian);
            _list.Add(sop);
        }

        #endregion

        #region IDicomScp Members

        public override bool OnReceiveRequest(DicomServer server, ServerAssociationParameters association,
                                              byte presentationID, DicomMessage message)
        {
            bool finalResponseSent = false;
            string errorComment;

            try
            {
                // check for a Cancel Message, and cance the scu 
                if (message.CommandField == DicomCommandField.CCancelRequest)
                {
                    if (_theScu != null)
                    {
                        _theScu.Cancel();
                    }

                    return true;
                }

                string level = message.DataSet[DicomTags.QueryRetrieveLevel].GetString(0, string.Empty);

                string remoteAe = message.MoveDestination.Trim();

                // load remote device for move information 
                using (var ctx = new PacsContext())
                {
                    var device = (from d in ctx.Devices
                                  where d.ServerPartitionPK == Partition.Id && d.AeTitle == remoteAe
                                  select d).FirstOrDefault();

                    if (device == null)
                    {
                        errorComment = string.Format(
                            "Unknown move destination \"{0}\", failing C-MOVE-RQ from {1} to {2}",
                            remoteAe, association.CallingAE, association.CalledAE);
                        Platform.Log(LogLevel.Error, errorComment);
                        server.SendCMoveResponse(presentationID, message.MessageId, new DicomMessage(),
                                                 DicomStatuses.QueryRetrieveMoveDestinationUnknown, errorComment);
                        finalResponseSent = true;
                        return true;
                    }

                    // If the remote node is a DHCP node, use its IP address from the connection information, else
                    // use what is configured.  Always use the configured port. 
                    if (device.Dhcp)
                    {
                        device.Hostname = association.RemoteEndPoint.Address.ToString();
                    }

                    // now setup the storage scu component 
                    _theScu = new PacsStorageScu(Partition, device, association.CallingAE, message.MessageId);

                    bool bOnline;

                    if (level.Equals("PATIENT"))
                    {
                        bOnline = GetSopListForInstance(ctx, message, out errorComment);
                    }
                    else if (level.Equals("STUDY"))
                    {
                        bOnline = GetSopListForStudy(ctx, message, out errorComment);
                    }
                    else if (level.Equals("SERIES"))
                    {
                        bOnline = GetSopListForSeries(ctx, message, out errorComment);
                    }
                    else if (level.Equals("IMAGE"))
                    {
                        bOnline = GetSopListForInstance(ctx, message, out errorComment);
                    }
                    else
                    {
                        errorComment = string.Format("Unexpected Study Root Move Query/Retrieve level: {0}", level);
                        Platform.Log(LogLevel.Error, errorComment);

                        server.SendCMoveResponse(presentationID, message.MessageId, new DicomMessage(),
                                                 DicomStatuses.QueryRetrieveIdentifierDoesNotMatchSOPClass,
                                                 errorComment);
                        finalResponseSent = true;
                        return true;
                    }

                    // Could not find an online/readable location for the requested objects to move.
                    // Note that if the C-MOVE-RQ included a list of study instance uids, and some 
                    // were online and some offline, we don't fail now (ie, the check on the Count)
                    if (!bOnline || _theScu.StorageInstanceList.Count == 0)
                    {
                        Platform.Log(LogLevel.Error, "Unable to find online storage location for C-MOVE-RQ: {0}",
                                     errorComment);

                        server.SendCMoveResponse(presentationID, message.MessageId, new DicomMessage(),
                                                 DicomStatuses.QueryRetrieveUnableToPerformSuboperations,
                                                 string.IsNullOrEmpty(errorComment) ? string.Empty : errorComment);
                        finalResponseSent = true;
                        _theScu.Dispose();
                        _theScu = null;
                        return true;
                    }

                    _theScu.ImageStoreCompleted += delegate(Object sender, StorageInstance instance)
                        {
                            var scu = (StorageScu) sender;
                            var msg = new DicomMessage();
                            DicomStatus status;

                            if (instance.SendStatus.Status == DicomState.Failure)
                            {
                                errorComment =
                                    string.IsNullOrEmpty(instance.ExtendedFailureDescription)
                                        ? instance.SendStatus.ToString()
                                        : instance.ExtendedFailureDescription;
                            }

                            if (scu.RemainingSubOperations == 0)
                            {
                                foreach (StorageInstance sop in _theScu.StorageInstanceList)
                                {
                                    if ((sop.SendStatus.Status != DicomState.Success)
                                        && (sop.SendStatus.Status != DicomState.Warning))
                                        msg.DataSet[DicomTags.FailedSopInstanceUidList].AppendString(sop.SopInstanceUid);
                                }
                                if (scu.Status == ScuOperationStatus.Canceled)
                                    status = DicomStatuses.Cancel;
                                else if (scu.Status == ScuOperationStatus.ConnectFailed)
                                    status = DicomStatuses.QueryRetrieveMoveDestinationUnknown;
                                else if (scu.FailureSubOperations > 0)
                                    status = DicomStatuses.QueryRetrieveSubOpsOneOrMoreFailures;
                                else if (!bOnline)
                                    status = DicomStatuses.QueryRetrieveUnableToPerformSuboperations;
                                else
                                    status = DicomStatuses.Success;
                            }
                            else
                            {
                                status = DicomStatuses.Pending;

                                if ((scu.RemainingSubOperations%5) != 0)
                                    return;
                                // Only send a RSP every 5 to reduce network load
                            }
                            server.SendCMoveResponse(presentationID, message.MessageId,
                                                     msg, status,
                                                     (ushort) scu.SuccessSubOperations,
                                                     (ushort) scu.RemainingSubOperations,
                                                     (ushort) scu.FailureSubOperations,
                                                     (ushort) scu.WarningSubOperations,
                                                     status == DicomStatuses.QueryRetrieveSubOpsOneOrMoreFailures
                                                         ? errorComment
                                                         : string.Empty);


                            if (scu.RemainingSubOperations == 0)
                                finalResponseSent = true;
                        };

                    _theScu.BeginSend(
                        delegate(IAsyncResult ar)
                            {
                                if (_theScu != null)
                                {
                                    if (!finalResponseSent)
                                    {
                                        var msg = new DicomMessage();
                                        server.SendCMoveResponse(presentationID, message.MessageId,
                                                                 msg, DicomStatuses.QueryRetrieveSubOpsOneOrMoreFailures,
                                                                 (ushort) _theScu.SuccessSubOperations,
                                                                 0,
                                                                 (ushort)
                                                                 (_theScu.FailureSubOperations +
                                                                  _theScu.RemainingSubOperations),
                                                                 (ushort) _theScu.WarningSubOperations, errorComment);
                                        finalResponseSent = true;
                                    }

                                    _theScu.EndSend(ar);
                                    _theScu.Dispose();
                                    _theScu = null;
                                }
                            },
                        _theScu);

                    return true;
                }
            }
            catch (Exception e)
            {
                Platform.Log(LogLevel.Error, e, "Unexpected exception when processing C-MOVE-RQ");
                if (finalResponseSent == false)
                {
                    try
                    {
                        server.SendCMoveResponse(presentationID, message.MessageId, new DicomMessage(),
                                                 DicomStatuses.QueryRetrieveUnableToProcess, e.Message);
                        finalResponseSent = true;
                    }
                    catch (Exception x)
                    {
                        Platform.Log(LogLevel.Error, x,
                                     "Unable to send final C-MOVE-RSP message on association from {0} to {1}",
                                     association.CallingAE, association.CalledAE);
                        server.Abort();
                    }
                }
            }

            return false;
        }

        public override IList<SupportedSop> GetSupportedSopClasses()
        {
            return _list;
        }

        #endregion

        protected override DicomPresContextResult OnVerifyAssociation(AssociationParameters association, byte pcid)
        {
            if (Device == null)
                return DicomPresContextResult.Accept;

            if (!Device.AllowRetrieve)
            {
                return DicomPresContextResult.RejectUser;
            }

            return DicomPresContextResult.Accept;
        }

        #region Private Member 

        private bool GetSopListForPatient(PacsContext ctx, DicomMessage message, out string errorComment)
        {
            errorComment = string.Empty;

            var patientId = message.DataSet[DicomTags.PatientId].GetString(0, string.Empty);
            int patientPK = ctx.Patients.Where(p => p.PatientId == patientId).Select(p => p.Id).FirstOrDefault();

            var studyList = (ctx.Studies.Where(s => s.PatientForeignKey == patientPK)).ToList();

            foreach (var study in studyList)
            {
                var seriesPkList = (from s in ctx.Series where study.Id == s.StudyForeignKey select s.Id).ToList();

                var sopList = (from s in ctx.Instances where seriesPkList.Contains(s.SeriesForeignKey) select s.Id).ToList();

                var fileList = (from f in ctx.Files where sopList.Contains(f.InstanceFk) select f).ToList();

                foreach (var file in fileList)
                {
                    _theScu.AddStorageInstance(new StorageInstance(file.FilePath));
                }
            }

            return true;
        }

        private bool GetSopListForStudy(PacsContext ctx, DicomMessage message, out string errorComment)
        {
            errorComment = string.Empty;

            var studyUidList = (string[])message.DataSet[DicomTags.StudyInstanceUid].Values;

            var studyList = (from s in ctx.Studies where studyUidList.Contains(s.StudyUid) select s).ToList();

            foreach (var study in studyList)
            {
                var seriesPkList = (from s in ctx.Series where study.Id == s.StudyForeignKey select s.Id).ToList();

                var sopList = (from s in ctx.Instances where seriesPkList.Contains(s.SeriesForeignKey) select s.Id).ToList();

                var fileList = (from f in ctx.Files where sopList.Contains(f.InstanceFk) select f).ToList();

                foreach (var file in fileList)
                {
                    _theScu.AddStorageInstance(new StorageInstance(file.FilePath));
                }
            }

            return true;
        }

        private bool GetSopListForSeries(PacsContext ctx, DicomMessage message, out string errorComment)
        {
            errorComment = string.Empty;

            var studyInstanctUid = message.DataSet[DicomTags.StudyInstanceUid].GetString(0, string.Empty);
            var seriesUidList = (string[]) message.DataSet[DicomTags.SeriesInstanceUid].Values;

            int studyPk = ctx.Studies.Where(s => s.StudyUid == studyInstanctUid).Select(s => s.Id).FirstOrDefault();

            var seriesPkList = (from s in ctx.Series where seriesUidList.Contains(s.SeriesUid) select s.Id).ToList();

            var sopList = (from s in ctx.Instances where seriesPkList.Contains(s.SeriesForeignKey) select s.Id).ToList();

            var fileList = (from f in ctx.Files where sopList.Contains(f.InstanceFk) select f).ToList();

            foreach (var file in fileList)
            {
                _theScu.AddStorageInstance(new StorageInstance(file.FilePath));
            }

            return true;
        }

        private bool GetSopListForInstance(PacsContext ctx, DicomMessage message, out string errorComment)
        {
            errorComment = string.Empty;

            string studyInstanceUid = message.DataSet[DicomTags.StudyInstanceUid].GetString(0, "");
            string seriesInstanceUid = message.DataSet[DicomTags.SeriesInstanceUid].GetString(0, "");
            var sopInstanceUidArray = (string[])message.DataSet[DicomTags.SopInstanceUid].Values;

            var sopPkArray =
                (from s in ctx.Instances where sopInstanceUidArray.Contains(s.SopInstanceUid) select s.Id).ToList();

            var fileList =
                (from f in ctx.Files where sopPkArray.Contains(f.InstanceFk) select f).ToList();

            foreach (var file in fileList)
            {
                _theScu.AddStorageInstance(new StorageInstance(file.FilePath));
            }

            return true;
        }

        #endregion
    }
}