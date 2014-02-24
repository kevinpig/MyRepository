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
                }
            }
            catch (Exception)
            {

            }

            return true;
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
                
            }


            return false;
        }

        private bool GetSopListForStudy(PacsContext ctx, DicomMessage message, out string errorComment)
        {
            errorComment = string.Empty;

            var studyUidList = (string[])message.DataSet[DicomTags.StudyInstanceUid].Values;

            var studyList = (from s in ctx.Studies where studyUidList.Contains(s.StudyUid) select s).ToList();


            return false;
        }

        private bool GetSopListForSeries(PacsContext ctx, DicomMessage message, out string errorComment)
        {
            errorComment = string.Empty;

            var studyInstanctUid = message.DataSet[DicomTags.StudyInstanceUid].GetString(0, string.Empty);
            var seriesUidList = (string[]) message.DataSet[DicomTags.SeriesInstanceUid].Values;

            var studyPk = ctx.Studies.Where(s => s.StudyUid == studyInstanctUid).Select(s => s.Id).FirstOrDefault();

            return false;
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