using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using uWS.Common;
using uWS.Dicom;
using uWS.Dicom.Iod.Iods;
using uWS.Dicom.Iod.Modules;
using uWS.Dicom.Network;
using uWS.Dicom.Network.Scp;

namespace uWS.Pacs.DicomService
{
    //[Export(typeof(IDicomScp<DicomScpContext>))]
    public class MppsScp : BaseScp
    {
        private readonly List<SupportedSop> _list = new List<SupportedSop>(); 

        public MppsScp()
        {
            var sop = new SupportedSop
            {
                SopClass = SopClass.ModalityPerformedProcedureStepSopClass
            };

            sop.SyntaxList.Add(TransferSyntax.ExplicitVrLittleEndian);
            sop.SyntaxList.Add(TransferSyntax.ImplicitVrLittleEndian);

            _list.Add(sop);
        }

        protected override DicomPresContextResult OnVerifyAssociation(AssociationParameters association, byte pcid)
        {
            if (!Device.AllowMPPS)
            {
                return DicomPresContextResult.RejectUser;
            }

            return DicomPresContextResult.Accept;
        }

        public override bool OnReceiveRequest(DicomServer server, ServerAssociationParameters association,
                                             byte presentationID, DicomMessage message)
        {
            #region MPPS NCreate Request

            if (message.CommandField == DicomCommandField.NCreateRequest)
            {
                ModalityPerformedProcedureStepIod mppsIod = new ModalityPerformedProcedureStepIod(message.DataSet);
                bool conform = CheckNCreateDataSetConformance(server, association, presentationID, mppsIod, true);

                if (!conform)
                {
                    server.SendNCreateResponse(presentationID, message.MessageId, new DicomMessage(), DicomStatuses.InvalidAttributeValue);
                    Platform.Log(LogLevel.Error, "Sending Invalid Attributes Response.");
                    return true;
                }

                // wrong status
                if (mppsIod.PerformedProcedureStepInformation.PerformedProcedureStepStatus != PerformedProcedureStepStatus.InProgress)
                {
                    server.SendNCreateResponse(presentationID, message.MessageId, new DicomMessage(), DicomStatuses.InvalidAttributeValue);
                    Platform.Log(LogLevel.Error, "Received N-Create Request with bad status.");
                    return true;
                }

                if (!ProcessNCreateRequest(server, presentationID, message))
                {
                    server.SendNCreateResponse(presentationID, message.MessageId, new DicomMessage(), DicomStatuses.ProcessingFailure);
                    return true;
                }

                server.SendNCreateResponse(presentationID, message.MessageId, new DicomMessage(), DicomStatuses.Success);

            }

            #endregion

            #region MPPS NSet Request

            if (message.CommandField == DicomCommandField.NSetRequest)
            {
                ModalityPerformedProcedureStepIod mppsIod = new ModalityPerformedProcedureStepIod(message.DataSet);

                string studyInstanceUID = mppsIod.
                                          PerformedProcedureStepRelationship.
                                          DicomAttributeProvider[DicomTags.StudyInstanceUid].
                                          GetString(0, "");

                // Get Cached 
                ModalityPerformedProcedureStepIod cachedMppsIod = new ModalityPerformedProcedureStepIod();
                bool conform = CheckNSetDataSetConformance(server, association, presentationID, cachedMppsIod, mppsIod, true);

                if (!conform)
                {
                    server.SendNCreateResponse(presentationID, message.MessageId, new DicomMessage(), DicomStatuses.InvalidAttributeValue);
                    Platform.Log(LogLevel.Error, "Sending Failure Response.");
                    return true;
                }

                bool success = true;
                if (mppsIod.PerformedProcedureStepInformation.PerformedProcedureStepStatus
                            == PerformedProcedureStepStatus.Completed)
                {
                    success = ProcessNSetRequestForCompleted(server, presentationID, message);
                }

                if (mppsIod.PerformedProcedureStepInformation.PerformedProcedureStepStatus
                            == PerformedProcedureStepStatus.Discontinued)
                {
                    success = ProcessNSetRequestForDiscontinued(server, presentationID, message);
                }
                
                server.SendNSetResponse(presentationID, message.MessageId, new DicomMessage(),
                                    success ? DicomStatuses.Success : DicomStatuses.ProcessingFailure);
            }

            #endregion

            // no supported message type, send a failure status 
            server.SendCFindResponse(presentationID, message.MessageId, new DicomMessage(),
                DicomStatuses.QueryRetrieveIdentifierDoesNotMatchSOPClass);

            return true;
        }

        #region MPPS process

        private bool ProcessNCreateRequest(DicomServer server, byte presentationId, DicomMessage message)
        {
            throw new NotImplementedException();
        }

        private bool ProcessNSetRequestForCompleted(DicomServer server, byte presentationId, DicomMessage message)
        {
            throw new NotImplementedException();
        }

        private bool ProcessNSetRequestForDiscontinued(DicomServer server, byte presentationId, DicomMessage message)
        {
            throw new NotImplementedException();
        }

        #endregion

        #region MPPS MessageChecking methods

        private bool CheckNCreateDataSetConformance(DicomServer server, ServerAssociationParameters association, byte presentationID,
                                         ModalityPerformedProcedureStepIod mppsIod,
                                         bool logFirstAnomalyOnly)
        {
            throw new NotImplementedException();
        }

        private bool CheckNSetDataSetConformance(DicomServer server, ServerAssociationParameters association, byte presentationID,
                                      ModalityPerformedProcedureStepIod cachedMppsIod,
                                      ModalityPerformedProcedureStepIod receivedMppsIod, bool logFirstAnomalyOnly)
        {
            throw new NotImplementedException();

        }


        #endregion
    }
}