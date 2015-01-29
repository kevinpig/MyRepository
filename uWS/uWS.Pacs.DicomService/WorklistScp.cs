using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Threading;
using uWS.Common;
using uWS.Dicom;
using uWS.Dicom.Iod;
using uWS.Dicom.Iod.Iods;
using uWS.Dicom.Iod.Sequences;
using uWS.Dicom.Network;
using uWS.Dicom.Network.Scp;

namespace uWS.Pacs.DicomService
{
    [Export(typeof(IDicomScp<DicomScpContext>))]
    public class WorklistScp : BaseScp
    {
        #region Private Field

        private readonly List<SupportedSop> _list = new List<SupportedSop>();
        private bool _cancelReceived = false;

        #endregion
        

        public WorklistScp()
        {
            var sop = new SupportedSop
                {
                    SopClass = SopClass.ModalityWorklistInformationModelFind
                };
            sop.SyntaxList.Add(TransferSyntax.ExplicitVrLittleEndian);
            sop.SyntaxList.Add(TransferSyntax.ImplicitVrLittleEndian);

            _list.Add(sop);
        }

        #region MWL

        private void OnReceiveMWLQuery(DicomServer server, byte presentationId, DicomMessage message)
        {
            var tagList = new List<DicomTag>();

            string specificCharacterSet = string.Empty;

            foreach (var attrib in message.DataSet)
            {
                tagList.Add(attrib.Tag);

                if (!attrib.IsNull)
                {
                    switch (attrib.Tag.TagValue)
                    {
                        case DicomTags.SpecificCharacterSet:
                            specificCharacterSet = message.DataSet[DicomTags.SpecificCharacterSet];
                            break;

                        #region Case Scheduled Procedure Step Seq

                        case DicomTags.ScheduledProcedureStepSequence:
                            DicomAttributeSQ sequence = attrib as DicomAttributeSQ;
                            DicomSequenceItem sqSubItems = sequence[0];

                            foreach (var sqSubItem in sqSubItems)
                            {
                                if(sqSubItem.IsNull)
                                    continue;

                                tagList.Add(sqSubItem.Tag);

                                switch (sqSubItem.Tag.TagValue)
                                {
                                    case DicomTags.ScheduledStationName:
                                        break;
                                    case DicomTags.Modality:
                                        break;
                                    case DicomTags.ScheduledPerformingPhysiciansName:
                                        break;
                                    case DicomTags.ScheduledProcedureStepStartDate:
                                        break;
                                    case DicomTags.ScheduledProcedureStepStartTime:
                                        break;

                                    // Optional Matching keys
                                    case DicomTags.ScheduledProcedureStepLocation:
                                        break;
                                    case DicomTags.ScheduledProcedureStepDescription:
                                        break;
                                    case DicomTags.RequestedProcedureId:
                                        break;
                                    case DicomTags.ScheduledProcedureStepId:
                                        break;
                                    default:
                                        break;
                                }
                            }
                            break;

                        #endregion

                        case DicomTags.PatientId:
                            break;
                        case DicomTags.PatientsName:
                            break;

                        //Optional Matching Keys
                        case DicomTags.AccessionNumber:
                            break;

                        case DicomTags.ReferringPhysiciansName:
                            break;
                        default:
                            break;
                    }
                }
            }

            // Populate result Information from Db
            List<DicomMessage> results = new List<DicomMessage>();

            var msg = new DicomMessage();
            msg.DataSet.SpecificCharacterSet = specificCharacterSet;
            ModalityWorklistIod iod = new ModalityWorklistIod(msg.DataSet);
            iod.SetCommonTags();

            iod.PatientIdentificationModule.PatientId = "1402001111";
            iod.PatientIdentificationModule.PatientsName = new PersonName("测试");
            iod.PatientIdentificationModule.PatientsSex = PatientsSex.M;
            iod.PatientIdentificationModule.PatientsBirthDate = new DateTime(2011, 11, 19);

            var spssIod = new ScheduledProcedureStepSequenceIod();
            spssIod.SetCommonTags();
            spssIod.DicomAttributeProvider[DicomTags.ScheduledProcedureStepStartDate].SetString(0, "20141119");
            spssIod.DicomAttributeProvider[DicomTags.ScheduledProcedureStepStartTime].SetString(0, "143806");

            iod.ScheduledProcedureStepModule.ScheduledProcedureStepSequenceList.Clear();
            iod.ScheduledProcedureStepModule.ScheduledProcedureStepSequenceList.Add(spssIod);

          results.Add(msg);
            try
            {
                foreach (var dicomMessage in results)
                {
                    if (_cancelReceived)
                    {
                        throw new DicomException("DICOM C-Cancel Received");
                    }
                    
                    server.SendCFindResponse(presentationId, message.MessageId, dicomMessage,
                                                         DicomStatuses.Pending);
                }
                
            }
            catch (Exception)
            {
                if (_cancelReceived)
                {
                    var errorResponse = new DicomMessage();
                    server.SendCFindResponse(presentationId, message.MessageId, errorResponse,
                                                DicomStatuses.Cancel);
                }

                return;
            }

            var finalResponse = new DicomMessage();
            server.SendCFindResponse(presentationId, message.MessageId, finalResponse, DicomStatuses.Success);
        }

        #endregion

        public override IList<SupportedSop> GetSupportedSopClasses()
        {
            return _list;
        }

        public override bool OnReceiveRequest(DicomServer server, ServerAssociationParameters association,
                                              byte presentationID, DicomMessage message)
        {
            if (message.CommandField == DicomCommandField.CCancelRequest)
            {
                Platform.Log(LogLevel.Info, "Received Worklist-CANCEL-RQ message.");
                _cancelReceived = true;
                return true;
            }

            if (message.AffectedSopClassUid.Equals(SopClass.ModalityWorklistInformationModelFindUid))
            {
                // We use the ThreadPool to process the thread requests. This is so that we return back
                // to the main message loop, and continue to look for cancel request messages coming
                // in.  There's a small chance this may cause delays in responding to query requests if
                // the .NET Thread pool fills up.
                ThreadPool.QueueUserWorkItem(delegate
                {
                    try
                    {
                        OnReceiveMWLQuery(server, presentationID, message);
                    }
                    catch (Exception x)
                    {
                        Platform.Log(LogLevel.Error, x, "Unexpected exception in OnReceiveStudyLevelQuery.");
                    }
                });
                return true;
            }

            // no supported message type, send a failure status 
            server.SendCFindResponse(presentationID, message.MessageId, new DicomMessage(),
                DicomStatuses.QueryRetrieveIdentifierDoesNotMatchSOPClass);

            return true;
        }

        protected override DicomPresContextResult OnVerifyAssociation(AssociationParameters association, byte pcid)
        {
//             if (!Device.AllowWorkList)
//             {
//                 return DicomPresContextResult.RejectUser;
//             }

            return DicomPresContextResult.Accept; 
        }
    }
}