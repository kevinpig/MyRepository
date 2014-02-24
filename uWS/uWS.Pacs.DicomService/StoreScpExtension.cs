using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using uWS.Common;
using uWS.Dicom;
using uWS.Dicom.Network;
using uWS.Dicom.Network.Scp;
using uWS.Pacs.BussinessLogic;
using uWs.PACS.Model;

namespace uWS.Pacs.DicomService
{
    [Export(typeof(IDicomScp<DicomScpContext>))]
    public class CStoreScp : BaseScp
    {
        #region Private Member

        private IList<SupportedSop> _list;

        private static readonly List<TransferSyntax> TransferSyntaxUIDList =
            new List<TransferSyntax>
                {
                    TransferSyntax.ExplicitVrLittleEndian,
                    TransferSyntax.ImplicitVrLittleEndian,
                };

        #endregion

        public override IList<SupportedSop> GetSupportedSopClasses()
        {
            if (_list == null)
            {
                _list = new List<SupportedSop>();

                var storageAbstractSyntaxList = new List<SopClass>();
                using (var ctx = new PacsContext())
                {
                    storageAbstractSyntaxList.AddRange(
                        ctx.SupportedSopClasses.ToList().Select(
                        sopClass => SopClass.GetSopClass(sopClass.SopClassUid)));
                }

                foreach (var abstractSyntax in storageAbstractSyntaxList)
                {
                    var supportedSop = new SupportedSop {SopClass = abstractSyntax};
                    supportedSop.AddSyntax(TransferSyntax.ExplicitVrLittleEndian);
                    supportedSop.AddSyntax(TransferSyntax.ImplicitVrLittleEndian);
                    _list.Add(supportedSop);
                }
            }

            return _list;
        }

        public override bool OnReceiveRequest(DicomServer server,
                                              ServerAssociationParameters association,
                                              byte presentationID, DicomMessage message)
        {
            try
            {
                IDicomImport import = new DicomImport();
                import.Insert(message);

                server.SendCStoreResponse(presentationID, message.MessageId,
                    message.AffectedSopInstanceUid, DicomStatuses.Success);
            }
            catch (DicomDataException ex)
            {
                Platform.Log(LogLevel.Error, ex);
                return false; // caller will abort the association
            }
            catch (Exception ex)
            {
                Platform.Log(LogLevel.Error, ex);
                return false; // caller will abort the association
            }

            return true;
        }

        protected override DicomPresContextResult OnVerifyAssociation(AssociationParameters association, byte pcid)
        {
            if (Device == null)
                return DicomPresContextResult.Accept;

            if (!Device.AllowStorage)
            {
                return DicomPresContextResult.RejectUser;
            }

            return DicomPresContextResult.Accept;
        }
    }
}