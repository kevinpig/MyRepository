using System.Collections.Generic;
using uWS.Dicom;
using uWS.Dicom.Network;

namespace uWS.ImageView.Shreds.DicomServer
{
    public class VerifyScpExtension : ScpExtension
    {
        public VerifyScpExtension()
            : base(GetSupportedSops())
        {
            
        }

        private static IEnumerable<SupportedSop> GetSupportedSops()
        {
            var sop = new SupportedSop()
                {
                    SopClass = SopClass.VerificationSopClass
                };

            sop.SyntaxList.Add(TransferSyntax.ExplicitVrLittleEndian);
            sop.SyntaxList.Add(TransferSyntax.ImplicitVrLittleEndian);

            yield return sop;
        }

        public override bool OnReceiveRequest(Dicom.Network.DicomServer server, ServerAssociationParameters association, byte presentationID,
                                              DicomMessage message)
        {
            server.SendCEchoResponse(presentationID, message.MessageId, DicomStatuses.Success);

            return true;
        }
    }
}