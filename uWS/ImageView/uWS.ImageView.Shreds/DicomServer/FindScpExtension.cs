using System;
using System.Collections.Generic;
using System.Net;
using uWS.Common;
using uWS.Dicom;
using uWS.Dicom.Network;

namespace uWS.ImageView.Shreds.DicomServer
{
    public class FindScpExtension : ScpExtension
    {
        public FindScpExtension() : base(GetSupportedSops())
        {
        }

        private static IEnumerable<SupportedSop> GetSupportedSops()
        {
            var sop = new SupportedSop()
                {
                    SopClass = SopClass.StudyRootQueryRetrieveInformationModelFind
                };
            sop.SyntaxList.Add(TransferSyntax.ExplicitVrLittleEndian);
            sop.SyntaxList.Add(TransferSyntax.ImplicitVrLittleEndian);

            yield return sop;
        }

        private static string GetRemoteHostname(AssociationParameters association)
        {
            string remoteHostname = null;
            try
            {
                 if (association.RemoteEndPoint != null)
                 {
                     try
                     {
                         IPHostEntry entry = Dns.GetHostEntry(association.RemoteEndPoint.Address);
                         remoteHostname = entry.HostName;
                     }
                     catch 
                     {
                         remoteHostname = association.RemoteEndPoint.Address.ToString();
                     }
                 }
            }
            catch (Exception e)
            {
                remoteHostname = null;
                Platform.Log(LogLevel.Warn, e, "Unable to resolve remote host name for auditing.");
            }

            return remoteHostname;
        }

        public override bool OnReceiveRequest(Dicom.Network.DicomServer server, ServerAssociationParameters association, byte presentationID,
                                              DicomMessage message)
        {
            throw new System.NotImplementedException();
        }
    }
}