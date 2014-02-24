using uWS.Dicom.Network;

namespace uWS.ImageView.Shreds.DicomServer
{
    public static class AssociationVerifier
    {
        public static bool VerifyAssociation(IDicomServerContext context, AssociationParameters assoParams, 
            out DicomRejectResult result, out DicomRejectReason reason)
        {
            string calledAET = (assoParams.CalledAE ?? "").Trim();
            string callingAET = (assoParams.CallingAE ?? "").Trim();

            result = DicomRejectResult.Permanent;
            reason = DicomRejectReason.NoReasonGiven;

            return true;
        }
    }
}