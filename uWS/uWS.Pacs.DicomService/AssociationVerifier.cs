#region License

// 
// Copyright (c) 2011 - 2012, United-Imaging Inc.
// All rights reserved.
// http://www.united-imaging.com

#endregion

using uWS.Common;
using uWS.Dicom.Network;
using uWs.PACS.Model;

namespace uWS.Pacs.DicomService
{
    /// <summary>
    /// Static class used to verify if an association should be accepted.
    /// </summary>
    public static class AssociationVerifier
    {
        /// <summary>
        /// Do the actual verification if an association is acceptable.
        /// </summary>
        /// <remarks>
        /// This method primarily checks the remote AE title to see if it is a valid device that can 
        /// connect to the partition.
        /// </remarks>
        /// <param name="context">Generic parameter passed in, is a DicomScpParameters instance.</param>
        /// <param name="assocParms">The association parameters.</param>
        /// <param name="result">Output parameter with the DicomRejectResult for rejecting the association.</param>
        /// <param name="reason">Output parameter with the DicomRejectReason for rejecting the association.</param>
        /// <returns>true if the association should be accepted, false if it should be rejected.</returns>
        public static bool Verify(DicomScpContext context, ServerAssociationParameters assocParms,
                                  out DicomRejectResult result, out DicomRejectReason reason)
        {
            bool isNew;
            Device device = DeviceManager.LookupDevice(context.Partition, assocParms, out isNew);

            if (device == null)
            {
                if (context.Partition.AcceptAnyDevice)
                {
                    reason = DicomRejectReason.NoReasonGiven;
                    result = DicomRejectResult.Permanent;
                    return true;
                }

                reason = DicomRejectReason.CallingAENotRecognized;
                result = DicomRejectResult.Permanent;
                return false;
            }

            if (device.Enabled == false)
            {
                Platform.Log(LogLevel.Error, "Rejecting association from {0} to {1}.  Device is disabled.",
                                       assocParms.CallingAE, assocParms.CalledAE);

                reason = DicomRejectReason.CallingAENotRecognized;
                result = DicomRejectResult.Permanent;
                return false;
            }

            reason = DicomRejectReason.NoReasonGiven;
            result = DicomRejectResult.Permanent;

            return true;
        }
    }
}