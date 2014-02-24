#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

﻿using System.Collections.Generic;
using uWS.Dicom;
using uWS.Dicom.Network;
using uWS.Dicom.Network.Scp;

namespace uWS.ImageView.Shreds.DicomServer
{
    public abstract class ScpExtension : IDicomScp<IDicomServerContext>
    {
        private IDicomServerContext _context;
        private readonly List<SupportedSop> _supportedSops;

        protected ScpExtension(IEnumerable<SupportedSop> supportedSops)
        {
            _supportedSops = new List<SupportedSop>(supportedSops);
        }

        protected IDicomServerContext Context
        {
            get { return _context; }
        }

        #region IDicomScp<IDicomServerContext> Member 

        public DicomPresContextResult VerifyAssociation(AssociationParameters association, byte pcid)
        {
            DicomRejectResult result;
            DicomRejectReason reason;

            if (!AssociationVerifier.VerifyAssociation(Context, association, out result, out reason))
            {
                return DicomPresContextResult.RejectUser;
            }

            return OnVerifyAssociation(association, pcid);

        }

        public virtual DicomPresContextResult OnVerifyAssociation(AssociationParameters association, byte pcid)
        {
            return DicomPresContextResult.Accept;
        }

        public abstract bool OnReceiveRequest(Dicom.Network.DicomServer server, 
            ServerAssociationParameters association, byte presentationID, DicomMessage message);

        public IList<SupportedSop> GetSupportedSopClasses()
        {
            return _supportedSops;
        }

        public void SetContext(IDicomServerContext context)
        {
            _context = context;
        }

        public virtual void Cleanup()
        {

        }

        #endregion
    }
}