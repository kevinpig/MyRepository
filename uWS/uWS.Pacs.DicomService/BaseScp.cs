#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

﻿using System;
using System.Collections.Generic;
using uWS.Common;
using uWS.Dicom;
using uWS.Dicom.Network;
using uWS.Dicom.Network.Scp;
using uWs.PACS.Model;

namespace uWS.Pacs.DicomService
{
    public abstract class BaseScp : IDicomScp<DicomScpContext>
    {
        #region Protected Members

        private DicomScpContext _context;

        #endregion

        #region Properties

        protected ServerPartition Partition
        {
            get { return _context.Partition; }
        }

        protected Device Device { get; set; }

        #endregion

        protected abstract DicomPresContextResult OnVerifyAssociation(AssociationParameters association,
                                                                      byte pcid);

        public DicomPresContextResult VerifyAssociation(AssociationParameters association, byte pcid)
        {
            bool isNew;

            Device = DeviceManager.LookupDevice(Partition, association, out isNew);

            var result = OnVerifyAssociation(association, pcid);
            if (result != DicomPresContextResult.Accept)
            {
                Platform.Log( LogLevel.Debug, 
                    "Rejecting Presentation Context {0}:{1} in association between {2} and {3}.",
                    pcid, association.GetAbstractSyntax(pcid).Description,
                    association.CallingAE, association.CalledAE);
            }

            return result;

        }

        public virtual bool OnReceiveRequest(DicomServer server, ServerAssociationParameters association,
                                     byte presentationID, DicomMessage message)
        {
            throw new NotImplementedException("The method must be implement");
        }

        public virtual IList<SupportedSop> GetSupportedSopClasses()
        {
            throw new System.NotImplementedException("The method must be implement");
        }

        public void SetContext(DicomScpContext context)
        {
            _context = context;
        }

        public void Cleanup()
        {
            throw new NotImplementedException();
        }
    }
}
