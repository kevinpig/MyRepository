#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System.Collections.Generic;
using System.ComponentModel.Composition;
using uWS.Dicom;
using uWS.Dicom.Network;
using uWS.Dicom.Network.Scp;

namespace uWS.Pacs.DicomService
{
    [Export(typeof(IDicomScp<DicomScpContext>))]
    public class CEchoScp : BaseScp
    {
        #region Private members

        private readonly List<SupportedSop> _list = new List<SupportedSop>();

        #endregion

        #region Contructors

        /// <summary>
        /// Public default constructor. Implements the Verification SOP Class.
        /// </summary>
        public CEchoScp()
        {
            var sop = new SupportedSop {SopClass = SopClass.VerificationSopClass};
            sop.SyntaxList.Add(TransferSyntax.ExplicitVrLittleEndian);
            sop.SyntaxList.Add(TransferSyntax.ImplicitVrLittleEndian);

            _list.Add(sop);
        }

        #endregion

        #region IDicomScp Members

        public override bool OnReceiveRequest(DicomServer server, ServerAssociationParameters association,
                                              byte presentationID, DicomMessage message)
        {
            server.SendCEchoResponse(presentationID, message.MessageId, DicomStatuses.Success);

            return true;
        }

        public override IList<SupportedSop> GetSupportedSopClasses()
        {
            return _list;
        }

        #endregion

        #region Overridden BaseSCP methods

        protected override DicomPresContextResult OnVerifyAssociation(AssociationParameters association,
                                                                      byte pcid)
        {
            return DicomPresContextResult.Accept;
        }

        #endregion
    }
}