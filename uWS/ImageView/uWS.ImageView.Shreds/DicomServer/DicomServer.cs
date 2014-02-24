#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

﻿using System.Net;
using uWS.Dicom.Network.Scp;

namespace uWS.ImageView.Shreds.DicomServer
{
    public interface IDicomServerContext
    {
        string AETitle { get; }
        string Host { get; }
        int Port { get; }
    }

    internal class DicomServer
    {
        public class DicomServerContext : IDicomServerContext
        {
            private readonly DicomServer _server;

            public string AETitle { get; private set; }
            public string Host { get; private set; }
            public int Port { get; private set; }
        }

        private readonly IDicomServerContext _context;

        private readonly DicomScp<IDicomServerContext> _scp;

        private readonly string _aeTitle;

        private readonly string _host;

        private readonly int _port;

        public DicomServer()
        {
            _aeTitle = string.Empty;
            _host = string.Empty;
            _port = 104;

            _context = new DicomServerContext();
            _scp = new DicomScp<IDicomServerContext>(_context, AssociationVerifier.VerifyAssociation);
        }

        #region Public Properties

        public string AETitle
        {
            get { return _aeTitle; }
        }

        public string Host
        {
            get { return _host; }
        }

        public int Port
        {
            get { return _port; }
        }

        #endregion

        #region Start & Stop Method

        public void Start()
        {
            IPHostEntry entry = Dns.GetHostEntry(_host);
            IPAddress address = entry.AddressList[0];
            IPAddress localhost = Dns.GetHostEntry("localhost").AddressList[0];

            if (localhost.Equals(address))
                address = IPAddress.Any;

            _scp.AeTitle = _aeTitle;
            _scp.ListenPort = _port;

            _scp.Start(address);
        }

        public void Stop()
        {
            _scp.Stop();
        }

        #endregion
    }
}