#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

﻿namespace uWs.ImageView.Common.DicomServer
{
    public class DicomServer : IDicomServer
    {
        public static string AETitle { get; set; }

        public static string Hostname { get; set; }

        public static int Port { get; set; }

    }
}