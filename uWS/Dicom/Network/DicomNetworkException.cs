#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;

namespace uWS.Dicom.Network
{
	[Serializable]
    public class DicomNetworkException : DicomException
    {
        public DicomNetworkException(String desc)
            : base(desc)
        {
        }
    }
}
