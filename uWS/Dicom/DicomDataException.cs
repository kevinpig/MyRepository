#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;

namespace uWS.Dicom
{
	[Serializable]
    public class DicomDataException : DicomException
    {
        public DicomDataException(String desc)
            : base(desc)
        {
        }
    }
}
