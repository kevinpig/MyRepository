#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;

namespace uWS.Dicom.Codec
{
    /// <summary>
    /// A codec specific exception.
    /// </summary>
    [Serializable]
    public class DicomCodecException : DicomException
    {
        public DicomCodecException()
        {
        }
        public DicomCodecException(string desc, Exception e) : base(desc, e)
        {
        }

        public DicomCodecException(string desc) : base(desc)
        {
        }
    }
}
