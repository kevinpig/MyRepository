#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;
using System.Runtime.Serialization;

namespace uWS.Dicom
{
    [Serializable]
    public class DicomException : Exception
    {
        public DicomException(){}

        public DicomException(String desc)
            : base(desc)
        {
        }
        public DicomException(String desc, Exception e)
            : base(desc,e)
        {
        }
        protected DicomException(SerializationInfo info, StreamingContext context)
            : base(info, context)
        {

        }
    }
}
