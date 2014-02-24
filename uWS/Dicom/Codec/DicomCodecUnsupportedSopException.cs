#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;

namespace uWS.Dicom.Codec
{
    [Serializable]
    public class DicomCodecUnsupportedSopException : DicomCodecException
	{
        public DicomCodecUnsupportedSopException()
        {
        }
		public DicomCodecUnsupportedSopException(string desc, Exception e) : base(desc, e)
        {
        }

		public DicomCodecUnsupportedSopException(string desc)
			: base(desc)
        {
        }
	}
}
