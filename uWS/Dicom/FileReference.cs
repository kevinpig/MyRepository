#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

namespace uWS.Dicom
{
	internal class FileReference
	{
		#region Private Members

		private readonly string _filename;
		private readonly long _offset;
		private readonly long _length;
		private readonly Endian _endian;
		private readonly DicomVr _vr;

		#endregion

		#region Public Properties

		internal string Filename
		{
			get { return _filename; }
		}

		internal long Offset
		{
			get { return _offset; }
		}

		internal Endian Endian
		{
			get { return _endian; }
		}

		internal DicomVr Vr
		{
			get { return _vr; }
		}

		public uint Length
		{
			get { return (uint) _length; }
		}

		#endregion

		#region Constructors

		internal FileReference(string file, long offset, long length, Endian endian, DicomVr vr)
		{
			_filename = file;
			_offset = offset;
			_length = length;
			_endian = endian;
			_vr = vr;
		}

		#endregion
	}
}