#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;

namespace uWS.Dicom.Iod
{
	[Flags]
	public enum ShutterShape
	{
		None,
		Circular = 1,
		Rectangular = 2,
		Polygonal = 4,
		Bitmap = 8
	}
}
