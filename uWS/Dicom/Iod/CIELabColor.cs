#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;
using System.Collections.Generic;
using System.Text;

namespace uWS.Dicom.Iod
{
	public struct CIELabColor
	{
		private ushort _l;
		private ushort _a;
		private ushort _b;

		public CIELabColor(ushort l, ushort a, ushort b)
		{
			_l = l;
			_a = a;
			_b = b;
		}

		public ushort L
		{
			get { return _l; }
			set { _l = value; }
		}

		public ushort A
		{
			get { return _a; }
			set { _a = value; }
		}

		public ushort B
		{
			get { return _b; }
			set { _b = value; }
		}

		public ushort[] ToArray()
		{
			return new ushort[] {_l, _a, _b};
		}
	}
}
