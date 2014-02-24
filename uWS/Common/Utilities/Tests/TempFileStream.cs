#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

#if UNIT_TESTS

using System.IO;

namespace uWS.Common.Utilities.Tests
{
	/// <summary>
	/// A read-writeable <see cref="FileStream"/> using a temporary file that will be deleted upon disposal.
	/// </summary>
	public sealed class TempFileStream : FileStream
	{
		public TempFileStream()
			: base(Path.GetTempFileName(), FileMode.Create, FileAccess.ReadWrite) {}

		protected override void Dispose(bool disposing)
		{
			var path = disposing ? Name : null;

			base.Dispose(disposing);

			if (!string.IsNullOrEmpty(path) && File.Exists(path))
			{
				File.Delete(path);
			}
		}
	}
}

#endif