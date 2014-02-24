#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

namespace uWS.Dicom.Iod
{
	/// <summary>
	/// Enumerated values for the <see cref="DicomTags.MacAlgorithm"/> attribute identifying the algorithm used in generating the MAC.
	/// </summary>
	/// <remarks>As defined in the DICOM Standard 2008, Part 3, Section C.17.2.1 (Table C.17-3a)</remarks>
	public enum MacAlgorithm
	{
		/// <summary>
		/// RIPEMD160
		/// </summary>
		RIPEMD160,

		/// <summary>
		/// MD5
		/// </summary>
		MD5,

		/// <summary>
		/// SHA1
		/// </summary>
		SHA1,

		/// <summary>
		/// Represents the unknown status, which is equivalent to the null value.
		/// </summary>
		Unknown
	}
}