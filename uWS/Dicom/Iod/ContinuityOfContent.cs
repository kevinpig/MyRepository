#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

namespace uWS.Dicom.Iod
{
	/// <summary>
	/// Enumerated values for the <see cref="DicomTags.ContinuityOfContent"/> attribute specifying for a CONTAINER whether or not
	/// its contained Content Items are logically linked in a
	/// continuous textual flow, or are separate items.
	/// </summary>
	/// <remarks>As defined in the DICOM Standard 2008, Part 3, Section C.18.8 (Table C.18.8-1)</remarks>
	public enum ContinuityOfContent
	{
		/// <summary>
		/// SEPARATE
		/// </summary>
		Separate,

		/// <summary>
		/// CONTINUOUS
		/// </summary>
		Continuous,

		/// <summary>
		/// Represents the unknown status, which is equivalent to the null value.
		/// </summary>
		Unknown
	}
}