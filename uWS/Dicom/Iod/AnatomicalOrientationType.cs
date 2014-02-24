#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

namespace uWS.Dicom.Iod
{
	/// <summary>
	/// Enumerated values for the <see cref="DicomTags.AnatomicalOrientationType"/> attribute indicating the anatomical orientation type of the named patient.
	/// </summary>
	/// <remarks>
	/// As defined in the DICOM Standard 2009, Part 3, Section C.7.3.1 (Table C.7-5a)
	/// See C.7.6.1.1.1 for the effect on the anatomical direction.
	/// </remarks>
	public enum AnatomicalOrientationType
	{
		/// <summary>
		/// None.
		/// </summary>
		None,

		/// <summary>
		/// Biped.
		/// </summary>
		Biped,

		/// <summary>
		/// Quadruped.
		/// </summary>
		Quadruped
	}
}