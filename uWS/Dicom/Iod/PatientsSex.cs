#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

namespace uWS.Dicom.Iod
{
	/// <summary>
	/// Enumerated values for the <see cref="DicomTags.PatientsSex"/> attribute indicating the sex of the named patient.
	/// </summary>
	/// <remarks>As defined in the DICOM Standard 2008, Part 3, Section C.7.1.1 (Table C.7-1)</remarks>
	public enum PatientsSex
	{
		/// <summary>
		/// male.
		/// </summary>
		M,

		/// <summary>
		/// female.
		/// </summary>
		F,

		/// <summary>
		/// other.
		/// </summary>
		O,

		/// <summary>
		/// Represents the null value.
		/// </summary>
		None
	}
}
