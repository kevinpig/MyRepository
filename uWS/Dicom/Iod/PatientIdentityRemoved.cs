#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

namespace uWS.Dicom.Iod
{
	/// <summary>
	/// Enumerated values for the <see cref="DicomTags.PatientIdentityRemoved"/> attribute indicating that the true identity of the patient has been
	/// removed from the Attributes and the Pixel Data
	/// </summary>
	/// <remarks>As defined in the DICOM Standard 2008, Part 3, Section C.7.1.1 (Table C.7-1)</remarks>
	public enum PatientIdentityRemoved
	{
		/// <summary>
		/// YES
		/// </summary>
		Yes, 

		/// <summary>
		/// NO
		/// </summary>
		No,

		/// <summary>
		/// Represents the unknown status, which is equivalent to the null value.
		/// </summary>
		Unknown
	}
}
