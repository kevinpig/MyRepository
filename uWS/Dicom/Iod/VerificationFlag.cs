#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

namespace uWS.Dicom.Iod
{
	/// <summary>
	/// Enumerated values for the <see cref="uWS.Dicom.DicomTags.VerificationFlag"/> attribute indicating whether the Encapsulated Document is verified.
	/// </summary>
	/// <remarks>
	/// <para>As defined in the DICOM Standard 2009, Part 3, Section C.24.2 (Table C.24-2)</para>
	/// </remarks>
	public enum VerificationFlag
	{
		/// <summary>
		/// Represents the null value.
		/// </summary>
		None,

		/// <summary>
		/// Indicates that the Encapsulated Document is not attested by a legally accountable person.
		/// </summary>
		Unverified,

		/// <summary>
		/// Indicates that the Encapsulated Document is attested to (signed) by a Verifying Observer
		/// or Legal Authenticator named in the document, who is accountable for its content.
		/// </summary>
		Verified
	}
}