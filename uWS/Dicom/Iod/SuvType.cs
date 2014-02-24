#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

namespace uWS.Dicom.Iod
{
	/// <summary>
	/// Enumerated values for the <see cref="uWS.Dicom.DicomTags.SuvType"/> attribute 
	/// </summary>
	/// <remarks>As defined in the DICOM Standard 2011, Part 3, Section C.8.9.1 (Table C.8-60)</remarks>
	public enum SuvType
	{
		/// <summary>
		/// Represents the null value.
		/// </summary>
		None,

		/// <summary>
		/// BSA (body surface area)
		/// </summary>
		BSA,

		/// <summary>
		/// BW (body weight)
		/// </summary>
		BW,

		/// <summary>
		/// LBM (lean body mass)
		/// </summary>
		LBM
	}
}