#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

namespace uWS.Dicom.Iod.Macros
{
	public interface IIodMacro
	{
		/// <summary>
		/// Initializes the underlying collection to implement the module or sequence using default values.
		/// </summary>
		void InitializeAttributes();

		/// <summary>
		/// Gets or sets the underlying DICOM sequence item.
		/// </summary>
		/// <remarks>
		/// This property may return NULL for macros implemented at the module level rather than on a sequence item.
		/// </remarks>
		/// <value>The DICOM sequence item.</value>
		DicomSequenceItem DicomSequenceItem { get; set; }
	}
}