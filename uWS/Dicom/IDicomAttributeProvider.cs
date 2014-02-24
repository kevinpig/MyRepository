#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using uWS.Dicom;

namespace uWS.Dicom
{
	public delegate DicomAttribute DicomAttributeGetter(uint tag);
	public delegate void DicomAttributeSetter(uint tag, DicomAttribute value);
	
	/// <summary>
	/// Interface for classes that provide <see cref="DicomAttribute"/>s.
	/// </summary>
	public interface IDicomAttributeProvider
	{
		/// <summary>
		/// Gets or sets the <see cref="DicomAttribute"/> for the given tag.
		/// </summary>
		DicomAttribute this[DicomTag tag] { get; set; }

		/// <summary>
		/// Gets or sets the <see cref="DicomAttribute"/> for the given tag.
		/// </summary>
		DicomAttribute this[uint tag] { get; set; }

		/// <summary>
		/// Attempts to get the attribute specified by <paramref name="tag"/>.
		/// </summary>
		bool TryGetAttribute(uint tag, out DicomAttribute attribute);

		/// <summary>
		/// Attempts to get the attribute specified by <paramref name="tag"/>.
		/// </summary>
		bool TryGetAttribute(DicomTag tag, out DicomAttribute attribute);
	}
}