#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using uWS.Dicom.Iod.Macros;

namespace uWS.Dicom.Iod.Sequences
{
	/// <summary>
	/// DeIdentificationMethod Code Sequence
	/// </summary>
	/// <remarks>As defined in the DICOM Standard 2008, Part 3, Section C.7.1.1 (Table C.7-1)</remarks>
	public class DeIdentificationMethodCodeSequence : CodeSequenceMacro
	{
		/// <summary>
		/// Initializes a new instance of the <see cref="DeIdentificationMethodCodeSequence"/> class.
		/// </summary>
		public DeIdentificationMethodCodeSequence() : base() {}

		/// <summary>
		/// Initializes a new instance of the <see cref="DeIdentificationMethodCodeSequence"/> class.
		/// </summary>
		/// <param name="dicomSequenceItem">The dicom sequence item.</param>
		public DeIdentificationMethodCodeSequence(DicomSequenceItem dicomSequenceItem) : base(dicomSequenceItem) {}
	}
}
