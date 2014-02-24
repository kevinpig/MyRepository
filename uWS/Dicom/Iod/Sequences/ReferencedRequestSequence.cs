#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using uWS.Dicom.Iod.Macros;

namespace uWS.Dicom.Iod.Sequences
{
	/// <summary>
	/// ReferencedRequest Sequence
	/// </summary>
	/// <remarks>As defined in the DICOM Standard 2008, Part 3, Section C.17.6.2 (Table dcmtable)</remarks>
	public class ReferencedRequestSequence : SequenceIodBase
	{
		/// <summary>
		/// Initializes a new instance of the <see cref="ReferencedRequestSequence"/> class.
		/// </summary>
		public ReferencedRequestSequence() : base() {}

		/// <summary>
		/// Initializes a new instance of the <see cref="ReferencedRequestSequence"/> class.
		/// </summary>
		/// <param name="dicomSequenceItem">The dicom sequence item.</param>
		public ReferencedRequestSequence(DicomSequenceItem dicomSequenceItem) : base(dicomSequenceItem) {}

		/// <summary>
		/// Gets or sets the value of StudyInstanceUid in the underlying collection.
		/// </summary>
		public string StudyInstanceUid
		{
			get { return base.DicomAttributeProvider[DicomTags.StudyInstanceUid].GetString(0, string.Empty); }
			set { base.DicomAttributeProvider[DicomTags.StudyInstanceUid].SetString(0, value); }
		}

		/// <summary>
		/// Gets or sets the value of ReferencedStudySequence in the underlying collection.
		/// </summary>
		public ISopInstanceReferenceMacro ReferencedStudySequence
		{
			get
			{
				var dicomAttribute = base.DicomAttributeProvider[DicomTags.ReferencedStudySequence];
				if (dicomAttribute.IsNull || dicomAttribute.Count == 0)
				{
					return null;
				}
				return new SopInstanceReferenceMacro(((DicomSequenceItem[]) dicomAttribute.Values)[0]);
			}
			set
			{
				if (value == null)
				{
					base.DicomAttributeProvider[DicomTags.ReferencedStudySequence] = null;
					return;
				}
				base.DicomAttributeProvider[DicomTags.ReferencedStudySequence].Values = new DicomSequenceItem[] {value.DicomSequenceItem};
			}
		}

		/// <summary>
		/// Gets or sets the value of AccessionNumber in the underlying collection.
		/// </summary>
		public string AccessionNumber
		{
			get { return base.DicomAttributeProvider[DicomTags.AccessionNumber].GetString(0, string.Empty); }
			set { base.DicomAttributeProvider[DicomTags.AccessionNumber].SetString(0, value); }
		}

		/// <summary>
		/// Gets or sets the value of PlacerOrderNumberImagingServiceRequest in the underlying collection.
		/// </summary>
		public string PlacerOrderNumberImagingServiceRequest
		{
			get { return base.DicomAttributeProvider[DicomTags.PlacerOrderNumberImagingServiceRequest].GetString(0, string.Empty); }
			set { base.DicomAttributeProvider[DicomTags.PlacerOrderNumberImagingServiceRequest].SetString(0, value); }
		}

		/// <summary>
		/// Gets or sets the value of FillerOrderNumberImagingServiceRequest in the underlying collection.
		/// </summary>
		public string FillerOrderNumberImagingServiceRequest
		{
			get { return base.DicomAttributeProvider[DicomTags.FillerOrderNumberImagingServiceRequest].GetString(0, string.Empty); }
			set { base.DicomAttributeProvider[DicomTags.FillerOrderNumberImagingServiceRequest].SetString(0, value); }
		}

		/// <summary>
		/// Gets or sets the value of RequestedProcedureId in the underlying collection.
		/// </summary>
		public string RequestedProcedureId
		{
			get { return base.DicomAttributeProvider[DicomTags.RequestedProcedureId].GetString(0, string.Empty); }
			set { base.DicomAttributeProvider[DicomTags.RequestedProcedureId].SetString(0, value); }
		}

		/// <summary>
		/// Gets or sets the value of RequestedProcedureDescription in the underlying collection.
		/// </summary>
		public string RequestedProcedureDescription
		{
			get { return base.DicomAttributeProvider[DicomTags.RequestedProcedureDescription].GetString(0, string.Empty); }
			set { base.DicomAttributeProvider[DicomTags.RequestedProcedureDescription].SetString(0, value); }
		}

		/// <summary>
		/// Gets or sets the value of RequestedProcedureCodeSequence in the underlying collection.
		/// </summary>
		public CodeSequenceMacro RequestedProcedureCodeSequence
		{
			get
			{
				var dicomAttribute = base.DicomAttributeProvider[DicomTags.RequestedProcedureCodeSequence];
				if (dicomAttribute.IsNull || dicomAttribute.Count == 0)
				{
					return null;
				}
				return new CodeSequenceMacro(((DicomSequenceItem[]) dicomAttribute.Values)[0]);
			}
			set
			{
				if (value == null)
				{
					base.DicomAttributeProvider[DicomTags.RequestedProcedureCodeSequence] = null;
					return;
				}
				base.DicomAttributeProvider[DicomTags.RequestedProcedureCodeSequence].Values = new DicomSequenceItem[] {value.DicomSequenceItem};
			}
		}
	}
}