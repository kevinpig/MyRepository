#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;
using uWS.Dicom.Iod.Macros.HierarchicalSeriesInstanceReference;
using uWS.Dicom.Iod.Sequences;

namespace uWS.Dicom.Iod.Macros
{
	/// <summary>
	/// HierarchicalSeriesInstanceReference Macro
	/// </summary>
	/// <remarks>As defined in the DICOM Standard 2008, Part 3, Section C.17.2.1 (Table C.17-3a)</remarks>
	public interface IHierarchicalSeriesInstanceReferenceMacro : IIodMacro
	{
		/// <summary>
		/// Gets or sets the value of SeriesInstanceUid in the underlying collection. Type 1.
		/// </summary>
		string SeriesInstanceUid { get; set; }

		/// <summary>
		/// Gets or sets the value of RetrieveAeTitle in the underlying collection. Type 3.
		/// </summary>
		string RetrieveAeTitle { get; set; }

		/// <summary>
		/// Gets or sets the value of StorageMediaFileSetId in the underlying collection. Type 3.
		/// </summary>
		string StorageMediaFileSetId { get; set; }

		/// <summary>
		/// Gets or sets the value of StorageMediaFileSetUid in the underlying collection. Type 3.
		/// </summary>
		string StorageMediaFileSetUid { get; set; }

		/// <summary>
		/// Gets or sets the value of ReferencedSopSequence in the underlying collection. Type 1.
		/// </summary>
		IReferencedSopSequence[] ReferencedSopSequence { get; set; }

		/// <summary>
		/// Creates a single instance of a ReferencedSopSequence item. Does not modify the ReferencedSopSequence in the underlying collection.
		/// </summary>
		IReferencedSopSequence CreateReferencedSopSequence();
	}

	namespace HierarchicalSeriesInstanceReference
	{
		public interface IReferencedSopSequence : ISopInstanceReferenceMacro
		{
			/// <summary>
			/// Gets or sets the value of PurposeOfReferenceCodeSequence in the underlying collection. Type 3.
			/// </summary>
			CodeSequenceMacro[] PurposeOfReferenceCodeSequence { get; set; }

			/// <summary>
			/// Gets or sets the value of ReferencedDigitalSignatureSequence in the underlying collection. Type 3.
			/// </summary>
			ReferencedDigitalSignatureSequence[] ReferencedDigitalSignatureSequence { get; set; }

			/// <summary>
			/// Gets or sets the value of ReferencedSopInstanceMacSequence in the underlying collection. Type 3.
			/// </summary>
			ReferencedSopInstanceMacSequence ReferencedSopInstanceMacSequence { get; set; }
		}
	}

	/// <summary>
	/// HierarchicalSeriesInstanceReference Macro Base Implementation
	/// </summary>
	/// <remarks>As defined in the DICOM Standard 2008, Part 3, Section C.17.2.1 (Table C.17-3a)</remarks>
	internal class HierarchicalSeriesInstanceReferenceMacro : SequenceIodBase, IHierarchicalSeriesInstanceReferenceMacro
	{
		/// <summary>
		/// Initializes a new instance of the <see cref="HierarchicalSeriesInstanceReferenceMacro"/> class.
		/// </summary>
		public HierarchicalSeriesInstanceReferenceMacro() : base() {}

		/// <summary>
		/// Initializes a new instance of the <see cref="HierarchicalSeriesInstanceReferenceMacro"/> class.
		/// </summary>
		/// <param name="dicomSequenceItem">The dicom sequence item.</param>
		public HierarchicalSeriesInstanceReferenceMacro(DicomSequenceItem dicomSequenceItem) : base(dicomSequenceItem) {}

		/// <summary>
		/// Initializes the underlying collection to implement the module or sequence using default values.
		/// </summary>
		public void InitializeAttributes()
		{
			this.SeriesInstanceUid = "1";
			this.RetrieveAeTitle = null;
			this.StorageMediaFileSetId = null;
			this.StorageMediaFileSetUid = null;
		}

		/// <summary>
		/// Gets or sets the value of SeriesInstanceUid in the underlying collection. Type 1.
		/// </summary>
		public string SeriesInstanceUid
		{
			get { return base.DicomAttributeProvider[DicomTags.SeriesInstanceUid].GetString(0, string.Empty); }
			set
			{
				if (string.IsNullOrEmpty(value))
					throw new ArgumentNullException("value", "SeriesInstanceUid is Type 1 Required.");
				base.DicomAttributeProvider[DicomTags.SeriesInstanceUid].SetString(0, value);
			}
		}

		/// <summary>
		/// Gets or sets the value of RetrieveAeTitle in the underlying collection. Type 3.
		/// </summary>
		public string RetrieveAeTitle
		{
			get { return base.DicomAttributeProvider[DicomTags.RetrieveAeTitle].ToString(); }
			set
			{
				if (string.IsNullOrEmpty(value))
				{
					base.DicomAttributeProvider[DicomTags.RetrieveAeTitle] = null;
					return;
				}
				base.DicomAttributeProvider[DicomTags.RetrieveAeTitle].SetStringValue(value);
			}
		}

		/// <summary>
		/// Gets or sets the value of StorageMediaFileSetId in the underlying collection. Type 3.
		/// </summary>
		public string StorageMediaFileSetId
		{
			get { return base.DicomAttributeProvider[DicomTags.StorageMediaFileSetId].GetString(0, string.Empty); }
			set
			{
				if (string.IsNullOrEmpty(value))
				{
					base.DicomAttributeProvider[DicomTags.StorageMediaFileSetId] = null;
					return;
				}
				base.DicomAttributeProvider[DicomTags.StorageMediaFileSetId].SetString(0, value);
			}
		}

		/// <summary>
		/// Gets or sets the value of StorageMediaFileSetUid in the underlying collection. Type 3.
		/// </summary>
		public string StorageMediaFileSetUid
		{
			get { return base.DicomAttributeProvider[DicomTags.StorageMediaFileSetUid].GetString(0, string.Empty); }
			set
			{
				if (string.IsNullOrEmpty(value))
				{
					base.DicomAttributeProvider[DicomTags.StorageMediaFileSetUid] = null;
					return;
				}
				base.DicomAttributeProvider[DicomTags.StorageMediaFileSetUid].SetString(0, value);
			}
		}

		/// <summary>
		/// Gets or sets the value of ReferencedSopSequence in the underlying collection. Type 1.
		/// </summary>
		public IReferencedSopSequence[] ReferencedSopSequence
		{
			get
			{
				DicomAttribute dicomAttribute = base.DicomAttributeProvider[DicomTags.ReferencedSopSequence];
				if (dicomAttribute.IsNull || dicomAttribute.Count == 0)
					return null;

				IReferencedSopSequence[] result = new IReferencedSopSequence[dicomAttribute.Count];
				DicomSequenceItem[] items = (DicomSequenceItem[]) dicomAttribute.Values;
				for (int n = 0; n < items.Length; n++)
					result[n] = new ReferencedSopSequenceType(items[n]);

				return result;
			}
			set
			{
				if (value == null || value.Length == 0)
					throw new ArgumentNullException("value", "ReferencedSopSequence is Type 1 Required.");

				DicomSequenceItem[] result = new DicomSequenceItem[value.Length];
				for (int n = 0; n < value.Length; n++)
					result[n] = value[n].DicomSequenceItem;

				base.DicomAttributeProvider[DicomTags.ReferencedSopSequence].Values = result;
			}
		}

		/// <summary>
		/// Creates a single instance of a ReferencedSopSequence item. Does not modify the ReferencedSopSequence in the underlying collection.
		/// </summary>
		public IReferencedSopSequence CreateReferencedSopSequence()
		{
			IReferencedSopSequence iodBase = new ReferencedSopSequenceType(new DicomSequenceItem());
			iodBase.InitializeAttributes();
			return iodBase;
		}

		/// <summary>
		/// ReferencedSop Sequence Base Implementation
		/// </summary>
		/// <remarks>As defined in the DICOM Standard 2008, Part 3, Section C.17.2.1 (Table C.17-3a)</remarks>
		internal class ReferencedSopSequenceType : SopInstanceReferenceMacro, IReferencedSopSequence
		{
			/// <summary>
			/// Initializes a new instance of the <see cref="ReferencedSopSequence"/> class.
			/// </summary>
			public ReferencedSopSequenceType() : base() {}

			/// <summary>
			/// Initializes a new instance of the <see cref="ReferencedSopSequence"/> class.
			/// </summary>
			/// <param name="dicomSequenceItem">The dicom sequence item.</param>
			public ReferencedSopSequenceType(DicomSequenceItem dicomSequenceItem) : base(dicomSequenceItem) {}

			/// <summary>
			/// Initializes the underlying collection to implement the module using default values.
			/// </summary>
			public override void InitializeAttributes()
			{
				base.InitializeAttributes();
				this.PurposeOfReferenceCodeSequence = null;
				this.ReferencedDigitalSignatureSequence = null;
				this.ReferencedSopInstanceMacSequence = null;
			}

			/// <summary>
			/// Gets or sets the value of PurposeOfReferenceCodeSequence in the underlying collection. Type 3.
			/// </summary>
			public CodeSequenceMacro[] PurposeOfReferenceCodeSequence
			{
				get
				{
					DicomAttribute dicomAttribute = base.DicomAttributeProvider[DicomTags.PurposeOfReferenceCodeSequence];
					if (dicomAttribute.IsNull || dicomAttribute.Count == 0)
					{
						return null;
					}

					CodeSequenceMacro[] result = new CodeSequenceMacro[dicomAttribute.Count];
					DicomSequenceItem[] items = (DicomSequenceItem[]) dicomAttribute.Values;
					for (int n = 0; n < items.Length; n++)
						result[n] = new CodeSequenceMacro(items[n]);

					return result;
				}
				set
				{
					if (value == null || value.Length == 0)
					{
						base.DicomAttributeProvider[DicomTags.PurposeOfReferenceCodeSequence] = null;
						return;
					}

					DicomSequenceItem[] result = new DicomSequenceItem[value.Length];
					for (int n = 0; n < value.Length; n++)
						result[n] = value[n].DicomSequenceItem;

					base.DicomAttributeProvider[DicomTags.PurposeOfReferenceCodeSequence].Values = result;
				}
			}

			/// <summary>
			/// Gets or sets the value of ReferencedDigitalSignatureSequence in the underlying collection. Type 3.
			/// </summary>
			public ReferencedDigitalSignatureSequence[] ReferencedDigitalSignatureSequence
			{
				get
				{
					DicomAttribute dicomAttribute = base.DicomAttributeProvider[DicomTags.ReferencedDigitalSignatureSequence];
					if (dicomAttribute.IsNull || dicomAttribute.Count == 0)
					{
						return null;
					}

					ReferencedDigitalSignatureSequence[] result = new ReferencedDigitalSignatureSequence[dicomAttribute.Count];
					DicomSequenceItem[] items = (DicomSequenceItem[]) dicomAttribute.Values;
					for (int n = 0; n < items.Length; n++)
						result[n] = new ReferencedDigitalSignatureSequence(items[n]);

					return result;
				}
				set
				{
					if (value == null || value.Length == 0)
					{
						base.DicomAttributeProvider[DicomTags.ReferencedDigitalSignatureSequence] = null;
						return;
					}

					DicomSequenceItem[] result = new DicomSequenceItem[value.Length];
					for (int n = 0; n < value.Length; n++)
						result[n] = value[n].DicomSequenceItem;

					base.DicomAttributeProvider[DicomTags.ReferencedDigitalSignatureSequence].Values = result;
				}
			}

			/// <summary>
			/// Gets or sets the value of ReferencedSopInstanceMacSequence in the underlying collection. Type 3.
			/// </summary>
			public ReferencedSopInstanceMacSequence ReferencedSopInstanceMacSequence
			{
				get
				{
					DicomAttribute dicomAttribute = base.DicomAttributeProvider[DicomTags.ReferencedSopInstanceMacSequence];
					if (dicomAttribute.IsNull || dicomAttribute.Count == 0)
					{
						return null;
					}
					return new ReferencedSopInstanceMacSequence(((DicomSequenceItem[]) dicomAttribute.Values)[0]);
				}
				set
				{
					DicomAttribute dicomAttribute = base.DicomAttributeProvider[DicomTags.ReferencedSopInstanceMacSequence];
					if (value == null)
					{
						base.DicomAttributeProvider[DicomTags.ReferencedSopInstanceMacSequence] = null;
						return;
					}
					dicomAttribute.Values = new DicomSequenceItem[] {value.DicomSequenceItem};
				}
			}
		}
	}
}