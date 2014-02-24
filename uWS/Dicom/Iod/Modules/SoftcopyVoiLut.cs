#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;
using System.Collections.Generic;
using uWS.Dicom.Iod.Macros;
using uWS.Dicom.Iod.Macros.VoiLut;
using uWS.Dicom.Iod.Sequences;

namespace uWS.Dicom.Iod.Modules
{
	/// <summary>
	/// SoftcopyVoiLut Module
	/// </summary>
	/// <remarks>As defined in the DICOM Standard 2008, Part 3, Section C.11.8 (Table C.11.8-1)</remarks>
	public class SoftcopyVoiLutModuleIod : IodBase
	{
		/// <summary>
		/// Initializes a new instance of the <see cref="SoftcopyVoiLutModuleIod"/> class.
		/// </summary>	
		public SoftcopyVoiLutModuleIod() : base() {}

		/// <summary>
		/// Initializes a new instance of the <see cref="SoftcopyVoiLutModuleIod"/> class.
		/// </summary>
		public SoftcopyVoiLutModuleIod(IDicomAttributeProvider dicomAttributeProvider) : base(dicomAttributeProvider) { }

		/// <summary>
		/// Gets or sets the value of SoftcopyVoiLutSequence in the underlying collection. Type 1.
		/// </summary>
		public SoftcopyVoiLutSequenceItem[] SoftcopyVoiLutSequence
		{
			get
			{
				DicomAttribute dicomAttribute = base.DicomAttributeProvider[DicomTags.SoftcopyVoiLutSequence];
				if (dicomAttribute.IsNull || dicomAttribute.Count == 0)
					return null;

				SoftcopyVoiLutSequenceItem[] result = new SoftcopyVoiLutSequenceItem[dicomAttribute.Count];
				DicomSequenceItem[] items = (DicomSequenceItem[]) dicomAttribute.Values;
				for (int n = 0; n < items.Length; n++)
					result[n] = new SoftcopyVoiLutSequenceItem(items[n]);

				return result;
			}
			set
			{
				if (value == null || value.Length == 0)
					throw new ArgumentNullException("value", "SoftcopyVoiLutSequence is Type 1 Required.");

				DicomSequenceItem[] result = new DicomSequenceItem[value.Length];
				for (int n = 0; n < value.Length; n++)
					result[n] = value[n].DicomSequenceItem;

				base.DicomAttributeProvider[DicomTags.SoftcopyVoiLutSequence].Values = result;
			}
		}

		/// <summary>
		/// SoftcopyVoiLut Sequence
		/// </summary>
		/// <remarks>As defined in the DICOM Standard 2008, Part 3, Section C.11.8 (Table C.11-8)</remarks>
		public class SoftcopyVoiLutSequenceItem : SequenceIodBase, IVoiLutMacro {
			/// <summary>
			/// Initializes a new instance of the <see cref="SoftcopyVoiLutSequenceItem"/> class.
			/// </summary>
			public SoftcopyVoiLutSequenceItem() : base() { }

			/// <summary>
			/// Initializes a new instance of the <see cref="SoftcopyVoiLutSequenceItem"/> class.
			/// </summary>
			/// <param name="dicomSequenceItem">The dicom sequence item.</param>
			public SoftcopyVoiLutSequenceItem(DicomSequenceItem dicomSequenceItem) : base(dicomSequenceItem) { }

			public void InitializeAttributes() { }

			/// <summary>
			/// Gets or sets the value of ReferencedImageSequence in the underlying collection. Type 1C.
			/// </summary>
			public ImageSopInstanceReferenceMacro[] ReferencedImageSequence
			{
				get
				{
					DicomAttribute dicomAttribute = base.DicomAttributeProvider[DicomTags.ReferencedImageSequence];
					if (dicomAttribute.IsNull || dicomAttribute.Count == 0)
					{
						return null;
					}

					ImageSopInstanceReferenceMacro[] result = new ImageSopInstanceReferenceMacro[dicomAttribute.Count];
					DicomSequenceItem[] items = (DicomSequenceItem[]) dicomAttribute.Values;
					for (int n = 0; n < items.Length; n++)
						result[n] = new ImageSopInstanceReferenceMacro(items[n]);

					return result;
				}
				set
				{
					if (value == null || value.Length == 0)
					{
						base.DicomAttributeProvider[DicomTags.ReferencedImageSequence] = null;
						return;
					}

					DicomSequenceItem[] result = new DicomSequenceItem[value.Length];
					for (int n = 0; n < value.Length; n++)
						result[n] = value[n].DicomSequenceItem;

					base.DicomAttributeProvider[DicomTags.ReferencedImageSequence].Values = result;
				}
			}

			/// <summary>
			/// Gets or sets the value of VoiLutSequence in the underlying collection. Type 1C.
			/// </summary>
			public VoiLutSequenceItem[] VoiLutSequence {
				get
				{
					DicomAttribute dicomAttribute = base.DicomAttributeProvider[DicomTags.VoiLutSequence];
					if (dicomAttribute.IsNull || dicomAttribute.Count == 0)
					{
						return null;
					}

					VoiLutSequenceItem[] result = new VoiLutSequenceItem[dicomAttribute.Count];
					DicomSequenceItem[] items = (DicomSequenceItem[]) dicomAttribute.Values;
					for (int n = 0; n < items.Length; n++)
						result[n] = new VoiLutSequenceItem(items[n]);

					return result;
				}
				set
				{
					if (value == null || value.Length == 0)
					{
						base.DicomAttributeProvider[DicomTags.VoiLutSequence] = null;
						return;
					}

					DicomSequenceItem[] result = new DicomSequenceItem[value.Length];
					for (int n = 0; n < value.Length; n++)
						result[n] = value[n].DicomSequenceItem;

					base.DicomAttributeProvider[DicomTags.VoiLutSequence].Values = result;
				}
			}

			/// <summary>
			/// Gets or sets the value of WindowCenter in the underlying collection. Type 1C.
			/// </summary>
			public double[] WindowCenter
			{
				get
				{
					DicomAttribute attribute = base.DicomAttributeProvider[DicomTags.WindowCenter];
					if (attribute.IsNull || attribute.IsEmpty || attribute.Count == 0)
						return null;

					double[] values = new double[attribute.Count];
					for (int n = 0; n < attribute.Count; n++)
						values[n] = attribute.GetFloat64(n, 0);
					return values;
				}
				set
				{
					if (value == null || value.Length == 0)
					{
						base.DicomAttributeProvider[DicomTags.WindowCenter] = null;
						return;
					}

					DicomAttribute attribute = base.DicomAttributeProvider[DicomTags.WindowCenter];
					for (int n = value.Length - 1; n >= 0; n--)
						attribute.SetFloat64(n, value[n]);
				}
			}

			/// <summary>
			/// Gets or sets the value of WindowWidth in the underlying collection. Type 1C.
			/// </summary>
			public double[] WindowWidth
			{
				get
				{
					DicomAttribute attribute = base.DicomAttributeProvider[DicomTags.WindowWidth];
					if (attribute.IsNull || attribute.IsEmpty || attribute.Count == 0)
						return null;

					double[] values = new double[attribute.Count];
					for (int n = 0; n < attribute.Count; n++)
						values[n] = attribute.GetFloat64(n, 0);
					return values;
				}
				set
				{
					if (value == null || value.Length == 0)
					{
						base.DicomAttributeProvider[DicomTags.WindowWidth] = null;
						return;
					}

					DicomAttribute attribute = base.DicomAttributeProvider[DicomTags.WindowWidth];
					for (int n = value.Length - 1; n >= 0; n--)
						attribute.SetFloat64(n, value[n]);
				}
			}

			/// <summary>
			/// Gets or sets the value of WindowCenterWidthExplanation in the underlying collection. Type 3.
			/// </summary>
			public string[] WindowCenterWidthExplanation
			{
				get
				{
					DicomAttribute attribute = base.DicomAttributeProvider[DicomTags.WindowCenterWidthExplanation];
					if (attribute.IsNull || attribute.IsEmpty || attribute.Count == 0)
						return null;
					return (string[]) attribute.Values;
				}
				set
				{
					if (value == null || value.Length == 0)
					{
						base.DicomAttributeProvider[DicomTags.WindowCenterWidthExplanation] = null;
						return;
					}
					base.DicomAttributeProvider[DicomTags.WindowCenterWidthExplanation].Values = value;
				}
			}

			/// <summary>
			/// Gets or sets the value of VoiLutFunction in the underlying collection. Type 3.
			/// </summary>
			public VoiLutFunction VoiLutFunction
			{
				get { return ParseEnum(base.DicomAttributeProvider[DicomTags.VoiLutFunction].GetString(0, string.Empty), VoiLutFunction.None); }
				set
				{
					if (value == VoiLutFunction.None)
					{
						base.DicomAttributeProvider[DicomTags.VoiLutFunction] = null;
						return;
					}
					SetAttributeFromEnum(base.DicomAttributeProvider[DicomTags.VoiLutFunction], value);
				}
			}

			/// <summary>
			/// Gets the number of VOI Data LUTs included in this sequence.
			/// </summary>
			public long CountDataLuts
			{
				get
				{
					DicomAttribute attribute = base.DicomAttributeProvider[DicomTags.VoiLutSequence];
					if (attribute.IsNull || attribute.IsEmpty)
						return 0;
					return attribute.Count;
				}
			}

			/// <summary>
			/// Gets the number of VOI Windows included in this sequence.
			/// </summary>
			public long CountWindows
			{
				get
				{
					DicomAttribute attribute = base.DicomAttributeProvider[DicomTags.WindowCenter];
					if (attribute.IsNull || attribute.IsEmpty)
						return 0;
					return attribute.Count;
				}
			}
		}

		/// <summary>
		/// Gets an enumeration of <see cref="DicomTag"/>s used by this module.
		/// </summary>
		public static IEnumerable<uint> DefinedTags {
			get {
				yield return DicomTags.SoftcopyVoiLutSequence;
			}
		}
	}
}
