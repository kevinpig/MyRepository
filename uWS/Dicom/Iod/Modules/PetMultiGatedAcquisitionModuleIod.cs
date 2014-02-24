#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System.Collections.Generic;

namespace uWS.Dicom.Iod.Modules
{
	/// <summary>
	/// PET Multi Gated Acquisition Module
	/// </summary>
	/// <remarks>As defined in the DICOM Standard 2011, Part 3, Section C.8.9.3 (Table C.8-62)</remarks>
	public class PetMultiGatedAcquisitionModuleIod : IodBase
	{
		/// <summary>
		/// Initializes a new instance of the <see cref="PetMultiGatedAcquisitionModuleIod"/> class.
		/// </summary>	
		public PetMultiGatedAcquisitionModuleIod() {}

		/// <summary>
		/// Initializes a new instance of the <see cref="PetMultiGatedAcquisitionModuleIod"/> class.
		/// </summary>
		/// <param name="dicomAttributeProvider">The DICOM attribute collection.</param>
		public PetMultiGatedAcquisitionModuleIod(IDicomAttributeProvider dicomAttributeProvider)
			: base(dicomAttributeProvider) {}

		/// <summary>
		/// Gets an enumeration of <see cref="DicomTag"/>s used by this module.
		/// </summary>
		public static IEnumerable<uint> DefinedTags
		{
			get
			{
				yield return DicomTags.BeatRejectionFlag;
				yield return DicomTags.TriggerSourceOrType;
				yield return DicomTags.PvcRejection;
				yield return DicomTags.SkipBeats;
				yield return DicomTags.HeartRate;
				yield return DicomTags.CardiacFramingType;
			}
		}

		/// <summary>
		/// Initializes the underlying collection to implement the module or sequence using default values.
		/// </summary>
		public void InitializeAttributes()
		{
			BeatRejectionFlag = null;
			TriggerSourceOrType = null;
			PvcRejection = null;
			SkipBeats = null;
			HeartRate = null;
			CardiacFramingType = null;
		}

		/// <summary>
		/// Checks if this module appears to be non-empty.
		/// </summary>
		/// <returns>True if the module appears to be non-empty; False otherwise.</returns>
		public bool HasValues()
		{
			return !(IsNullOrEmpty(BeatRejectionFlag)
			         && IsNullOrEmpty(TriggerSourceOrType)
			         && IsNullOrEmpty(PvcRejection)
			         && IsNullOrEmpty(SkipBeats)
			         && IsNullOrEmpty(HeartRate)
			         && IsNullOrEmpty(CardiacFramingType));
		}

		/// <summary>
		/// Gets or sets the value of BeatRejectionFlag in the underlying collection. Type 2.
		/// </summary>
		public string BeatRejectionFlag
		{
			get { return DicomAttributeProvider[DicomTags.BeatRejectionFlag].GetString(0, string.Empty); }
			set
			{
				if (string.IsNullOrEmpty(value))
				{
					DicomAttributeProvider[DicomTags.BeatRejectionFlag].SetNullValue();
					return;
				}
				DicomAttributeProvider[DicomTags.BeatRejectionFlag].SetString(0, value);
			}
		}

		/// <summary>
		/// Gets or sets the value of TriggerSourceOrType in the underlying collection. Type 3.
		/// </summary>
		public string TriggerSourceOrType
		{
			get { return DicomAttributeProvider[DicomTags.TriggerSourceOrType].GetString(0, string.Empty); }
			set
			{
				if (string.IsNullOrEmpty(value))
				{
					DicomAttributeProvider[DicomTags.TriggerSourceOrType] = null;
					return;
				}
				DicomAttributeProvider[DicomTags.TriggerSourceOrType].SetString(0, value);
			}
		}

		/// <summary>
		/// Gets or sets the value of PvcRejection in the underlying collection. Type 3.
		/// </summary>
		public string PvcRejection
		{
			get { return DicomAttributeProvider[DicomTags.PvcRejection].GetString(0, string.Empty); }
			set
			{
				if (string.IsNullOrEmpty(value))
				{
					DicomAttributeProvider[DicomTags.PvcRejection] = null;
					return;
				}
				DicomAttributeProvider[DicomTags.PvcRejection].SetString(0, value);
			}
		}

		/// <summary>
		/// Gets or sets the value of SkipBeats in the underlying collection. Type 3.
		/// </summary>
		public int? SkipBeats
		{
			get
			{
				int result;
				if (DicomAttributeProvider[DicomTags.SkipBeats].TryGetInt32(0, out result))
					return result;
				return null;
			}
			set
			{
				if (!value.HasValue)
				{
					DicomAttributeProvider[DicomTags.SkipBeats] = null;
					return;
				}
				DicomAttributeProvider[DicomTags.SkipBeats].SetInt32(0, value.Value);
			}
		}

		/// <summary>
		/// Gets or sets the value of HeartRate in the underlying collection. Type 3.
		/// </summary>
		public int? HeartRate
		{
			get
			{
				int result;
				if (DicomAttributeProvider[DicomTags.HeartRate].TryGetInt32(0, out result))
					return result;
				return null;
			}
			set
			{
				if (!value.HasValue)
				{
					DicomAttributeProvider[DicomTags.HeartRate] = null;
					return;
				}
				DicomAttributeProvider[DicomTags.HeartRate].SetInt32(0, value.Value);
			}
		}

		/// <summary>
		/// Gets or sets the value of CardiacFramingType in the underlying collection. Type 3.
		/// </summary>
		public string CardiacFramingType
		{
			get { return DicomAttributeProvider[DicomTags.CardiacFramingType].GetString(0, string.Empty); }
			set
			{
				if (string.IsNullOrEmpty(value))
				{
					DicomAttributeProvider[DicomTags.CardiacFramingType] = null;
					return;
				}
				DicomAttributeProvider[DicomTags.CardiacFramingType].SetString(0, value);
			}
		}
	}
}