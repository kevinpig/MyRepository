#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;
using System.Collections.Generic;

namespace uWS.Dicom.Iod.Modules
{
	/// <summary>
	/// ScEquipment Module
	/// </summary>
	/// <remarks>
	/// <para>As defined in the DICOM Standard 2009, Part 3, Section C.8.6.1 (Table C.8-24)</para>
	/// </remarks>
	public class ScEquipmentModuleIod
		: IodBase
	{
		/// <summary>
		/// Initializes a new instance of the <see cref="ScEquipmentModuleIod"/> class.
		/// </summary>	
		public ScEquipmentModuleIod() {}

		/// <summary>
		/// Initializes a new instance of the <see cref="ScEquipmentModuleIod"/> class.
		/// </summary>
		/// <param name="dicomAttributeProvider">The DICOM attribute provider.</param>
		public ScEquipmentModuleIod(IDicomAttributeProvider dicomAttributeProvider)
			: base(dicomAttributeProvider) {}

		/// <summary>
		/// Gets or sets the value of ConversionType in the underlying collection. Type 1.
		/// </summary>
		public string ConversionType
		{
			get { return DicomAttributeProvider[DicomTags.ConversionType].ToString(); }
			set
			{
				if (string.IsNullOrEmpty(value))
					throw new ArgumentNullException("value", "ConversionType is Type 1 Required.");
				DicomAttributeProvider[DicomTags.ConversionType].SetStringValue(value);
			}
		}

		/// <summary>
		/// Gets or sets the value of Modality in the underlying collection. Type 3.
		/// </summary>
		public string Modality
		{
			get { return DicomAttributeProvider[DicomTags.Modality].ToString(); }
			set
			{
				if (string.IsNullOrEmpty(value))
				{
					DicomAttributeProvider[DicomTags.Modality] = null;
					return;
				}
				DicomAttributeProvider[DicomTags.Modality].SetStringValue(value);
			}
		}

		/// <summary>
		/// Gets or sets the value of SecondaryCaptureDeviceId in the underlying collection. Type 3.
		/// </summary>
		public string SecondaryCaptureDeviceId
		{
			get { return DicomAttributeProvider[DicomTags.SecondaryCaptureDeviceId].ToString(); }
			set
			{
				if (string.IsNullOrEmpty(value))
				{
					DicomAttributeProvider[DicomTags.SecondaryCaptureDeviceId] = null;
					return;
				}
				DicomAttributeProvider[DicomTags.SecondaryCaptureDeviceId].SetStringValue(value);
			}
		}

		/// <summary>
		/// Gets or sets the value of SecondaryCaptureDeviceManufacturer in the underlying collection. Type 3.
		/// </summary>
		public string SecondaryCaptureDeviceManufacturer
		{
			get { return DicomAttributeProvider[DicomTags.SecondaryCaptureDeviceManufacturer].ToString(); }
			set
			{
				if (string.IsNullOrEmpty(value))
				{
					DicomAttributeProvider[DicomTags.SecondaryCaptureDeviceManufacturer] = null;
					return;
				}
				DicomAttributeProvider[DicomTags.SecondaryCaptureDeviceManufacturer].SetStringValue(value);
			}
		}

		/// <summary>
		/// Gets or sets the value of SecondaryCaptureDeviceManufacturersModelName in the underlying collection. Type 3.
		/// </summary>
		public string SecondaryCaptureDeviceManufacturersModelName
		{
			get { return DicomAttributeProvider[DicomTags.SecondaryCaptureDeviceManufacturersModelName].ToString(); }
			set
			{
				if (string.IsNullOrEmpty(value))
				{
					DicomAttributeProvider[DicomTags.SecondaryCaptureDeviceManufacturersModelName] = null;
					return;
				}
				DicomAttributeProvider[DicomTags.SecondaryCaptureDeviceManufacturersModelName].SetStringValue(value);
			}
		}

		/// <summary>
		/// Gets or sets the value of SecondaryCaptureDeviceSoftwareVersions in the underlying collection. Type 3.
		/// </summary>
		public string[] SecondaryCaptureDeviceSoftwareVersions
		{
			get
			{
				var dicomAttribute = DicomAttributeProvider[DicomTags.SecondaryCaptureDeviceSoftwareVersions];
				if (dicomAttribute.IsNull || dicomAttribute.IsEmpty)
					return null;

				var result = new string[dicomAttribute.Count];
				for (var n = 0; n < result.Length; n++)
					result[n] = dicomAttribute.GetString(n, string.Empty);
				return result;
			}
			set
			{
				if (value == null || value.Length == 0)
				{
					DicomAttributeProvider[DicomTags.SecondaryCaptureDeviceSoftwareVersions] = null;
					return;
				}

				var dicomAttribute = DicomAttributeProvider[DicomTags.SecondaryCaptureDeviceSoftwareVersions];
				for (var n = 0; n < value.Length; n++)
					dicomAttribute.SetString(n, value[n]);
			}
		}

		/// <summary>
		/// Gets or sets the value of VideoImageFormatAcquired in the underlying collection. Type 3.
		/// </summary>
		public string VideoImageFormatAcquired
		{
			get { return DicomAttributeProvider[DicomTags.VideoImageFormatAcquired].ToString(); }
			set
			{
				if (string.IsNullOrEmpty(value))
				{
					DicomAttributeProvider[DicomTags.VideoImageFormatAcquired] = null;
					return;
				}
				DicomAttributeProvider[DicomTags.VideoImageFormatAcquired].SetStringValue(value);
			}
		}

		/// <summary>
		/// Gets or sets the value of DigitalImageFormatAcquired in the underlying collection. Type 3.
		/// </summary>
		public string DigitalImageFormatAcquired
		{
			get { return DicomAttributeProvider[DicomTags.DigitalImageFormatAcquired].ToString(); }
			set
			{
				if (string.IsNullOrEmpty(value))
				{
					DicomAttributeProvider[DicomTags.DigitalImageFormatAcquired] = null;
					return;
				}
				DicomAttributeProvider[DicomTags.DigitalImageFormatAcquired].SetStringValue(value);
			}
		}

		/// <summary>
		/// Initializes the attributes in this module to their default values.
		/// </summary>
		public void InitializeAttributes()
		{
			ConversionType = ' '.ToString();
		}

		/// <summary>
		/// Gets an enumeration of <see cref="uWS.Dicom.DicomTag"/>s used by this module.
		/// </summary>
		public static IEnumerable<uint> DefinedTags
		{
			get
			{
				yield return DicomTags.ConversionType;
				yield return DicomTags.Modality;
				yield return DicomTags.SecondaryCaptureDeviceId;
				yield return DicomTags.SecondaryCaptureDeviceManufacturer;
				yield return DicomTags.SecondaryCaptureDeviceManufacturersModelName;
				yield return DicomTags.SecondaryCaptureDeviceSoftwareVersions;
				yield return DicomTags.VideoImageFormatAcquired;
				yield return DicomTags.DigitalImageFormatAcquired;
			}
		}
	}
}