#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;
using System.Collections.Generic;
using uWS.Dicom.Iod.Macros;
using uWS.Dicom.Utilities;

namespace uWS.Dicom.Iod.Modules
{
	/// <summary>
	/// PresentationSeries Module
	/// </summary>
	/// <remarks>As defined in the DICOM Standard 2008, Part 3, Section C.11.9 (Table C.11.9-1)</remarks>
	public class PresentationSeriesModuleIod : IodBase
	{
		/// <summary>
		/// Initializes a new instance of the <see cref="PresentationSeriesModuleIod"/> class.
		/// </summary>	
		public PresentationSeriesModuleIod() : base() {}

		/// <summary>
		/// Initializes a new instance of the <see cref="PresentationSeriesModuleIod"/> class.
		/// </summary>
		public PresentationSeriesModuleIod(IDicomAttributeProvider provider) : base(provider) {}

		/// <summary>
		/// Initializes the underlying collection to implement the module or sequence using default values.
		/// </summary>
		public void InitializeAttributes()
		{
			this.Modality = Modality.PR;
		}

		/// <summary>
		/// Gets or sets the value of Modality in the underlying collection. Type 1.
		/// </summary>
		public Modality Modality
		{
			get { return ParseEnum(base.DicomAttributeProvider[DicomTags.Modality].GetString(0, string.Empty), Modality.None); }
			set
			{
				if (value != Modality.PR)
					throw new ArgumentOutOfRangeException("value", "Modality must be PR.");
				SetAttributeFromEnum(base.DicomAttributeProvider[DicomTags.Modality], value);
			}
		}

		/// <summary>
		/// Gets an enumeration of <see cref="DicomTag"/>s used by this module.
		/// </summary>
		public static IEnumerable<uint> DefinedTags {
			get {
				yield return DicomTags.Modality;
			}
		}
	}
}