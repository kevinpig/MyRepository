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
	/// PetIsotope Module
	/// </summary>
	/// <remarks>As defined in the DICOM Standard 2011, Part 3, Section C.8.9.2 (Table C.8-61)</remarks>
	public class PetIsotopeModuleIod : IodBase
	{
		/// <summary>
		/// Initializes a new instance of the <see cref="PetIsotopeModuleIod"/> class.
		/// </summary>	
		public PetIsotopeModuleIod() {}

		/// <summary>
		/// Initializes a new instance of the <see cref="PetIsotopeModuleIod"/> class.
		/// </summary>
		/// <param name="dicomAttributeProvider">The DICOM attribute collection.</param>
		public PetIsotopeModuleIod(IDicomAttributeProvider dicomAttributeProvider)
			: base(dicomAttributeProvider) {}

		/// <summary>
		/// Gets an enumeration of <see cref="DicomTag"/>s used by this module.
		/// </summary>
		public static IEnumerable<uint> DefinedTags
		{
			get
			{
				yield return DicomTags.RadiopharmaceuticalInformationSequence;
				yield return DicomTags.InterventionDrugInformationSequence;
			}
		}

		/// <summary>
		/// Initializes the underlying collection to implement the module or sequence using default values.
		/// </summary>
		public void InitializeAttributes()
		{
			RadiopharmaceuticalInformationSequence = null;
			InterventionDrugInformationSequence = null;
		}

		/// <summary>
		/// Checks if this module appears to be non-empty.
		/// </summary>
		/// <returns>True if the module appears to be non-empty; False otherwise.</returns>
		public bool HasValues()
		{
			return !(IsNullOrEmpty(RadiopharmaceuticalInformationSequence)
			         && IsNullOrEmpty(InterventionDrugInformationSequence));
		}

		/// <summary>
		/// NOT IMPLEMENTED. Gets or sets the value of RadiopharmaceuticalInformationSequence in the underlying collection. Type 2.
		/// </summary> 		
		public object RadiopharmaceuticalInformationSequence
		{
			// TODO - Implement this.
			get { throw new NotImplementedException(); }
			set { throw new NotImplementedException(); }
		}

		/// <summary>
		/// NOT IMPLEMENTED. Gets or sets the value of InterventionDrugInformationSequence in the underlying collection. Type 3.
		/// </summary> 		
		public object InterventionDrugInformationSequence
		{
			// TODO - Implement this.
			get { throw new NotImplementedException(); }
			set { throw new NotImplementedException(); }
		}
	}
}