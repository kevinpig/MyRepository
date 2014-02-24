#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System.Collections.Generic;
using uWS.Dicom.Iod.Macros;
using uWS.Dicom.Iod.Sequences;

namespace uWS.Dicom.Iod.Modules
{
	/// <summary>
	/// NM/PET Patient Orientation Module
	/// </summary>
	/// <remarks>As defined in the DICOM Standard 2011, Part 3, Section C.8.4.6 (Table C.8-5)</remarks>
	public class NmPetPatientOrientationModuleIod : IodBase
	{
		/// <summary>
		/// Initializes a new instance of the <see cref="NmPetPatientOrientationModuleIod"/> class.
		/// </summary>	
		public NmPetPatientOrientationModuleIod() {}

		/// <summary>
		/// Initializes a new instance of the <see cref="NmPetPatientOrientationModuleIod"/> class.
		/// </summary>
		/// <param name="dicomAttributeProvider">The DICOM attribute collection.</param>
		public NmPetPatientOrientationModuleIod(IDicomAttributeProvider dicomAttributeProvider)
			: base(dicomAttributeProvider) {}

		/// <summary>
		/// Gets an enumeration of <see cref="DicomTag"/>s used by this module.
		/// </summary>
		public static IEnumerable<uint> DefinedTags
		{
			get
			{
				yield return DicomTags.PatientOrientationCodeSequence;
				yield return DicomTags.PatientGantryRelationshipCodeSequence;
			}
		}

		/// <summary>
		/// Initializes the underlying collection to implement the module or sequence using default values.
		/// </summary>
		public void InitializeAttributes()
		{
			PatientOrientationCodeSequence = null;
			PatientGantryRelationshipCodeSequence = null;
		}

		/// <summary>
		/// Checks if this module appears to be non-empty.
		/// </summary>
		/// <returns>True if the module appears to be non-empty; False otherwise.</returns>
		public bool HasValues()
		{
			if (PatientOrientationCodeSequence == null
			    && PatientGantryRelationshipCodeSequence == null)
				return false;
			return true;
		}

		/// <summary>
		/// Gets or sets the value of PatientOrientationCodeSequence in the underlying collection. Type 2.
		/// </summary>
		public PatientOrientationCodeSequence PatientOrientationCodeSequence
		{
			get
			{
				var dicomAttribute = DicomAttributeProvider[DicomTags.PatientOrientationCodeSequence];
				if (dicomAttribute.IsNull || dicomAttribute.IsEmpty)
				{
					return null;
				}
				return new PatientOrientationCodeSequence(((DicomSequenceItem[]) dicomAttribute.Values)[0]);
			}
			set
			{
				var dicomAttribute = DicomAttributeProvider[DicomTags.PatientOrientationCodeSequence];
				if (value == null)
				{
					dicomAttribute.SetNullValue();
					return;
				}
				dicomAttribute.Values = new[] {value.DicomSequenceItem};
			}
		}

		/// <summary>
		/// Creates the PatientOrientationCodeSequence in the underlying collection. Type 2.
		/// </summary>
		public PatientOrientationCodeSequence CreatePatientOrientationCodeSequence()
		{
			var dicomAttribute = DicomAttributeProvider[DicomTags.PatientOrientationCodeSequence];
			if (dicomAttribute.IsNull || dicomAttribute.IsEmpty)
			{
				var dicomSequenceItem = new DicomSequenceItem();
				dicomAttribute.Values = new[] {dicomSequenceItem};
				var sequenceType = new PatientOrientationCodeSequence(dicomSequenceItem);
				return sequenceType;
			}
			return new PatientOrientationCodeSequence(((DicomSequenceItem[]) dicomAttribute.Values)[0]);
		}

		/// <summary>
		/// Gets or sets the value of PatientGantryRelationshipCodeSequence in the underlying collection. Type 2.
		/// </summary>
		public CodeSequenceMacro PatientGantryRelationshipCodeSequence
		{
			get
			{
				var dicomAttribute = DicomAttributeProvider[DicomTags.PatientGantryRelationshipCodeSequence];
				if (dicomAttribute.IsNull || dicomAttribute.IsEmpty)
				{
					return null;
				}
				return new CodeSequenceMacro(((DicomSequenceItem[]) dicomAttribute.Values)[0]);
			}
			set
			{
				var dicomAttribute = DicomAttributeProvider[DicomTags.PatientGantryRelationshipCodeSequence];
				if (value == null)
				{
					dicomAttribute.SetNullValue();
					return;
				}
				dicomAttribute.Values = new[] {value.DicomSequenceItem};
			}
		}

		/// <summary>
		/// Creates the PatientGantryRelationshipCodeSequence in the underlying collection. Type 2.
		/// </summary>
		public CodeSequenceMacro CreatePatientGantryRelationshipCodeSequence()
		{
			var dicomAttribute = DicomAttributeProvider[DicomTags.PatientGantryRelationshipCodeSequence];
			if (dicomAttribute.IsNull || dicomAttribute.IsEmpty)
			{
				var dicomSequenceItem = new DicomSequenceItem();
				dicomAttribute.Values = new[] {dicomSequenceItem};
				var sequenceType = new CodeSequenceMacro(dicomSequenceItem);
				return sequenceType;
			}
			return new CodeSequenceMacro(((DicomSequenceItem[]) dicomAttribute.Values)[0]);
		}
	}
}