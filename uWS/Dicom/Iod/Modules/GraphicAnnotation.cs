#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;
using System.Collections.Generic;
using uWS.Dicom.Iod.Sequences;

namespace uWS.Dicom.Iod.Modules
{
	/// <summary>
	/// GraphicAnnotation Module
	/// </summary>
	/// <remarks>As defined in the DICOM Standard 2008, Part 3, Section C.10.5 (Table C.10-5)</remarks>
	public class GraphicAnnotationModuleIod : IodBase
	{
		/// <summary>
		/// Initializes a new instance of the <see cref="GraphicAnnotationModuleIod"/> class.
		/// </summary>	
		public GraphicAnnotationModuleIod() : base() {}

		/// <summary>
		/// Initializes a new instance of the <see cref="GraphicAnnotationModuleIod"/> class.
		/// </summary>
		public GraphicAnnotationModuleIod(IDicomAttributeProvider dicomAttributeProvider) : base(dicomAttributeProvider) {}

		/// <summary>
		/// Gets or sets the value of GraphicAnnotationSequence in the underlying collection. Type 1.
		/// </summary>
		public GraphicAnnotationSequenceItem[] GraphicAnnotationSequence
		{
			get
			{
				DicomAttribute dicomAttribute = base.DicomAttributeProvider[DicomTags.GraphicAnnotationSequence];
				if (dicomAttribute.IsNull || dicomAttribute.Count == 0)
					return null;

				GraphicAnnotationSequenceItem[] result = new GraphicAnnotationSequenceItem[dicomAttribute.Count];
				DicomSequenceItem[] items = (DicomSequenceItem[]) dicomAttribute.Values;
				for (int n = 0; n < items.Length; n++)
					result[n] = new GraphicAnnotationSequenceItem(items[n]);

				return result;
			}
			set
			{
				if (value == null || value.Length == 0)
					throw new ArgumentNullException("value", "GraphicAnnotationSequence is Type 1 Required.");

				DicomSequenceItem[] result = new DicomSequenceItem[value.Length];
				for (int n = 0; n < value.Length; n++)
					result[n] = value[n].DicomSequenceItem;

				base.DicomAttributeProvider[DicomTags.GraphicAnnotationSequence].Values = result;
			}
		}

		/// <summary>
		/// Gets an enumeration of <see cref="DicomTag"/>s used by this module.
		/// </summary>
		public static IEnumerable<uint> DefinedTags {
			get {
				yield return DicomTags.GraphicAnnotationSequence;
			}
		}
	}
}