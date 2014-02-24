#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;

namespace uWS.Dicom.Iod.Sequences
{
	/// <summary>
	/// Referenced Series Sequence.  
	/// </summary>
	/// <remarks>As per Part 3, Table 10.4, pg 78</remarks>
	public class RequestAttributesSequenceIod : SequenceIodBase
	{
		#region Constructors
		/// <summary>
		/// Initializes a new instance of the <see cref="RequestAttributesSequenceIod"/> class.
		/// </summary>
		public RequestAttributesSequenceIod()
		{
		}

		/// <summary>
		/// Initializes a new instance of the <see cref="RequestAttributesSequenceIod"/> class.
		/// </summary>
		/// <param name="dicomSequenceItem">The dicom sequence item.</param>
		public RequestAttributesSequenceIod(DicomSequenceItem dicomSequenceItem)
			: base(dicomSequenceItem)
		{
		}
		#endregion

		#region Public Properties

		/// <summary>
		/// Requested Procedure Id.
		/// </summary>
		/// <value>The requested procedure Id.</value>
		public string RequestedProcedureId
		{
			get { return DicomAttributeProvider[DicomTags.RequestedProcedureId].GetString(0, String.Empty); }
			set { DicomAttributeProvider[DicomTags.RequestedProcedureId].SetString(0, value); }
		}

		/// <summary>
		/// Scheduled Procedure Step Id.
		/// </summary>
		/// <value>The scheduled procedure step Id.</value>
		public string ScheduledProcedureStepId
		{
			get { return DicomAttributeProvider[DicomTags.ScheduledProcedureStepId].GetString(0, String.Empty); }
			set { DicomAttributeProvider[DicomTags.ScheduledProcedureStepId].SetString(0, value); }
		}
		#endregion
	}

}
