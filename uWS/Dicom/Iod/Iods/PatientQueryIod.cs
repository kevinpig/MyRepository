#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;

namespace uWS.Dicom.Iod.Iods
{
    /// <summary>
    /// IOD for common Patient Query Retrieve items.
    /// </summary>
    public class PatientQueryIod : QueryIodBase
    {
        #region Constructors
        /// <summary>
        /// Initializes a new instance of the <see cref="PatientQueryIod"/> class.
        /// </summary>
        public PatientQueryIod()
        {
            SetAttributeFromEnum(DicomAttributeProvider[DicomTags.QueryRetrieveLevel], QueryRetrieveLevel.Patient);
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="PatientQueryIod"/> class.
        /// </summary>
		public PatientQueryIod(IDicomAttributeProvider dicomAttributeProvider)
            :base(dicomAttributeProvider)
        {
            SetAttributeFromEnum(DicomAttributeProvider[DicomTags.QueryRetrieveLevel], QueryRetrieveLevel.Patient);
        }
        #endregion

        #region Public Properties
        /// <summary>
        /// Gets or sets the patient id.
        /// </summary>
        /// <value>The patient id.</value>
        public string PatientId
        {
            get { return DicomAttributeProvider[DicomTags.PatientId].GetString(0, String.Empty); }
            set { DicomAttributeProvider[DicomTags.PatientId].SetString(0, value); }
        }

        /// <summary>
        /// Gets or sets the name of the patient.
        /// </summary>
        /// <value>The name of the patients.</value>
        public PersonName PatientsName
        {
            get { return new PersonName(DicomAttributeProvider[DicomTags.PatientsName].GetString(0, String.Empty)); }
            set { DicomAttributeProvider[DicomTags.PatientsName].SetString(0, value.ToString()); }
        }

        /// <summary>
        /// Gets or sets the patients birth date.
        /// </summary>
        /// <value>The patients birth date.</value>
        public DateTime PatientsBirthDate
        {
            get { return DicomAttributeProvider[DicomTags.PatientsBirthDate].GetDateTime(0, DateTime.MinValue); }
            set { DicomAttributeProvider[DicomTags.PatientsBirthDate].SetDateTime(0, value); }
        }

        /// <summary>
        /// Gets or sets the patients sex.
        /// </summary>
        /// <value>The patients sex.</value>
        public string PatientsSex
        {
            get { return DicomAttributeProvider[DicomTags.PatientsSex].GetString(0, String.Empty); }
            set { DicomAttributeProvider[DicomTags.PatientsSex].SetString(0, value); }
        }

		/// <summary>
		/// Gets or sets the number of patient related instances.
		/// </summary>
		/// <value>The number of patient related instances.</value>
		public uint NumberOfPatientRelatedInstances
		{
			get { return DicomAttributeProvider[DicomTags.NumberOfPatientRelatedInstances].GetUInt32(0, 0); }
			set { DicomAttributeProvider[DicomTags.NumberOfPatientRelatedInstances].SetUInt32(0, value); }
		}

		/// <summary>
		/// Gets or sets the number of patient related series.
		/// </summary>
		/// <value>The number of patient related series.</value>
		public uint NumberOfPatientRelatedSeries
		{
			get { return DicomAttributeProvider[DicomTags.NumberOfPatientRelatedSeries].GetUInt32(0, 0); }
			set { DicomAttributeProvider[DicomTags.NumberOfPatientRelatedSeries].SetUInt32(0, value); }
		}

		/// <summary>
		/// Gets or sets the number of patient related studies.
		/// </summary>
		/// <value>The number of patient related studies.</value>
		public uint NumberOfPatientRelatedStudies
		{
			get { return DicomAttributeProvider[DicomTags.NumberOfPatientRelatedStudies].GetUInt32(0, 0); }
			set { DicomAttributeProvider[DicomTags.NumberOfPatientRelatedStudies].SetUInt32(0, value); }
		}

        #endregion

        #region Public Methods
        /// <summary>
        /// Sets the common tags for a patient query retrieve request.
        /// </summary>
         public void SetCommonTags()
        {
            SetCommonTags(DicomAttributeProvider);
        }

        /// <summary>
        /// Sets the common tags for a patient query retrieve request.
        /// </summary>
        public static void SetCommonTags(IDicomAttributeProvider dicomAttributeProvider)
        {
			SetAttributeFromEnum(dicomAttributeProvider[DicomTags.QueryRetrieveLevel], QueryRetrieveLevel.Patient);

			// Always set the Patient 
			dicomAttributeProvider[DicomTags.PatientsName].SetString(0, "*");
			dicomAttributeProvider[DicomTags.PatientId].SetNullValue();
			dicomAttributeProvider[DicomTags.PatientsBirthDate].SetNullValue();
			dicomAttributeProvider[DicomTags.PatientsBirthTime].SetNullValue();
			dicomAttributeProvider[DicomTags.PatientsSex].SetNullValue();
			dicomAttributeProvider[DicomTags.NumberOfPatientRelatedStudies].SetNullValue();
			dicomAttributeProvider[DicomTags.NumberOfPatientRelatedSeries].SetNullValue();
			dicomAttributeProvider[DicomTags.NumberOfPatientRelatedInstances].SetNullValue();
		}
        #endregion
    }

}
