#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;

namespace uWS.Dicom.Iod.Sequences
{
    /// <summary>
    /// Exposure Dose Sequence.  
    /// </summary>
    /// <remarks>As per Part 3, Table C4.16, pg 259</remarks>
    public class ExposureDoseSequenceIod : SequenceIodBase
    {
        #region Constructors
        /// <summary>
        /// Initializes a new instance of the <see cref="ExposureDoseSequenceIod"/> class.
        /// </summary>
        public ExposureDoseSequenceIod()
            :base()
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="ExposureDoseSequenceIod"/> class.
        /// </summary>
        /// <param name="dicomSequenceItem">The dicom sequence item.</param>
        public ExposureDoseSequenceIod(DicomSequenceItem dicomSequenceItem)
            : base(dicomSequenceItem)
        {
        }
        #endregion

        #region Public Properties
        /// <summary>
        /// Gets or sets the x-ray radiation mode.
        /// </summary>
        /// <value>The radiation mode.</value>
        public RadiationMode RadiationMode
        {
            get { return IodBase.ParseEnum<RadiationMode>(base.DicomAttributeProvider[DicomTags.RadiationMode].GetString(0, String.Empty), RadiationMode.None); }
            set { IodBase.SetAttributeFromEnum(base.DicomAttributeProvider[DicomTags.RadiationMode], value); }
        }

        /// <summary>
        /// Peak kilo voltage output of the x-ray generator used. An average in the case of fluoroscopy (continuous radiation mode).
        /// </summary>
        /// <value>The KVP.</value>
        public float Kvp
        {
            get { return base.DicomAttributeProvider[DicomTags.Kvp].GetFloat32(0, 0.0F); }
            set { base.DicomAttributeProvider[DicomTags.Kvp].SetFloat32(0, value); }
        }

        /// <summary>
        /// X-ray Tube Current in μA. An average in the case of fluoroscopy (continuous radiation mode).
        /// </summary>
        /// <remarks>
        /// The name of this property is misleading as the value is actually in units of microamperes. Use <see cref="XRayTubeCurrentInUa"/> instead.
        /// </remarks>
        /// <value>The X ray tube current in μA.</value>
        [Obsolete("The name of this property is misleading as the value is actually in units of microamperes. Use XRayTubeCurrentInUa instead.")]
        public float XRayTubeCurrentInA
        {
            get { return XRayTubeCurrentInUa; }
            set { XRayTubeCurrentInUa = value; }
        }

        /// <summary>
        /// X-ray Tube Current in μA. An average in the case of fluoroscopy (continuous radiation mode).
        /// </summary>
        /// <value>The X ray tube current in μA.</value>
        public float XRayTubeCurrentInUa
        {
            get { return base.DicomAttributeProvider[DicomTags.XRayTubeCurrentInUa].GetFloat32(0, 0.0F); }
            set { base.DicomAttributeProvider[DicomTags.XRayTubeCurrentInUa].SetFloat32(0, value); }
        }

        /// <summary>
        /// Time of x-ray exposure or fluoroscopy in msec.
        /// </summary>
        /// <value>The exposure time.</value>
        public DateTime? ExposureTime
        {
        	get { return base.DicomAttributeProvider[DicomTags.ExposureTime].GetDateTime(0);  }
            set { base.DicomAttributeProvider[DicomTags.ExposureTime].SetDateTime(0, value); }
        }

        /// <summary>
        /// Type of filter(s) inserted into the X-Ray beam (e.g. wedges). See C.8.7.10 and C.8.15.3.9 (for enhanced CT) for Defined Terms.
        /// </summary>
        /// <value>The type of the filter.</value>
        public string FilterType
        {
            get { return base.DicomAttributeProvider[DicomTags.FilterType].GetString(0, String.Empty); }
            set { base.DicomAttributeProvider[DicomTags.FilterType].SetString(0, value); }
        }

        /// <summary>
        /// The X-Ray absorbing material used in the filter. May be multi-valued. See C.8.7.10 and C.8.15.3.9 (for enhanced CT) for Defined Terms.
        /// </summary>
        /// <value>The filter material.</value>
        public string FilterMaterial
        {
            get { return base.DicomAttributeProvider[DicomTags.FilterMaterial].GetString(0, String.Empty); }
            set { base.DicomAttributeProvider[DicomTags.FilterMaterial].SetString(0, value); }
        }

        /// <summary>
        /// User-defined comments on any special conditions related to radiation dose encountered during during
        /// the episode described by this Exposure Dose Sequence Item.
        /// </summary>
        /// <value>The comments on radiation dose.</value>
        public string CommentsOnRadiationDose
        {
            get { return base.DicomAttributeProvider[DicomTags.CommentsOnRadiationDose].GetString(0, String.Empty); }
            set { base.DicomAttributeProvider[DicomTags.CommentsOnRadiationDose].SetString(0, value); }
        }
        
       #endregion
    }

    #region RadiationMode Enum
    /// <summary>
    /// Specifies X-Ray radiation mode.
    /// </summary>
    public enum RadiationMode
    {
        /// <summary>
        /// 
        /// </summary>
        None,
        /// <summary>
        /// 
        /// </summary>
        Continuous,
        /// <summary>
        /// 
        /// </summary>
        Pulsed
    }
    #endregion
    
}
