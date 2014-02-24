#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using uWS.Dicom.Iod.Modules;

namespace uWS.Dicom.Iod.Iods
{
    /// <summary>
    /// Generic Image IOD.  Note, in progress.
    /// </summary>
    public class ImageIod : IodBase
    {
        #region Constructors
        /// <summary>
        /// Initializes a new instance of the <see cref="ImageIod"/> class.
        /// </summary>
        public ImageIod()
            :base()
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="ImageIod"/> class.
        /// </summary>
        public ImageIod(IDicomAttributeProvider dicomAttributeProvider) : base(dicomAttributeProvider)
        {
        }
        #endregion

        #region Public Properties
        /// <summary>
        /// Gets the patient module.
        /// </summary>
        /// <value>The patient module.</value>
        public PatientIdentificationModuleIod PatientIdentificationModule
        {
            get { return base.GetModuleIod<PatientIdentificationModuleIod>(); }
        }

        /// <summary>
        /// Gets the study module.
        /// </summary>
        /// <value>The study module.</value>
        public StudyModuleIod StudyModule
        {
            get { return base.GetModuleIod<StudyModuleIod>(); }
        }
        #endregion

    }
}
