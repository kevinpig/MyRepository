#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using uWS.Dicom.Iod.Macros;
using uWS.Dicom.Iod.Sequences;

namespace uWS.Dicom.Iod.Modules
{
    /// <summary>
    /// As per Dicom DOC 3 Table C.4-17
    /// </summary>
    public class BillingAndMaterialManagementCodesModuleIod : IodBase
    {
        #region Constructors
        /// <summary>
        /// Initializes a new instance of the <see cref="BillingAndMaterialManagementCodesModuleIod"/> class.
        /// </summary>
        public BillingAndMaterialManagementCodesModuleIod()
            :base()
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="BillingAndMaterialManagementCodesModuleIod"/> class.
        /// </summary>
		public BillingAndMaterialManagementCodesModuleIod(IDicomAttributeProvider dicomAttributeProvider) : base(dicomAttributeProvider)
        {
        }
        #endregion

        #region Public Properties
        /// <summary>
        /// Contains billing codes for the Procedure Type performed within the Procedure Step. The sequence may have zero or more Items.
        /// </summary>
        /// <value>The billing procedure step sequence list.</value>
        public SequenceIodList<CodeSequenceMacro> BillingProcedureStepSequenceList
        {
            get
            {
                return new SequenceIodList<CodeSequenceMacro>(base.DicomAttributeProvider[DicomTags.BillingProcedureStepSequence] as DicomAttributeSQ);
            }
        }

        /// <summary>
        /// Information about the film consumption for this Performed Procedure Step. The sequence may have zero or more Items.
        /// </summary>
        /// <value>The film consumption sequence list.</value>
        public SequenceIodList<FilmConsumptionSequenceIod> FilmConsumptionSequenceList
        {
            get
            {
                return new SequenceIodList<FilmConsumptionSequenceIod>(base.DicomAttributeProvider[DicomTags.FilmConsumptionSequence] as DicomAttributeSQ);
            }
        }

        /// <summary>
        /// Chemicals, supplies and devices for billing used in the Performed Procedure Step. The sequence may have one or more Items.
        /// </summary>
        /// <value>The billing supplies and devices sequence list.</value>
        public SequenceIodList<BillingSuppliesAndDevicesSequenceIod> BillingSuppliesAndDevicesSequenceList
        {
            get
            {
                return new SequenceIodList<BillingSuppliesAndDevicesSequenceIod>(base.DicomAttributeProvider[DicomTags.BillingSuppliesAndDevicesSequence] as DicomAttributeSQ);
            }
        }

        #endregion

    }
}
