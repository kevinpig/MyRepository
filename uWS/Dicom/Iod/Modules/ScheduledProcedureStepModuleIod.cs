#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using uWS.Dicom.Iod.Sequences;

namespace uWS.Dicom.Iod.Modules
{
    /// <summary>
    /// Scheduled Procedure Step Modole
    /// </summary>
    /// <remarks>As per Dicom Doc 3, Table C.4-10 (pg 246)</remarks>
    public class ScheduledProcedureStepModuleIod : IodBase
    {
        #region Constructors
        /// <summary>
		/// Constructor.
		/// </summary>
        public ScheduledProcedureStepModuleIod()
            :base()
        {
        }

        /// <summary>
        /// Constructor.
        /// </summary>
		public ScheduledProcedureStepModuleIod(IDicomAttributeProvider dicomAttributeProvider) : base(dicomAttributeProvider)
        {
        }
        #endregion

        #region Public Properties

        /// <summary>
        /// Gets the scheduled procedure step sequence list.
        /// </summary>
        /// <value>The scheduled procedure step sequence list.</value>
        public SequenceIodList<ScheduledProcedureStepSequenceIod> ScheduledProcedureStepSequenceList
        {
            get 
            {
                return new SequenceIodList<ScheduledProcedureStepSequenceIod>(base.DicomAttributeProvider[DicomTags.ScheduledProcedureStepSequence] as DicomAttributeSQ); 
            }
        }

       #endregion

    }

}
