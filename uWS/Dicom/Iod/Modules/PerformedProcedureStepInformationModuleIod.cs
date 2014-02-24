#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;
using uWS.Dicom.Iod.Macros;
using uWS.Dicom.Utilities;

namespace uWS.Dicom.Iod.Modules
{
    /// <summary>
    /// Patient Identification Module, as per Part 3, C.4.14 (pg 255)
    /// </summary>
    public class PerformedProcedureStepInformationModuleIod : IodBase
    {
        #region Constructors
        /// <summary>
        /// Initializes a new instance of the <see cref="PerformedProcedureStepInformationModuleIod"/> class.
        /// </summary>
        public PerformedProcedureStepInformationModuleIod()
            :base()
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="PerformedProcedureStepInformationModuleIod"/> class.
        /// </summary>
		public PerformedProcedureStepInformationModuleIod(IDicomAttributeProvider dicomAttributeProvider) : base(dicomAttributeProvider)
        {
        }
        #endregion

        #region Public Properties

        /// <summary>
        /// Gets or sets the performed station ae title.
        /// </summary>
        /// <value>The performed station ae title.</value>
        public string PerformedStationAeTitle
        {
            get { return base.DicomAttributeProvider[DicomTags.PerformedStationAeTitle].GetString(0, String.Empty); }
            set { base.DicomAttributeProvider[DicomTags.PerformedStationAeTitle].SetString(0, value); }
        }

        public string PerformedStationName
        {
            get { return base.DicomAttributeProvider[DicomTags.PerformedStationName].GetString(0, String.Empty); }
            set { base.DicomAttributeProvider[DicomTags.PerformedStationName].SetString(0, value); }
        }

        public string PerformedLocation
        {
            get { return base.DicomAttributeProvider[DicomTags.PerformedLocation].GetString(0, String.Empty); }
            set { base.DicomAttributeProvider[DicomTags.PerformedLocation].SetString(0, value); }
        }

        /// <summary>
        /// Gets or sets the performed procedure step start date.
        /// </summary>
        /// <value>The performed procedure step start date.</value>
        public DateTime? PerformedProcedureStepStartDate
        {
            get { return DateTimeParser.ParseDateAndTime(base.DicomAttributeProvider, 0, DicomTags.PerformedProcedureStepStartDate, DicomTags.PerformedProcedureStepStartTime); }

            set { DateTimeParser.SetDateTimeAttributeValues(value, base.DicomAttributeProvider, 0, DicomTags.PerformedProcedureStepStartDate, DicomTags.PerformedProcedureStepStartTime); }
        }

        /// <summary>
        /// Gets or sets the performed procedure step id.
        /// </summary>
        /// <value>The performed procedure step id.</value>
        public string PerformedProcedureStepId
        {
            get { return base.DicomAttributeProvider[DicomTags.PerformedProcedureStepId].GetString(0, String.Empty); }
            set { base.DicomAttributeProvider[DicomTags.PerformedProcedureStepId].SetString(0, value); }
        }

        public DateTime? PerformedProcedureStepEndDate
        {
            get { return DateTimeParser.ParseDateAndTime(base.DicomAttributeProvider, 0, DicomTags.PerformedProcedureStepEndDate, DicomTags.PerformedProcedureStepEndTime); }
        
            set { DateTimeParser.SetDateTimeAttributeValues(value, base.DicomAttributeProvider, 0, DicomTags.PerformedProcedureStepEndDate, DicomTags.PerformedProcedureStepEndTime); }
        }

        /// <summary>
        /// Gets or sets the performed procedure step status.
        /// </summary>
        /// <value>The performed procedure step status.</value>
        public PerformedProcedureStepStatus PerformedProcedureStepStatus
        {
            get { return IodBase.ParseEnum<PerformedProcedureStepStatus>(base.DicomAttributeProvider[DicomTags.PerformedProcedureStepStatus].GetString(0, String.Empty), PerformedProcedureStepStatus.None); }
            set { IodBase.SetAttributeFromEnum(base.DicomAttributeProvider[DicomTags.PerformedProcedureStepStatus], value, true); }
        }

        /// <summary>
        /// Gets or sets the performed procedure step description.
        /// </summary>
        /// <value>The performed procedure step description.</value>
        public string PerformedProcedureStepDescription
        {
            get { return base.DicomAttributeProvider[DicomTags.PerformedProcedureStepDescription].GetString(0, String.Empty); }
            set { base.DicomAttributeProvider[DicomTags.PerformedProcedureStepDescription].SetString(0, value); }
        }

        /// <summary>
        /// Gets or sets the comments on the performed procedure step.
        /// </summary>
        /// <value>The comments on the performed procedure step.</value>
        public string CommentsOnThePerformedProcedureStep
        {
            get { return base.DicomAttributeProvider[DicomTags.CommentsOnThePerformedProcedureStep].GetString(0, String.Empty); }
            set { base.DicomAttributeProvider[DicomTags.CommentsOnThePerformedProcedureStep].SetString(0, value); }
        }

        /// <summary>
        /// Gets or sets the performed procedure type description.
        /// </summary>
        /// <value>The performed procedure type description.</value>
        public string PerformedProcedureTypeDescription
        {
            get { return base.DicomAttributeProvider[DicomTags.PerformedProcedureTypeDescription].GetString(0, String.Empty); }
            set { base.DicomAttributeProvider[DicomTags.PerformedProcedureTypeDescription].SetString(0, value); }
        }

        /// <summary>
        /// Gets the procedure code sequence list.
        /// </summary>
        /// <value>The procedure code sequence list.</value>
        public SequenceIodList<CodeSequenceMacro> ProcedureCodeSequenceList
        {
            get
            {
                return new SequenceIodList<CodeSequenceMacro>(base.DicomAttributeProvider[DicomTags.ProcedureCodeSequence] as DicomAttributeSQ);
            }
        }

        /// <summary>
        /// Gets the performed procedure step discontinuation reason code sequence list.
        /// </summary>
        /// <value>
        /// The performed procedure step discontinuation reason code sequence list.
        /// </value>
        public SequenceIodList<CodeSequenceMacro> PerformedProcedureStepDiscontinuationReasonCodeSequenceList
        {
            get
            {
                return new SequenceIodList<CodeSequenceMacro>(base.DicomAttributeProvider[DicomTags.PerformedProcedureStepDiscontinuationReasonCodeSequence] as DicomAttributeSQ);
            }
        }
       
        #endregion

    }

    #region PerformedProcedureStepStatus Enum
    /// <summary>
    /// Enumeration for PerformedProcedureStepStatus
    /// </summary>
    public enum PerformedProcedureStepStatus
    {
        /// <summary>
        /// None, or blank value
        /// </summary>
        None,
        /// <summary>
        /// Started but not complete
        /// </summary>
        InProgress,
        /// <summary>
        /// Canceled or unsuccessfully terminated
        /// </summary>
        Discontinued,
        /// <summary>
        /// Successfully completed
        /// </summary>
        Completed
    }
    #endregion
    
}
