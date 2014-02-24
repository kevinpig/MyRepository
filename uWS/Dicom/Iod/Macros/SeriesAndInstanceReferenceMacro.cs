#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using uWS.Dicom.Iod.Sequences;

namespace uWS.Dicom.Iod.Macros
{
    /// <summary>
    /// Series and Instance Reference Macro Attributes
    /// </summary>
    /// <remarks>As per Dicom Doc 3, Table 10.4 (pg 78)</remarks>
    public class SeriesAndInstanceReferenceMacro : SequenceIodBase
    {
        #region Constructors
        /// <summary>
        /// Initializes a new instance of the <see cref="SeriesAndInstanceReferenceMacro"/> class.
        /// </summary>
        public SeriesAndInstanceReferenceMacro()
            :base()
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="SeriesAndInstanceReferenceMacro"/> class.
        /// </summary>
        /// <param name="dicomSequenceItem">The dicom sequence item.</param>
        public SeriesAndInstanceReferenceMacro(DicomSequenceItem dicomSequenceItem)
            : base(dicomSequenceItem)
        {
        }
        #endregion

        #region Public Properties
        /// <summary>
        /// Sequence of Items each of which includes the Attributes of one Series. 
        /// One or more Items shall be present. (0008,1115)
        /// </summary>
        /// <value>The referenced series sequence list.</value>
        public SequenceIodList<ReferencedSeriesSequenceIod> ReferencedSeriesSequenceList
        {
            get
            {
                return new SequenceIodList<ReferencedSeriesSequenceIod>(base.DicomAttributeProvider[DicomTags.ReferencedSeriesSequence] as DicomAttributeSQ);
            }
        } 
        #endregion

    }
}
