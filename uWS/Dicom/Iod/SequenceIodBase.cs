#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

namespace uWS.Dicom.Iod
{
    /// <summary>
    /// Sequence IOD, subclasses <see cref="Iod"/> to take a <see cref="DicomSequenceItem"/> instead of a <see cref="DicomAttributeCollection"/>.
    /// </summary>
    public abstract class SequenceIodBase : IodBase
    {
        #region Constructors
        /// <summary>
        /// Initializes a new instance of the <see cref="SequenceIodBase"/> class.
        /// </summary>
        protected SequenceIodBase() : base(new DicomSequenceItem())
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="SequenceIodBase"/> class.
        /// </summary>
        /// <param name="dicomSequenceItem">The dicom sequence item.</param>
        protected SequenceIodBase(DicomSequenceItem dicomSequenceItem) : base(dicomSequenceItem)
        {
        }

		#endregion

        #region Public Properties
        /// <summary>
        /// Gets the dicom attribute collection as a dicom sequence item.
        /// </summary>
        /// <value>The dicom sequence item.</value>
        public DicomSequenceItem DicomSequenceItem
        {
            get { return base.DicomAttributeProvider as DicomSequenceItem; }
            set { base.DicomAttributeProvider = value; }
        }
        #endregion
    }
}
