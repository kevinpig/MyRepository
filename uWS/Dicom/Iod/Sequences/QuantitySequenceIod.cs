#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using uWS.Dicom.Iod.Macros;

namespace uWS.Dicom.Iod.Sequences
{
    /// <summary>
    /// Quantity Sequence.  
    /// </summary>
    /// <remarks>As per Part 3, Table C4.17, pg 260</remarks>
    public class QuantitySequenceIod : SequenceIodBase
    {
        #region Constructors
        /// <summary>
        /// Initializes a new instance of the <see cref="QuantitySequenceIod"/> class.
        /// </summary>
        public QuantitySequenceIod()
            :base()
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="QuantitySequenceIod"/> class.
        /// </summary>
        /// <param name="dicomSequenceItem">The dicom sequence item.</param>
        public QuantitySequenceIod(DicomSequenceItem dicomSequenceItem)
            : base(dicomSequenceItem)
        {
        }
        #endregion

        #region Public Properties

        /// <summary>
        /// Numerical quantity value.
        /// </summary>
        /// <value>The quantity.</value>
        public float Quantity
        {
            get { return base.DicomAttributeProvider[DicomTags.Quantity].GetFloat32(0, 0.0F); }
            set { base.DicomAttributeProvider[DicomTags.Quantity].SetFloat32(0, value); }
        }

        /// <summary>
        /// Unit of measurement. The sequence may have zero or one Items.
        /// Baseline Context ID is 82.
        /// </summary>
        /// <value>The measuring units sequence list.</value>
        public SequenceIodList<CodeSequenceMacro> MeasuringUnitsSequenceList
        {
            get
            {
                return new SequenceIodList<CodeSequenceMacro>(base.DicomAttributeProvider[DicomTags.MeasuringUnitsSequence] as DicomAttributeSQ);
            }
        }
        
        #endregion
    }
    
}
