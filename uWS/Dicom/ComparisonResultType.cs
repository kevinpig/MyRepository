#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

namespace uWS.Dicom
{
    /// <summary>
    /// Types of differences when two sets of attributes are compared using <see cref="DicomAttributeCollection.Equals()"/>.
    /// </summary>
    public enum ComparisonResultType
    {
        /// <summary>
        /// Cannot be compared with the target because of its type.
        /// </summary>
        InvalidType,

        /// <summary>
        /// Source and target does not have the same set of attributes.
        /// </summary>
        DifferentAttributeSet,

        /// <summary>
        /// Attributes in the source and target have different values.
        /// </summary>
        DifferentValues
    }
}