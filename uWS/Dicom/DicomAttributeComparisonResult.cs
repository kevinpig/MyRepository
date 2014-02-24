#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;
using System.Xml.Serialization;

namespace uWS.Dicom
{
    /// <summary>
    /// Represents the result of the comparison when two sets of attributes are compared using <see cref="DicomAttributeCollection.Equals()"/>.
    /// </summary>
    public class DicomAttributeComparisonResult
    {
        #region Public Overrides
		public override string  ToString()
		{
			return Details;
		}
    	#endregion

        #region Public Properties

    	/// <summary>
    	/// Type of differences.
    	/// </summary>
    	[XmlAttribute]
    	public ComparisonResultType ResultType { get; set; }

    	/// <summary>
    	/// The name of the offending tag. This can be null if the difference is not tag specific.
    	/// </summary>
    	[XmlAttribute]
    	public String TagName { get; set; }

    	/// <summary>
    	/// Detailed text describing the problem.
    	/// </summary>
    	public string Details { get; set; }

    	#endregion

    }
}