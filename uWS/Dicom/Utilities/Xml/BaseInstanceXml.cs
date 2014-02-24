#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System.Xml;
using uWS.Common;

namespace uWS.Dicom.Utilities.Xml
{
	/// <summary>
	/// Class for representing a base instance of a series as XML.
	/// </summary>
	public class BaseInstanceXml : InstanceXml
	{
		/// <summary>
		/// Creates an empty instance of <see cref="BaseInstanceXml"/>.
		/// </summary>
		public BaseInstanceXml()
			: base(new InstanceXmlDicomAttributeCollection(), null, TransferSyntax.ExplicitVrLittleEndian)
		{
		}

		/// <summary>
		/// Creates an instance of <see cref="BaseInstanceXml"/> from a specified Xml node.
		/// </summary>
		/// <param name="node"></param>
		public BaseInstanceXml(XmlNode node)
			: base(node, null)
		{
		}

		/// <summary>
		/// Creates an instance of <see cref="BaseInstanceXml"/> based on the specified <see cref="DicomAttributeCollection"/>.
		/// </summary>
		/// <param name="collect1"></param>
		/// <param name="collect2"></param>
		public BaseInstanceXml(DicomAttributeCollection collect1, DicomAttributeCollection collect2)
			: this()
		{
			Platform.CheckForNullReference(collect1, "collect1");
			Platform.CheckForNullReference(collect2, "collect2");

			foreach (DicomAttribute attrib1 in collect1)
			{
				DicomAttribute attrib2;
				if ((attrib1 is DicomAttributeOB)
					|| (attrib1 is DicomAttributeOW)
					|| (attrib1 is DicomAttributeOF)
					|| (attrib1 is DicomFragmentSequence))
				{
					if (collect2.TryGetAttribute(attrib1.Tag, out attrib2))
						((IPrivateInstanceXmlDicomAttributeCollection)Collection).ExcludedTagsHelper.Add(attrib1.Tag);
					continue;
				}

				if (collect2.TryGetAttribute(attrib1.Tag, out attrib2))
				{
					if (!attrib1.IsEmpty && attrib1.Equals(attrib2)) //don't store empty tags in the base collection.
					{
						Collection[attrib1.Tag] = attrib1.Copy();
					}
				}
			}

			if (collect1 is IInstanceXmlDicomAttributeCollection && collect2 is IInstanceXmlDicomAttributeCollection)
			{
				IInstanceXmlDicomAttributeCollection collection2 = (IInstanceXmlDicomAttributeCollection) collect2;
				foreach (DicomTag tag in ((IInstanceXmlDicomAttributeCollection)collect1).ExcludedTags)
				{
					if (collection2.ExcludedTags.Contains(tag))
						PrivateCollection.ExcludedTagsHelper.Add(tag);
				}
			}
		}

		public new InstanceXmlDicomAttributeCollection Collection
		{
			get { return (InstanceXmlDicomAttributeCollection)base.Collection; }
		}

		internal IPrivateInstanceXmlDicomAttributeCollection PrivateCollection
		{
			get { return (IPrivateInstanceXmlDicomAttributeCollection)base.Collection; }
		}
	}
}