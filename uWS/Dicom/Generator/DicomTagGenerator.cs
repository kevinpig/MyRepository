#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

﻿using System.Linq;
using System;
using System.Text;

namespace uWS.Dicom.Generator
{
    public class DicomTagGenerator
    {
         public static string Generator(string vnamespace, string vclass, DicomDictionary dict)
         {
             StringBuilder output = new StringBuilder();

             output.AppendFormat("namespace {0} {{ ", vnamespace).AppendLine();
             output.AppendFormat("\tpublic partial class {0} {{", vclass).AppendLine();

             foreach (DicomDictionaryEntry entry in dict)
             {
                 string vrs = string.Join("/", entry.ValueRepresentations.Select(x => x.ToString()));
                 string variable = "_" + Char.ToLower(entry.Keyword[0]) + entry.Keyword.Substring(1);

                 output.AppendFormat("\t\t///<summary>{0} VR={1} VM={2} {3}{4}</summary>",
                                     entry.Tag, vrs, entry.ValueMultiplicity, entry.Name,
                                     entry.IsRetired ? "Retired" : "").AppendLine();
                 output.AppendFormat("\t\tpublic readonly static DicomTag {0}{1} = new DicomTag(0x{2:x4}, 0x{3:x4});",
                                     entry.Keyword, entry.IsRetired ? "Retired" : "", entry.Tag.Group, entry.Tag.Element)
                       .AppendLine();
                 output.AppendLine();
             }

             output.AppendLine("\t}");
             output.AppendLine("}");

             return output.ToString();
         }
    }
}