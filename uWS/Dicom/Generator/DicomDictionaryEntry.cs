#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

﻿namespace uWS.Dicom.Generator
{
    public class DicomDictionaryEntry
    {
        public DicomDictionaryEntry(DicomTag tag, string name, string keyword, DicomVM vm, bool retired,
                                    params DicomVr[] vrs)
        {
            Tag = tag;

            Name = string.IsNullOrWhiteSpace(name) ? tag.ToString() : name;

            Keyword = string.IsNullOrWhiteSpace(keyword) ? Name : keyword;

            ValueMultiplicity = vm;
            ValueRepresentations = vrs;
            IsRetired = retired;
        }

        public DicomDictionaryEntry(DicomMaskedTag tag, string name, string keyword, DicomVM vm, bool retired,
                                    params DicomVr[] vrs)
        {
            Tag = tag.Tag;
            MaskTag = tag;

            Name = string.IsNullOrWhiteSpace(name) ? Tag.ToString() : name;
            Keyword = string.IsNullOrWhiteSpace(keyword) ? Name : keyword;

            ValueMultiplicity = vm;
            ValueRepresentations = vrs;
            IsRetired = retired;
        }

        public DicomTag Tag { get; set; }

        public DicomMaskedTag MaskTag
        {
            get;
            private set;
        }

        public string Name { get; set; }

        public string Keyword { get; set; }

        public DicomVr[] ValueRepresentations { get; set; }

        public DicomVM ValueMultiplicity { get; set; }

        public bool IsRetired { get; set; }
    }
}