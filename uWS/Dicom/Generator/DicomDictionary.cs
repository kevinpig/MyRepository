#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

﻿using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace uWS.Dicom.Generator
{
    public class DicomDictionary : IEnumerable<DicomDictionaryEntry>
    {
        public readonly static DicomDictionaryEntry UnknownTag =
            new DicomDictionaryEntry(DicomMaskedTag.Parse("xxxx", "xxxx").Tag, 
                "Unknown", "Unknown", DicomVM.VM_1_n, false, DicomVr.UNvr);

        #region Private Fields 

        public IDictionary<DicomTag, DicomDictionaryEntry> _entries; 

        private IList<DicomDictionaryEntry> _masked; 

        #endregion

        #region IEnumerable Members

        public IEnumerator<DicomDictionaryEntry> GetEnumerator()
        {
            var items = new List<DicomDictionaryEntry>();
            items.AddRange(_entries.Values.OrderBy(x => x.Tag));

            return items.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        #endregion

        public DicomDictionaryEntry this[DicomTag tag]
        {
            get
            {
                DicomDictionaryEntry entry = null;
                if (_entries.TryGetValue(tag, out entry))
                    return entry;

                foreach (var x in _masked)
                {
                    if (x.MaskTag.IsMatch(tag))
                        return x;
                }

                return UnknownTag;
            }
        }

        public void Add(DicomDictionaryEntry entry)
        {
            if (entry.MaskTag != null)
            {
                _entries.Add(entry.Tag, entry);    
            }
            else
            {
                _masked.Add(entry);
            }
            
        }
        
    }
}