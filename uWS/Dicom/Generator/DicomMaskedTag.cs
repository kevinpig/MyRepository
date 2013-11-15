using System;
using System.Globalization;

namespace uWS.Dicom.Generator
{
    public sealed class DicomMaskedTag
    {
        private DicomTag _tag = null;
        public const uint FullMask = 0xffffffff;

        public DicomMaskedTag(DicomTag tag)
        {
            _tag = tag;
            Mask = FullMask;
        }

        private DicomMaskedTag()
        {
            
        }

        public DicomTag Tag
        {
            get { return _tag ?? (_tag = new DicomTag(Group, Element)); }
            set 
            {
                _tag = value;
                Card = ((uint)Group << 16) | (uint)Element;
            }
        }

        public ushort Group
        {
            get { return _tag.Group; }
        }

        public ushort Element
        {
            get { return _tag.Element; }
        }

        public uint Card
        {
            get;
            private set;
        }

        public uint Mask { get; private set; }

        public bool IsMatch(DicomTag tag)
        {
            return Card == ((((uint)tag.Group << 16) | (uint)tag.Element) & Mask);
        }

        public static DicomMaskedTag Parse(string s)
        {
            try
            {
                if (s.Length < 8)
                    throw new ArgumentOutOfRangeException("s", "Expected a string of 8 or more characters");

                int pos = 0;
                if (s[pos] == '(')
                    pos++;

                int idx = s.IndexOf(',');
                if (idx == -1)
                    idx = pos + 4;

                string group = s.Substring(pos, idx - pos);

                pos = idx + 1;

                string element = null;
                if (s[s.Length - 1] == ')')
                    element = s.Substring(pos, s.Length - pos - 1);
                else
                    element = s.Substring(pos);

                return Parse(group, element);
            }
            catch (Exception e)
            {
                if (e is DicomDataException)
                    throw;
                else
                    throw new DicomDataException("Error parsing masked DICOM tag ['" + s + "']", e);
            }
        }

        public static DicomMaskedTag Parse(string group, string element)
        {
            try
            {
                DicomMaskedTag tag = new DicomMaskedTag();

                ushort g = ushort.Parse(group.ToLower().Replace('x', '0'), NumberStyles.HexNumber);
                ushort e = ushort.Parse(element.ToLower().Replace('x', '0'), NumberStyles.HexNumber);
                tag.Tag = new DicomTag(g, e);

                string mask = group + element;
                mask = mask.Replace('0', 'f').Replace('1', 'f').Replace('2', 'f')
                            .Replace('3', 'f').Replace('4', 'f').Replace('5', 'f')
                            .Replace('6', 'f').Replace('7', 'f').Replace('8', 'f')
                            .Replace('9', 'f').Replace('a', 'f').Replace('b', 'f')
                            .Replace('c', 'f').Replace('d', 'f').Replace('e', 'f')
                            .Replace('f', 'f').Replace('x', '0');
                tag.Mask = uint.Parse(mask, NumberStyles.HexNumber);

                return tag;
            }
            catch (Exception e)
            {
                throw new DicomDataException("Error parsing masked DICOM tag [group:'" + group + "', element:'" + element + "']", e);
            }
        }
    }
}