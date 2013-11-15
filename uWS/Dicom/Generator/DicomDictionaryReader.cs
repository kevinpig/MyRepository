using System.Collections.Generic;
using System.IO;
using System.Xml.Linq;
using System.Linq;

namespace uWS.Dicom.Generator
{
    public class DicomDictionaryReader
    {
        private DicomDictionary _dict;

        private Stream _stream;

        private void ReadDictionaryXML()
        {
            XDocument xdoc = XDocument.Load(_stream);

            IEnumerable<XElement> xdicts;

            if (xdoc.Root != null && xdoc.Root.Name == "dictionaries")
            {
                xdicts = xdoc.Root.Elements("dictionary");
            }
            else
            {
                XElement xdict = xdoc.Element("dictionary");

                if (xdict == null)
                {
                    throw new DicomDataException("Expected <dictionary> root node in DICOM Dictionary");
                }

                var dicts = new List<XElement> {xdict};
                xdicts = dicts;
            }

            foreach (XElement xdict in xdicts)
            {
                foreach (var xEntry in xdict.Elements("tag"))
                {
                    string name = xEntry.Value ?? "Unknown";

                    string keyword = string.Empty;
                    if (xEntry.Attribute("keyword") != null)
                        keyword = xEntry.Attribute("keyword").Value;

                    // parse the dicom VR
                    List<DicomVr> vrs = new List<DicomVr>();
                    XAttribute xvr = xEntry.Attribute("vr");
                    if (xvr != null && !string.IsNullOrEmpty(xvr.Value))
                    {
                        string[] vra = xvr.Value.Split('_', '/', '\\', ',', '|');

                        foreach (var vr in vra)
                        {
                            vrs.Add(DicomVr.GetVR(vr));   
                        }
                    }
                    else
                    {
                        vrs.Add(DicomVr.UNvr);
                    }

                    // VM 
                    DicomVM vm = DicomVM.Parse(xEntry.Attribute("vm").Value);

                    bool retired = false;
                    XAttribute xretired = xEntry.Attribute("retired");
                    if (xretired != null && !string.IsNullOrEmpty(xretired.Value) && bool.Parse(xretired.Value))
                    {
                        retired = true;
                    }

                    string group = xEntry.Attribute("group").Value;
                    string element = xEntry.Attribute("element").Value;

                    if (group.ToLower().Contains('x') | element.ToLower().Contains('x'))
                    {
                        DicomMaskedTag tag = DicomMaskedTag.Parse(group, element);
                        _dict.Add(new DicomDictionaryEntry(tag, name, keyword, vm, retired, vrs.ToArray()));
                    }
                    else
                    {
                        DicomTag tag = Dicom.DicomTag.Parse(group + "," + element);

                        _dict.Add(new DicomDictionaryEntry(tag, name, keyword, vm, retired, vrs.ToArray()));
                    }
                }
            }
        }
    }
}