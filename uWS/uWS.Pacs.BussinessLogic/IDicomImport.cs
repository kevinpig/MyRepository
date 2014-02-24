using uWS.Dicom;

namespace uWS.Pacs.BussinessLogic
{
    public interface IDicomImport
    {
        void Insert(string filename);

        void Insert(DicomMessageBase dicomMessage);
    }
}