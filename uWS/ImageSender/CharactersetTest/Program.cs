using System;
using uWS.Dicom;

namespace CharactersetTest
{
    class Program
    {
        static void Main(string[] args)
        {
            DicomFile file = new DicomFile();
            file.DataSet.SpecificCharacterSet = "ISO_IR 100";

            file.DataSet[DicomTags.SpecificCharacterSet].SetStringValue("ISO_IR 100");
            file.DataSet[DicomTags.PatientsBirthName].SetStringValue("中文");
            file.DataSet[DicomTags.PatientsName].SetStringValue("(中文)");

            file.Save("1.dcm");

            DicomFile readFile = new DicomFile("1.dcm");
            readFile.Load();
            Console.WriteLine(readFile.DataSet[DicomTags.PatientsBirthName].GetString(0, string.Empty));
            Console.WriteLine(readFile.DataSet[DicomTags.PatientsName].GetString(0, string.Empty));

            DicomFile file2 = new DicomFile();
            file2.DataSet.SpecificCharacterSet = "GB18030";

            file2.DataSet[DicomTags.SpecificCharacterSet].SetStringValue("GB18030");
            file2.DataSet[DicomTags.PatientsName].SetStringValue("刘瑞飞(liuruifei)");
            file2.Save("2.dcm");

            DicomFile readFile2 = new DicomFile("2.dcm");
            readFile2.Load();
            Console.WriteLine(readFile2.DataSet[DicomTags.PatientsName].GetString(0, string.Empty));

            Console.WriteLine(@"press any key to exit!");
            Console.ReadKey();

        }
    }
}
