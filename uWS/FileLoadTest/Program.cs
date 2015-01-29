using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using uWS.Common.Utilities;
using uWS.Dicom;

namespace FileLoadTest
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length < 2)
            {
                Console.WriteLine(@"Usage: FileLoadTest.exe filepath");
                return;
            }

            string path = args[0];
            string multiThread = args[1];

            Stopwatch watch = new Stopwatch();

            if (multiThread == "0")
            {
                watch.Start();
                DirectoryInfo di = new DirectoryInfo(path);
                List<DicomFile> dicoms = new List<DicomFile>();
                Int64 totalSize = 0;
                int count = 0;
                foreach (FileInfo fi in di.EnumerateFiles())
                {
                    FileStream fs = File.OpenRead(fi.FullName);

                    byte[] buffer = new byte[fi.Length];

                    fs.Read(buffer, 0, buffer.Length);
                    MemoryStream ms = new MemoryStream(buffer);

                    DicomFile dicom = new DicomFile();

                    dicom.Load(ms);
// 
//                     DicomFile dicom = new DicomFile(fi.FullName);
//                     dicom.Load(DicomReadOptions.Default);
//                     totalSize += fi.Length;
//                    dicoms.Add(dicom);

//                     FileStream fs = File.OpenRead(fi.FullName);
// 
//                     byte[] buffer = new byte[fi.Length];
//                     fs.Read(buffer, 0, buffer.Length);

                    totalSize += fi.Length;
                    count += 1;
                }
                watch.Stop();
                Console.WriteLine(@"Load Count: {0}, TotalSize: {1} , Easple Times: {2} second",
                    count, totalSize, watch.ElapsedMilliseconds);
                Console.WriteLine(@"Press any key to exit!");
            }
            else
            {
                watch.Start();
                MultiThreadDicomLoad loader = new MultiThreadDicomLoad();
                loader.LoadAllFile(path);
                watch.Stop();

                Console.WriteLine(@"Load Count: {0}, TotalSize: {1} , Easple Times: {2} second",
                loader.DicomFiles.Count, loader.Size, watch.ElapsedMilliseconds);
                Console.WriteLine(@"Press any key to exit!");
            }
            
        }
    }

    public class MultiThreadDicomLoad
    {
        private readonly ItemProcessingThreadPool<MemoryStream> _threadPool = new ItemProcessingThreadPool<MemoryStream>(5);

        public readonly List<DicomFile> DicomFiles = new List<DicomFile>();

        public long Size { get; set; }

        public MultiThreadDicomLoad()
        {
            Size = 0;
        }

        public void LoadAllFile(string dir)
        {
            _threadPool.Start();

            DirectoryInfo di = new DirectoryInfo(dir);
            foreach (var fi in di.EnumerateFiles())
            {
                FileStream fs = File.OpenRead(fi.FullName);

                byte[] buffer = new byte[fi.Length];

                fs.Read(buffer, 0, buffer.Length);
                MemoryStream ms = new MemoryStream(buffer);

                Size += fi.Length;

                _threadPool.Enqueue(ms, ProcessItem);
            }

            _threadPool.Stop(true);
        }

        private void ProcessItem(MemoryStream ms)
        {
            DicomFile df = new DicomFile();

            df.Load(ms);

            //DicomFiles.Add(df);
        }
    }
}
