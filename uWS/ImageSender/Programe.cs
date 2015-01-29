using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading;
using uWS.Dicom;
using uWS.Dicom.Network.Scu;

namespace ImageSender
{
    public class MyDicomGenerater
    {
        public static Random _random = new Random();

        private static ThreadLocal<int> _data = new ThreadLocal<int>();

        public static string GenerateUid()
        {
            int id = Thread.CurrentThread.ManagedThreadId;

            var sb = new StringBuilder();

            sb.Append("1.3.6.1.4.1.25403.");
            sb.Append(id);

            _data.Value++;
            
            sb.Append("." + _data.Value.ToString(CultureInfo.InvariantCulture));

            sb.Append("." + _random.Next(0, Int32.MaxValue));

            String time = DateTime.Now.ToString("yyyyMMddhhmmss");

            sb.Append("." + time);

            return sb.ToString();

        }
    }

    public class Programe
    {
        public static void SpawnAndWait(List<Action> actions)
        {
            var list = actions.ToList();
            ManualResetEvent[] handles = new ManualResetEvent[actions.Count()];
            for (var i = 0; i < list.Count; i++)
            {
                handles[i] = new ManualResetEvent(false);
                var currentAction = list[i];
                var currentHandle = handles[i];
                Action wrappedAction = () => { try { currentAction(); } finally { currentHandle.Set(); } };
                ThreadPool.QueueUserWorkItem(x => wrappedAction());
            }

            WaitHandle.WaitAll(handles);
        }

        static void Main(string[] args)
        {
            if (args.Length < 3)
            {
                Console.WriteLine(@"Usage: ImagerSender.exe filename studynumber seriesnumber");
                return;
            }

            var actions = new List<Action>();
            for (int threadNum = 0; threadNum < Settings1.Default.SendThreadNumber; threadNum++)
            {
                actions.Add( () =>
                    {
                        var f = new DicomFile(args[0]);
                        f.Load();

                        for (int i = 0; i < int.Parse(args[1]); i++)
                        {
                            f.DataSet[DicomTags.StudyInstanceUid].SetString(0, DicomUid.GenerateUid().UID);
                            for (int j = 0; j < int.Parse(args[2]); j++)
                            {
                                f.DataSet[DicomTags.SeriesInstanceUid].SetString(0, DicomUid.GenerateUid().UID);
                                
                                for (int k = 0; k < 20; k++)
                                {
                                    var scu = new StorageScu(Settings1.Default.AETitle, Settings1.Default.RemoteAETitle,
                                        Settings1.Default.RemoteHost, Settings1.Default.RemotePort);
                                    f.DataSet[DicomTags.SopInstanceUid].SetString(0, MyDicomGenerater.GenerateUid());
                                    scu.AddStorageInstance(new StorageInstance(f));
                                    scu.Send();
                                }

                            }
                        }
                    } );
            }
            SpawnAndWait(actions);

            Console.WriteLine(@"Press any key to exist...");
            Console.ReadKey();

        }
    }
}
