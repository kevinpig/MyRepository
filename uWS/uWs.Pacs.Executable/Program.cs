using System;
using uWS.Common;
using uWS.Pacs.DicomService;

namespace uWs.Pacs.Executable
{
    class Program
    {
        static void Main(string[] args)
        {
            Platform.StartApp(args);

            DicomServiceManager.Instance.StartService();

            Console.WriteLine(@"press any key to exit!");
            Console.Read();
        }
    }
}
