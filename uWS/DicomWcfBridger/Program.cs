using System;
using System.Collections.Generic;
using System.ServiceModel;
using System.ServiceModel.Description;
using uWS.Dicom.ServiceModel;
using uWS.Dicom.ServiceModel.Query;

namespace DicomWcfBridger
{
    class Program
    {
        static void Main(string[] args)
        {
            QueryByApi();

            StartWcf();
            QueryByWcf();
            
            Console.WriteLine(@"Press any key to exit!");
            Console.ReadKey();
        }

        private static void QueryByApi()
        {
            ApplicationEntity ae = new ApplicationEntity
                {
                    AETitle = "UIHPACSSERVER",
                    ScpParameters = new ScpParameters("localhost", 3333)
                };

            var query = new StudyRootQueryBridge(new RemoteStudyRootQuery(ae));
            IList<StudyRootStudyIdentifier> results = query.QueryByPatientId("013127");
            foreach (var id in results)
            {
                Console.WriteLine(@"PatientId = {0}, PatientName = {1}", id.PatientId, id.PatientsName);
            }
        }

        private static void StartWcf()
        {
            ServiceHost host = new ServiceHost(typeof(WcfStudyRootQuery));

            foreach (ServiceEndpoint endpoint in host.Description.Endpoints)
                endpoint.Binding.Namespace = QueryNamespace.Value;

            host.Open();
        }

        private static void QueryByWcf()
        {
            Console.WriteLine(@"--------------------------------------------------------");
            Console.WriteLine(@"-------------------QueryByWcf---------------------------");

            StudyRootQueryBridge client = 
                new StudyRootQueryBridge(new StudyRootQueryServiceClient());

            IList<StudyRootStudyIdentifier> results = client.QueryByPatientId("013127");
            foreach (var id in results)
            {
                Console.WriteLine(@"PatientId = {0}, PatientName = {1}", id.PatientId, id.PatientsName);
            }
        }
    }
}
