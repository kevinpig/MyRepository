using System.Collections.Generic;

namespace uWs.PACS.Model
{
    public class ServerPartition
    {
        public int Id { get; set; }

        public string Aet { get; set; }

        public string Hostname { get; set; }

        public int Port { get; set; }

        public string AeDescription { get; set; }

        public string StationName { get; set; }

        public string Institution { get; set; }

        public string Department { get; set; }

        public string WadoUrl { get; set; }

        public virtual ICollection<Device> Devices { get; set; } 
    }
}