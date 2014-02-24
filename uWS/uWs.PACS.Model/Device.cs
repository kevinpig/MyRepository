using System;

namespace uWs.PACS.Model
{
    public partial class Device
    {
        public int Id { get; set; }

        public int ServerPartitionPK { get; set; }

        public string AeTitle { get; set; }

        public string Hostname { get; set; }

        public int Port { get; set; }

        public bool Enabled { get; set; }

        public string Decription { get; set; }

        public bool AllowStorage { get; set; }

        public bool AllowRetrieve { get; set; }

        public bool AllowQuery { get; set; }

        public bool AllowMPPS { get; set; }

        public bool AllowWorkList { get; set; }

        public DateTime LastAccessTime { get; set; }

        public virtual ServerPartition ServerPartition { get; set; }
    }
}