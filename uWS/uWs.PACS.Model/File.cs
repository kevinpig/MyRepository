using System;

namespace uWs.PACS.Model
{
    public class File
    {
        public int Id { get; set; }

        public int InstanceFk { get; set; }

        public int FileSystemFk { get; set; }

        public int FilePath { get; set; }

        public string Md5 { get; set; }

        public int FileSize { get; set; }

        public DateTime CreateTime { get; set; }

        public virtual Instance Instance { get; set; }

        public virtual FileSystem FileSystem { get; set; }
    }
}