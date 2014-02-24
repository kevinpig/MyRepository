using System.Collections.Generic;

namespace uWs.PACS.Model
{
    public class FileSystem
    {
        public int Id { get; set; }

        public string Name { get; set; }

        public string DirPath { get; set; }

        public int NextFileSytemFK { get; set; }

        public string Description { get; set; }

        public virtual ICollection<File> Files { get; set; }

        public virtual FileSystem NextFileSystem { get; set; }

        public virtual ICollection<FileSystem> PriviouseFileSystems { get; set; } 
    }
}