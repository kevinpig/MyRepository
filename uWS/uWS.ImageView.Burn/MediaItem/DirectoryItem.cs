using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using uWS.ImageView.Imapi2InteropWrapper.Interop;

namespace uWS.ImageView.Burn.MediaItem
{
    public class DirectoryItem : IMediaItem
    {
        private readonly List<IMediaItem> _mediaItems = new List<IMediaItem>();

        /// <summary>
        /// constructor
        /// </summary>
        /// <param name="directoryPath">the media item path</param>
        public DirectoryItem(string directoryPath)
        {
            try
            {
                if (!Directory.Exists(directoryPath))
                {
                    throw new FileNotFoundException("The directory added to DirectoryItem was not found!", directoryPath);
                }

                _mDirectoryPath = directoryPath;
                FileInfo fileInfo = new FileInfo(_mDirectoryPath);
                _displayName = fileInfo.Name;

                //
                // Get all the files in the directory
                //
                string[] files = Directory.GetFiles(_mDirectoryPath);
                foreach (string file in files)
                {
                    _mediaItems.Add(new FileItem(file));
                }

                //
                // Get all the subdirectories
                //
                string[] directories = Directory.GetDirectories(_mDirectoryPath);
                foreach (string directory in directories)
                {
                    _mediaItems.Add(new DirectoryItem(directory));
                }

                //
                // Get the Directory icon
                //
                SHFILEINFO shinfo = new SHFILEINFO();
                IntPtr hImg = Win32.SHGetFileInfo(_mDirectoryPath, 0, ref shinfo,
                    (uint)Marshal.SizeOf(shinfo), Win32.SHGFI_ICON | Win32.SHGFI_SMALLICON);

                if (IntPtr.Zero != shinfo.hIcon)
                {
                    //The icon is returned in the hIcon member of the shinfo struct
                    System.Drawing.IconConverter imageConverter = new System.Drawing.IconConverter();
                    System.Drawing.Icon icon = System.Drawing.Icon.FromHandle(shinfo.hIcon);
                    try
                    {
                        _fileIconImage = (System.Drawing.Image)
                            imageConverter.ConvertTo(icon, typeof(System.Drawing.Image));
                    }
                    catch (NotSupportedException)
                    {
                    }

                    Win32.DestroyIcon(shinfo.hIcon);
                }
            }
            catch (Exception ex)
            {
                throw new global::System.InvalidOperationException("Error adding folder" + ex.Message);
            }
        }

        /// <summary>
        /// the path of the directory
        /// </summary>
        public string Path
        {
            get
            {
                return _mDirectoryPath;
            }
        }
        private readonly string _mDirectoryPath;

        /// <summary>
        /// display name of item
        /// </summary>
        public override string ToString()
        {
            return _displayName;
        }
        private readonly string _displayName;

        /// <summary>
        /// item size
        /// </summary>
        public Int64 SizeOnDisc
        {
            get
            {
                return _mediaItems.Sum(mediaItem => mediaItem.SizeOnDisc);
            }
        }

        /// <summary>
        /// directory icon
        /// </summary>
        public System.Drawing.Image FileIconImage
        {
            get
            {
                return _fileIconImage;
            }
        }
        private readonly System.Drawing.Image _fileIconImage = null;

        /// <summary>
        /// add item to burn data tree
        /// </summary>
        /// <param name="rootItem"></param>
        /// <param name="isBurnDirectoryExist"></param>
        /// <returns></returns>
        public bool AddToFileSystem(IFsiDirectoryItem rootItem)
        {
            try
            {
                var filename = System.IO.Path.GetFileName(_mDirectoryPath);

                IFsiItem item;
                try
                {
                    item = (IFsiItem)rootItem["\\" + filename];
                }
                catch (Exception)
                {
                    item = null;
                }

                if (item != null)
                {
                    IFsiDirectoryItem dirItem = item as IFsiDirectoryItem;
                    foreach (var mediaItem in _mediaItems)
                    {
                        mediaItem.AddToFileSystem(dirItem);
                    }
                }
                else
                {
                    rootItem.AddTree(_mDirectoryPath, true);
                }

                return true;
            }
            catch (Exception ex)
            {
                throw new global::System.InvalidOperationException("Error adding to FileSystem" + ex.Message);
            }
        }
    }
}