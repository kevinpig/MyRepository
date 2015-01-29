using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Runtime.InteropServices.ComTypes;
using uWS.ImageView.Imapi2InteropWrapper.Interop;

namespace uWS.ImageView.Burn.MediaItem
{
    public class FileItem : IMediaItem
    {
        /// <summary>
        /// burn media file item
        /// </summary>
        private const Int64 SECTOR_SIZE = 2048;

        private readonly Int64 _fileLength = 0;

        public FileItem(string path)
        {
            try
            {
                if (!File.Exists(path))
                {
                    throw new FileNotFoundException("The file added to FileItem was not found!", path);
                }

                filePath = path;

                FileInfo fileInfo = new FileInfo(filePath);
                _displayName = fileInfo.Name;
                _fileLength = fileInfo.Length;

                //
                // Get the File icon
                //
                SHFILEINFO shinfo = new SHFILEINFO();
                IntPtr hImg = Win32.SHGetFileInfo(filePath, 0, ref shinfo,
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
                throw new global::System.InvalidOperationException("Error adding file" + ex.Message);
            }
        }

        /// <summary>
        /// item size
        /// </summary>
        public Int64 SizeOnDisc
        {
            get
            {
                if (_fileLength > 0)
                {
                    return ((_fileLength / SECTOR_SIZE) + 1) * SECTOR_SIZE;
                }

                return 0;
            }
        }

        /// <summary>
        /// the path of the file
        /// </summary>
        public string Path
        {
            get
            {
                return filePath;
            }
        }

        private string filePath;

        /// <summary>
        /// file icon
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
        /// display name of item
        /// </summary>
        public override string ToString()
        {
            return _displayName;
        }

        private readonly string _displayName;

        /// <summary>
        /// add item to burn data tree
        /// </summary>
        /// <param name="rootItem"></param>
        /// <returns></returns>
        public bool AddToFileSystem(IFsiDirectoryItem rootItem)
        {
            IStream stream = null;

            try
            {
                Win32.SHCreateStreamOnFile(filePath, Win32.STGM_READ | Win32.STGM_SHARE_DENY_WRITE, ref stream);

                if (stream != null)
                {
                    rootItem.AddFile(_displayName, stream);
                    return true;
                }
            }
            catch (Exception ex)
            {
                throw new global::System.InvalidOperationException("Error adding to FileSystem" + ex.Message);
            }
            finally
            {
                if (stream != null)
                {
                    Marshal.FinalReleaseComObject(stream);
                }
            }

            return false;
            
        }
    }
}