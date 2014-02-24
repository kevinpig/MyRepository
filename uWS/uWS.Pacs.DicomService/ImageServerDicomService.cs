#region License

// 
// Copyright (c) 2011 - 2012, United-Imaging Inc.
// All rights reserved.
// http://www.united-imaging.com

#endregion

using System.ComponentModel.Composition;

namespace uWs.Pacs.Executable
{
    /*
    [Export(typeof(IPluginService)),
     ExportMetadata("Name", "Image Server Dicom Service"),
     ExportMetadata("Description", "Provide Pacs C-Query C-Store C-Move SCP")]
    public class ImagerServerDicomService : IPluginService
    {
        #region Private Member

        private bool _isActive;

        #endregion

        #region Public Methods

        public void Start()
        {

            DicomServiceManager.Instance.StartService();

            _isActive = true;
        }

        public void Stop()
        {
            DicomServiceManager.Instance.StopService();

            _isActive = false;
        }

        public bool IsActive()
        {
            return _isActive;
        }

        #endregion
    }
     * 
     */
}