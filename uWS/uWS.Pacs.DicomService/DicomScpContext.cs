#region License

// 
// Copyright (c) 2011 - 2012, United-Imaging Inc.
// All rights reserved.
// http://www.united-imaging.com

#endregion

using uWs.PACS.Model;

namespace uWS.Pacs.DicomService
{
    public class DicomScpContext
    {
        #region Constructors

        public DicomScpContext(ServerPartition partition)
        {
            Partition = partition;
        }

        #endregion

        #region Properties

        public ServerPartition Partition { get; set; }

        #endregion
    }
}