#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

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