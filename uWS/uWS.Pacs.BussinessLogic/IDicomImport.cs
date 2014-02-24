#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

﻿using uWS.Dicom;

namespace uWS.Pacs.BussinessLogic
{
    public interface IDicomImport
    {
        void Insert(string filename);

        void Insert(DicomMessageBase dicomMessage);
    }
}