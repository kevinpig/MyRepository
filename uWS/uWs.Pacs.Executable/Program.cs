#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

﻿using System;
using uWS.Common;
using uWS.Pacs.DicomService;

namespace uWs.Pacs.Executable
{
    class Program
    {
        static void Main(string[] args)
        {
            Platform.StartApp(args);

            HibernatingRhinos.Profiler.Appender.EntityFramework.EntityFrameworkProfiler.Initialize();

            DicomServiceManager.Instance.StartService();
            
            Console.WriteLine(@"press any key to exit!");
            Console.Read();
        }
    }
}
