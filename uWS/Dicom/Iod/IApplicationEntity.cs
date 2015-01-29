#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

namespace uWS.Dicom.Iod
{
	public interface IApplicationEntity
	{
        string Name { get; }
        string AETitle { get; set; }
	    string Description { get; }
		string Location { get; }

	    // TODO (CR Mar 2012): Unsure about this
        IScpParameters ScpParameters { get; }
        IStreamingParameters StreamingParameters { get; }
    }

    public interface IScpParameters
    {
        string HostName { get; }
        int Port { get; }
    }

    public interface IStreamingParameters
    {
        int HeaderServicePort { get; }
        int WadoServicePort { get; }
    }
}
