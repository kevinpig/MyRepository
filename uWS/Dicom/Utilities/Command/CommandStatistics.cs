#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using uWS.Common.Statistics;

namespace uWS.Dicom.Utilities.Command
{
    /// <summary>
    /// Stores the statistics of a <see cref="CommandBase"/>.
    /// </summary>
    public class CommandStatistics : TimeSpanStatistics
    {
        public CommandStatistics(ICommand cmd)
            : base(cmd.Description)
        {
        }
    }
}
