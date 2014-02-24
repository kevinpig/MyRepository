#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;

namespace uWS.Dicom.Utilities.Command
{
    /// <summary>
    /// Interfacce for the context that the <see cref="CommandProcessor"/> is run within.
    /// </summary>
    /// <remarks>
    /// Note that a <see cref="CommandProcessor"/> should only access a single instance of 
    /// ICommandProcessorContext.  Onces the <see cref="CommandProcessor"/> is disposed, the
    /// <see cref="ICommandProcessorContext"/> will also be disposed.
    /// </remarks>
    public interface ICommandProcessorContext : IDisposable
    {
        /// <summary>
        /// Called by the <see cref="CommandProcessor"/> before an <see cref="ICommand"/> is executed.
        /// </summary>
        /// <param name="command">The command being executed.</param>
        void PreExecute(ICommand command);

        /// <summary>
        /// Called when the <see cref="CommandProcessor"/> commits its <see cref="ICommand"/>s.
        /// </summary>
        void Commit();

        /// <summary>
        /// Called when the <see cref="CommandProcessor"/> rolls back its <see cref="ICommand"/>
        /// </summary>
        void Rollback();

        /// <summary>
        /// Temporary directory path that can be used by <see cref="ICommand"/> instances to store
        /// temporary files.
        /// </summary>
        String TempDirectory { get; }

        /// <summary>
        /// Directory to backup directory that can be used by <see cref="ICommand"/> instances to store backup files.
        /// </summary>
        string BackupDirectory { get; set; }
    }
}
