#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;
using System.IO;
using uWS.Common;
using uWS.Common.Utilities;

namespace uWS.Dicom.Utilities.Command
{
	/// <summary>
	/// A ServerCommand derived class for creating a directory.
	/// </summary>
	public class CreateDirectoryCommand : CommandBase
	{
		#region Private Members
		protected string _directory;
        protected bool _created = false;
		#endregion

        private GetDirectoryDelegateMethod GetDirectoryDelegate;
        public delegate string GetDirectoryDelegateMethod();

		public CreateDirectoryCommand(string directory)
			: base("Create Directory", true)
		{
			Platform.CheckForNullReference(directory, "Directory name");

			_directory = directory;
		}

        public CreateDirectoryCommand(GetDirectoryDelegateMethod getDirectoryDelegate)
            : base("Create Directory", true)
        {
            Platform.CheckForNullReference(getDirectoryDelegate, "getDirectoryDelegate");
            GetDirectoryDelegate = getDirectoryDelegate;
        }

		protected override void OnExecute(CommandProcessor theProcessor)
		{
            if (String.IsNullOrEmpty(_directory) && GetDirectoryDelegate!=null)
            {
                _directory = GetDirectoryDelegate();
            }

			if (Directory.Exists(_directory))
			{
				_created = false;
				return;
			}

			try
			{
			    Directory.CreateDirectory(_directory);
			}
            catch(UnauthorizedAccessException)
            {
                //alert the system admin
                //ServerPlatform.Alert(AlertCategory.System, AlertLevel.Critical, "Filesystem", 
                //                        AlertTypeCodes.NoPermission, null, TimeSpan.Zero,
                //                     "Unauthorized access to {0} from {1}", _directory, ServerPlatform.HostId);
                throw;
            }

			_created = true;
		}

		protected override void OnUndo()
		{
			if (_created)
			{
				DirectoryUtility.DeleteIfExists(_directory);
				_created = false;
			}
		}
	}

}