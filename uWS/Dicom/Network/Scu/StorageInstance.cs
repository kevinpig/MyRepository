#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;
using uWS.Common;

namespace uWS.Dicom.Network.Scu
{
	/// <summary>
	/// Used by <see cref="StorageScu"/> to specify the <see cref="DicomFile"/>s to transfer over the association.
	/// </summary>
	public class StorageInstance : EventArgs
	{
		#region Private Variables...
		private string _filename;
		private SopClass _sopClass;
		private TransferSyntax _syntax;
		private bool _infoLoaded = false;
		private string _patientId = string.Empty;
		private string _patientsName = string.Empty;
		private DicomFile _dicomFile = null;
		#endregion

		#region Public Properties
		/// <summary>
		/// The filename of the storage instance.
		/// </summary>
		public string Filename
		{
			get { return _filename; }
			set { _filename = value; }
		}

		/// <summary>
		/// The <see cref="SopClass"/> of the storage instance.
		/// </summary>
		public SopClass SopClass
		{
			get { return _sopClass; }
			set
			{
				_sopClass = value;
				if (_sopClass != null && _syntax != null)
					_infoLoaded = true;
			}
		}

		/// <summary>
		/// The SOP Instance Uid of the storage instance.
		/// </summary>
		public string SopInstanceUid { get; set; }

		/// <summary>
		/// The Study Instance Uid of the storage instance.
		/// </summary>
		public string StudyInstanceUid { get; set; }

		/// <summary>
		/// The Study Instance Uid of the storage instance.
		/// </summary>
		public string SeriesInstanceUid { get; set; }

		/// <summary>
		/// The Patient's Name of the storage instance.
		/// </summary>
		public string PatientsName
		{
			get { return _patientsName; }
			set { _patientsName = value; }
		}

		/// <summary>
		/// The Patient Id of the storage instance.
		/// </summary>
		public string PatientId
		{
			get { return _patientId; }
			set { _patientId = value; }
		}

		/// <summary>
		/// The <see cref="TransferSyntax"/> of the storage instance.
		/// </summary>
		public TransferSyntax TransferSyntax
		{
			get { return _syntax; }
			set
			{
				_syntax = value;
				if (_sopClass != null && _syntax != null)
					_infoLoaded = true;
			}
		}

		/// <summary>
		/// The <see cref="DicomStatus"/> returned from the remote SCP when the storage instance was transferred.
		/// </summary>
		public DicomStatus SendStatus { get; set; }

        /// <summary>
        /// The Message ID assigned to the instance when transferred.  Used to identify the response.
        /// </summary>
        public ushort SentMessageId { get; set; }

		/// <summary>
		/// An extended failure description if <see cref="SendStatus"/> is a failure status.
		/// </summary>
		public string ExtendedFailureDescription { get; set; }

		#endregion

		#region Constructors
		/// <summary>
		/// Constructor.
		/// </summary>
		/// <param name="dicomFile"></param>
		public StorageInstance(DicomFile dicomFile)
		{
			_dicomFile = dicomFile;
			
			string sopClassInFile = _dicomFile.DataSet[DicomTags.SopClassUid].ToString();
			if (!sopClassInFile.Equals(_dicomFile.SopClass.Uid))
			{
				Platform.Log(LogLevel.Warn, "SOP Class in Meta Info ({0}) does not match SOP Class in DataSet ({1})",
							 _dicomFile.SopClass.Uid, sopClassInFile);
				_sopClass = SopClass.GetSopClass(sopClassInFile);
				if (_sopClass == null)
				{
					Platform.Log(LogLevel.Warn, "Unknown SOP Class in dataset, reverting to meta info:  {0}", sopClassInFile);
					_sopClass = _dicomFile.SopClass;
				}
			}
			else
				_sopClass = _dicomFile.SopClass;

			_syntax = _dicomFile.TransferSyntax;
			SopInstanceUid = _dicomFile.MediaStorageSopInstanceUid;
			_filename = dicomFile.Filename;

			StudyInstanceUid = _dicomFile.DataSet[DicomTags.StudyInstanceUid].GetString(0, string.Empty);
			SeriesInstanceUid = _dicomFile.DataSet[DicomTags.SeriesInstanceUid].GetString(0, string.Empty);
			PatientsName = _dicomFile.DataSet[DicomTags.PatientsName].GetString(0, string.Empty);
			PatientId = _dicomFile.DataSet[DicomTags.PatientId].GetString(0, string.Empty);
			_infoLoaded = true;
		}

		public StorageInstance(DicomMessage msg)
		{
			_sopClass = msg.SopClass;

			_syntax = msg.TransferSyntax;
			SopInstanceUid = msg.DataSet[DicomTags.SopInstanceUid].GetString(0, string.Empty);

			StudyInstanceUid = msg.DataSet[DicomTags.StudyInstanceUid].GetString(0, string.Empty);
			SeriesInstanceUid = msg.DataSet[DicomTags.SeriesInstanceUid].GetString(0, string.Empty);
			PatientsName = msg.DataSet[DicomTags.PatientsName].GetString(0, string.Empty);
			PatientId = msg.DataSet[DicomTags.PatientId].GetString(0, string.Empty);
			_infoLoaded = true;
		}

		/// <summary>
		/// Constructor.
		/// </summary>
		/// <param name="filename"></param>
		public StorageInstance(string filename)
		{
			_filename = filename;
			StudyInstanceUid = string.Empty;
			SeriesInstanceUid = string.Empty;
			SopInstanceUid = string.Empty;
		}

		/// <summary>
		/// Constructor for primary usage with the <see cref="StorageCommitScu"/> class.
		/// </summary>
		/// <param name="sopClass">The SOP Class for a DICOM instance</param>
		/// <param name="sopInstanceUid">The SOP Instance UID of a DICOM instance</param>
		public StorageInstance(SopClass sopClass, string sopInstanceUid)
		{
			_sopClass = sopClass;
			SopInstanceUid = sopInstanceUid;
			StudyInstanceUid = string.Empty;
			SeriesInstanceUid = string.Empty;
			_filename = String.Empty;
		}
		#endregion

		#region Public Methods
		/// <summary>
		/// Load a <see cref="DicomFile"/> for the storage instance.
		/// </summary>
		/// <remarks>
		/// If the constructor that supplies a <see cref="DicomFile"/> is used, that file is returned.
		/// Otherwise, the file is loaded and returned.  Note that a reference is not kept for the file
		/// in this case.
		/// </remarks>
		/// <returns></returns>
		public DicomFile LoadFile()
		{
			if (_dicomFile != null)
				return _dicomFile;

			DicomFile theFile = new DicomFile(_filename);

			theFile.Load(DicomReadOptions.StorePixelDataReferences | DicomReadOptions.Default);

			StudyInstanceUid = theFile.DataSet[DicomTags.StudyInstanceUid].GetString(0, string.Empty);
			_patientsName = theFile.DataSet[DicomTags.PatientsName].GetString(0, string.Empty);
			_patientId = theFile.DataSet[DicomTags.PatientId].GetString(0, string.Empty);

			return theFile;
		}

		/// <summary>
		/// Load enough information from the file to allow negotiation of the association.
		/// </summary>
		public void LoadInfo()
		{
			if (_infoLoaded)
				return;

			DicomFile theFile = new DicomFile(_filename);

			theFile.Load(DicomTags.RelatedGeneralSopClassUid, DicomReadOptions.Default);
			string sopClassInFile = theFile.DataSet[DicomTags.SopClassUid].ToString();
			if (!sopClassInFile.Equals(theFile.SopClass.Uid))
			{
				Platform.Log(LogLevel.Warn, "SOP Class in Meta Info ({0}) does not match SOP Class in DataSet ({1})",
				             theFile.SopClass.Uid, sopClassInFile);
				_sopClass = SopClass.GetSopClass(sopClassInFile);
				if (_sopClass == null)
				{
					Platform.Log(LogLevel.Warn,"Unknown SOP Class in dataset, reverting to meta info:  {0}", sopClassInFile);
					_sopClass = theFile.SopClass;
				}
			}
			else
				_sopClass = theFile.SopClass;

			_syntax = theFile.TransferSyntax;
			SopInstanceUid = theFile.MediaStorageSopInstanceUid;

			_infoLoaded = true;
		}
		#endregion
	}
}