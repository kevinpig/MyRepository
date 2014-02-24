#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;
using System.Xml;
using uWS.Common;
using uWS.Common.Statistics;
using uWS.Dicom.Codec;

namespace uWS.Dicom.Utilities.Command
{
	/// <summary>
	/// Command for compressing a DICOM Sop Instance.
	/// </summary>
	public class DicomCompressCommand : CommandBase
	{
		private readonly DicomMessageBase _file;
		private readonly IDicomCodec _codec;
		private readonly DicomCodecParameters _parms;
		private readonly TransferSyntax _syntax;
		private readonly TimeSpanStatistics _timeSpan = new TimeSpanStatistics("CompressTime");

		public TimeSpanStatistics CompressTime
		{
			get { return _timeSpan; }
		}

		public DicomCompressCommand(DicomMessageBase file, TransferSyntax syntax, IDicomCodec codec, DicomCodecParameters parms)
			: base("DICOM Compress Command", true)
		{

			_file = file;
			_syntax = syntax;
			_codec = codec;
			_parms = parms;
		}

		public DicomCompressCommand(DicomMessageBase file, XmlDocument parms)
			: base("DICOM Compress Command", true)
		{
			_file = file;

			XmlElement element = parms.DocumentElement;

			string syntax = element.Attributes["syntax"].Value;

			_syntax = TransferSyntax.GetTransferSyntax(syntax);
			if (_syntax == null)
			{
				string failureDescription =
					String.Format("Invalid transfer syntax in compression command: {0}", element.Attributes["syntax"].Value);
				Platform.Log(LogLevel.Error, "Error with input syntax: {0}", failureDescription);
				throw new DicomCodecException(failureDescription);
			}

			IDicomCodecFactory[] codecs = DicomCodecRegistry.GetCodecFactories();
			IDicomCodecFactory theCodecFactory = null;
			foreach (IDicomCodecFactory codec in codecs)
				if (codec.CodecTransferSyntax.Equals(_syntax))
				{
					theCodecFactory = codec;
					break;
				}

			if (theCodecFactory == null)
			{
				string failureDescription = String.Format("Unable to find codec for compression: {0}", _syntax.Name);
				Platform.Log(LogLevel.Error, "Error with compression input parameters: {0}", failureDescription);
				throw new DicomCodecException(failureDescription);
			}

			_codec = theCodecFactory.GetDicomCodec();
			_parms = theCodecFactory.GetCodecParameters(parms);
		}

		protected override void OnExecute(CommandProcessor theProcessor)
		{
			// Check if its already in the right syntax.
			if (_file.TransferSyntax.Equals(_syntax))
				return;

			_timeSpan.Start();

			// Check for decompression first
			if (_file.TransferSyntax.Encapsulated)
				_file.ChangeTransferSyntax(TransferSyntax.ExplicitVrLittleEndian);

			_file.ChangeTransferSyntax(_syntax, _codec, _parms);

			_timeSpan.End();
		}

		protected override void OnUndo()
		{
			
		}
	}
}