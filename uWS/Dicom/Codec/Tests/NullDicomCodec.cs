#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

#if UNIT_TESTS

using System.IO;

namespace ClearCanvas.Dicom.Codec.Tests
{
	/// <summary>
	/// A null implementation of a <see cref="IDicomCodec"/> that simply stores the frame data in frame fragments and calls it &quot;compressed&quot;.
	/// </summary>
	public sealed class NullDicomCodec : IDicomCodec
	{
		public const string TransferSyntaxUid = "1.3.6.1.4.1.25403.2200303521616.1888.20130201032029.1";

		public static readonly TransferSyntax TransferSyntax = new TransferSyntax("Null Compression Codec Implementation for Unit Tests",
		                                                                          TransferSyntaxUid,
		                                                                          true, true, true, false, false, true);

		public string Name
		{
			get { return TransferSyntax.Name; }
		}

		public TransferSyntax CodecTransferSyntax
		{
			get { return TransferSyntax; }
		}

		public void Encode(DicomUncompressedPixelData oldPixelData, DicomCompressedPixelData newPixelData, DicomCodecParameters parameters)
		{
			if (oldPixelData.UncompressedFrameSize%2 == 1)
			{
				for (var n = 0; n < oldPixelData.NumberOfFrames; ++n)
				{
					using (var stream = new MemoryStream())
					{
						var data = oldPixelData.GetFrame(n);
						stream.Write(data, 0, data.Length);
						stream.WriteByte(0); // must pad fragments to even length
						newPixelData.AddFrameFragment(stream.ToArray());
					}
				}
			}
			else
			{
				for (var n = 0; n < oldPixelData.NumberOfFrames; ++n)
				{
					newPixelData.AddFrameFragment(oldPixelData.GetFrame(n));
				}
			}
		}

		public void Decode(DicomCompressedPixelData oldPixelData, DicomUncompressedPixelData newPixelData, DicomCodecParameters parameters)
		{
			for (var n = 0; n < oldPixelData.NumberOfFrames; ++n)
				DecodeFrame(n, oldPixelData, newPixelData, parameters);
		}

		public void DecodeFrame(int frame, DicomCompressedPixelData oldPixelData, DicomUncompressedPixelData newPixelData, DicomCodecParameters parameters)
		{
			if (oldPixelData.UncompressedFrameSize%2 == 1)
			{
				using (var stream = new MemoryStream(oldPixelData.GetFrameFragmentData(frame)))
				{
					var data = new byte[oldPixelData.UncompressedFrameSize];
					stream.Read(data, 0, data.Length);
					newPixelData.AppendFrame(data);
				}
			}
			else
			{
				newPixelData.AppendFrame(oldPixelData.GetFrameFragmentData(frame));
			}
		}
	}
}

#endif