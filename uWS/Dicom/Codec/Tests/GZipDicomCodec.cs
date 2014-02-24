#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

#if UNIT_TESTS

using System.IO;
using System.IO.Compression;

namespace ClearCanvas.Dicom.Codec.Tests
{
	/// <summary>
	/// A gzip implementation of a <see cref="IDicomCodec"/> that simply compresses each frame's data into a frame fragment.
	/// </summary>
	public sealed class GZipDicomCodec : IDicomCodec
	{
		public const string TransferSyntaxUid = "1.3.6.1.4.1.25403.2200303521616.1888.20130201032029.2";

		public static readonly TransferSyntax TransferSyntax = new TransferSyntax("gzip Compression Codec Implementation for Unit Tests",
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
			for (var n = 0; n < oldPixelData.NumberOfFrames; ++n)
			{
				using (var output = new MemoryStream())
				{
					using (var gzipStream = new GZipStream(output, CompressionMode.Compress, true))
					{
						var data = oldPixelData.GetFrame(n);
						gzipStream.Write(data, 0, data.Length);
					}

					// if the compressed stream is odd length, append an extra byte - gzip will know that it's padding during decompression
					if (output.Length%2 == 1) output.WriteByte(0);

					newPixelData.AddFrameFragment(output.ToArray());
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
			using (var input = new MemoryStream(oldPixelData.GetFrameFragmentData(frame)))
			using (var gzipStream = new GZipStream(input, CompressionMode.Decompress, false))
			{
				var data = new byte[oldPixelData.UncompressedFrameSize];
				gzipStream.Read(data, 0, data.Length);
				newPixelData.AppendFrame(data);
			}
		}
	}
}

#endif