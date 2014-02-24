#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

#if UNIT_TESTS

using System;

namespace uWS.Common.Utilities.Tests
{
	/// <summary>
	/// A pseudorandom number generator implementation based on a 32-bit linear feedback shift register with taps at bits 31, 21, 1 and 0.
	/// </summary>
	/// <remarks>
	/// Unlike <see cref="Random"/>, this implementation will produce deterministic results for a given seed.
	/// </remarks>
	public sealed class PseudoRandom
	{
		private int _currentValue;

		public PseudoRandom()
			: this(Environment.TickCount) {}

		public PseudoRandom(int seed)
		{
			_currentValue = seed != 0 ? seed : 1;
		}

		public int Next()
		{
			// a 32-bit linear feedback shift register implementation - taps at 31, 21, 1, and 0 will produce a maximal length sequence
			var bit = ((_currentValue >> 31) ^ (_currentValue >> 21) ^ (_currentValue >> 1) ^ (_currentValue >> 0)) & 1;
			return _currentValue = ((_currentValue >> 1) & int.MaxValue) | (bit << 31);
		}

		public int Next(int minValue, int maxValue)
		{
			const string message = "minValue must be less than or equal to maxValue.";
			if (minValue > maxValue)
				throw new ArgumentOutOfRangeException("minValue", message);
			var range = (long) maxValue - minValue;
			return (int) (NextDouble()*range) + minValue;
		}

		public int Next(int maxValue)
		{
			const string message = "maxValue must be greater than or equal to zero.";
			if (maxValue < 0)
				throw new ArgumentOutOfRangeException("maxValue", message);
			return (int) (NextDouble()*maxValue);
		}

		public void NextBytes(byte[] buffer)
		{
			if (buffer == null)
				throw new ArgumentNullException("buffer");
			for (var n = 0; n < buffer.Length; n++)
				buffer[n] = (byte) (Next()%256);
		}

		public double NextDouble()
		{
			return ((uint) Next())/(double) (1L + uint.MaxValue);
		}
	}
}

#endif