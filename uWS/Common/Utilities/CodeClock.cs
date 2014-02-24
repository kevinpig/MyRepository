#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;

namespace uWS.Common.Utilities
{
	#region IPerformanceCounter

	//this used to be public to support other performance counter types, but has been made internal for now.
	internal interface IPerformanceCounter
	{
		long Count
		{
			get;
		}

		long Frequency
		{
			get;
		}
	}

	#endregion

	#region Performance Counters

	internal class Win32PerformanceCounter : IPerformanceCounter
	{
		[System.Runtime.InteropServices.DllImport("KERNEL32")]
		private static extern bool QueryPerformanceCounter(ref long lpPerformanceCount);

		[System.Runtime.InteropServices.DllImport("KERNEL32")]
		private static extern bool QueryPerformanceFrequency(ref long lpFrequency);

		public long Count
		{
			get
			{
				long count = 0;
				QueryPerformanceCounter(ref count);
				return count;
			}
		}

		public long Frequency
		{
			get
			{
				long freq = 0;
				QueryPerformanceFrequency(ref freq);
				return freq;
			}
		}
	}

	internal class DefaultPerformanceCounter : IPerformanceCounter
	{

		public long Count
		{
			get
			{
				return System.DateTime.UtcNow.Ticks;
			}
		}

		public long Frequency
		{
			get
			{
				return 10000000;	// 10 million
			}
		}
	}

	#endregion 

	/// <summary>
	/// A simple stopwatch class that can be used to profile code.  
	/// </summary>
	/// <remarks>
	/// <para>
	/// To ensure portability, use this class instead of the <see cref="System.Diagnostics.Stopwatch" /> 
	/// class which has not yet been implemented in Mono.
	/// </para>
	/// <para>
	/// On Windows, this class will internally use the Win32 high resolution performance counter.
	/// On other platforms, a default portable clock is used.
	/// </para>
	/// </remarks>
	/// <example>
	/// <code>
    /// CodeClock clock = new CodeClock();
    /// clock.Start();
	///
	/// // Code to be timed
	///
    /// clock.Stop();
    /// Trace.Write(clock.ToString());
	/// </code>
	/// </example>
	public class CodeClock
	{
		long elapsedCount = 0;
		long startCount = 0;
		
		private IPerformanceCounter _clock;

		/// <summary>
        /// Initializes a new instance of the <see cref="CodeClock"/> class.
		/// </summary>
		public CodeClock()
		{
			if(Platform.IsWin32Platform)
			{
				_clock = new Win32PerformanceCounter();
			}
			else
			{
				_clock = new DefaultPerformanceCounter();
			}
		}

		/// <summary>
		/// Starts the clock.
		/// </summary>
		public void Start()
		{
			startCount = _clock.Count;
		}

		/// <summary>
		/// Stops the clock.
		/// </summary>
		public void Stop()
		{
			long stopCount = _clock.Count;
			elapsedCount += (stopCount - startCount);
		}

		/// <summary>
		/// Clears (resets) the clock.
		/// </summary>
		public void Clear()
		{
			elapsedCount = 0;
		}

		/// <summary>
		/// Gets the number of seconds elapsed between start and stop.
		/// </summary>
		public float Seconds
		{
			get
			{
				return((float) elapsedCount / (float) _clock.Frequency);
			}
		}

		/// <summary>
		/// Gets the number of seconds elapsed between start and stop as a formatted string.
		/// </summary>
		public override string ToString()
		{
			return String.Format(SR.FormatSeconds, Seconds);
		}
	}
}
