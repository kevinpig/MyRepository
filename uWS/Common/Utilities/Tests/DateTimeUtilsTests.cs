#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

#if UNIT_TESTS

#pragma warning disable 1591

using System;
using NUnit.Framework;

namespace uWS.Common.Utilities.Tests
{
    [TestFixture]
    public class DateTimeUtilsTests
    {
        public DateTimeUtilsTests()
        {
        }

        [Test]
        public void TestFormatISO()
        {
            DateTime date1 = new DateTime(2008, 4, 10, 6, 30, 0);
            Assert.AreEqual("2008-04-10T06:30:00", DateTimeUtils.FormatISO(date1));
        }

        [Test]
        public void TestParseISO()
        {
            string s = "2008-04-10T06:30:00";
            DateTime date1 = new DateTime(2008, 4, 10, 6, 30, 0);
            Assert.AreEqual(date1, DateTimeUtils.ParseISO(s));
        }
    }
}

#endif