#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

#if	UNIT_TESTS

#pragma warning disable 1591,0419,1574,1587

using NUnit.Framework;

namespace uWS.Common.Utilities.Tests
{
	[Cloneable]
	internal class TestDerivedClass : TestCallOrder
	{
		public static bool Cloning = false;

		[CloneIgnore]
		private object _ignoredValue;
		
		private TestDerivedClass(TestDerivedClass source, ICloningContext context)
		{
			context.CloneFields(source, this);
		}

		public TestDerivedClass()
		{
			if (Cloning)
				Assert.Fail("Cloning constructor should be called.");
		}

		public object IgnoredValue
		{
			get { return _ignoredValue; }
			set { _ignoredValue = value; }
		}

		[OnCloneComplete]
		private void OnCloneComplete()
		{
			Assert.AreEqual(CallOrder++, 2);
		}
	}

	[Cloneable(true)]
	internal class TestCallOrder : TestDefaultConstructor
	{
		private int _testField;

		public TestCallOrder()
		{
		}

		public int TestField
		{
			get { return _testField; }
			set { _testField = value; }
		}

		[OnCloneComplete]
		private void OnCloneComplete()
		{
			Assert.AreEqual(CallOrder++, 1);
		}
	}

	[Cloneable(true)]
	internal class TestDefaultConstructor
	{
		private object _cloneableObject;
		[CloneCopyReference]
		private object _copyReferenceObject;
		private int _value;

		[CloneIgnore] 
		public int CallOrder = 0;
		[CloneIgnore]
		public bool CloneInitializeCalled = false;
		[CloneIgnore]
		public bool CloneCompleteCalled = false;

		public TestDefaultConstructor()
		{
		}

		public object CloneableObject
		{
			get { return _cloneableObject; }
			set { _cloneableObject = value; }
		}

		public object CopyReferenceObject
		{
			get { return _copyReferenceObject; }
			set { _copyReferenceObject = value; }
		}

		public int Value
		{
			get { return _value; }
			set { _value = value; }
		}
		
		[CloneInitialize]
		private void Initialize(TestDefaultConstructor source, ICloningContext context)
		{
			context.CloneFields(source, this);
			CloneInitializeCalled = true;
		}

		[OnCloneComplete]
		private void OnCloneComplete()
		{
			CloneCompleteCalled = true;
			Assert.AreEqual(CallOrder++, 0);
		}
	}

	[Cloneable(true)]
	public class SimpleCloneableObject
	{
		public SimpleCloneableObject()
		{
		}
	}

	[TestFixture]
	public class CloningTests
	{
		public CloningTests()
		{
		}

		[Test]
		public void Test()
		{
			try
			{
				SimpleCloneableObject simple = new SimpleCloneableObject();

				TestDerivedClass.Cloning = false;

				TestDerivedClass test = new TestDerivedClass();

				TestDerivedClass.Cloning = true;

				test.IgnoredValue = simple;
				test.Value = 4;
				test.TestField = 5;
				test.CloneableObject = simple;
				test.CopyReferenceObject = simple;

				TestDerivedClass clone = (TestDerivedClass) CloneBuilder.Clone(test);

				Assert.AreEqual(clone.IgnoredValue, null);
				Assert.AreEqual(test.Value, clone.Value);
				Assert.AreEqual(test.TestField, clone.TestField);
				Assert.AreEqual(clone.CloneInitializeCalled, true);
				Assert.AreEqual(clone.CloneCompleteCalled, true);
				Assert.AreSame(clone.CopyReferenceObject, simple);
				Assert.AreNotSame(clone.CloneableObject, simple);
			}
			finally
			{
				TestDerivedClass.Cloning = false;
			}
		}
	}
}

#endif