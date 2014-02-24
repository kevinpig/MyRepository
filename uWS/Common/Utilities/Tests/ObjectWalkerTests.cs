#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

#if UNIT_TESTS

#pragma warning disable 1591

using System;
using System.Collections.Generic;
using NUnit.Framework;
using System.Reflection;

namespace uWS.Common.Utilities.Tests
{
	[TestFixture]
	public class ObjectWalkerTests
	{
		public class Foo
		{
			public static readonly object SomeObject = new object();

			private bool PrivateField = true;
			public string PublicField = "hello";

			private int _privatePropertyField = 10;
			private object _publicPropertyField = SomeObject;

			private int PrivateProperty
			{
				get { return _privatePropertyField; }
				set { _privatePropertyField = value;  }
			}

			public object PublicProperty
			{
				get { return _publicPropertyField; }
				set { _publicPropertyField = value; }
			}

			public bool CheckConsistency(bool b, string s, int i, object o)
			{
				return PrivateField == b
				       && PublicField == s
				       && _privatePropertyField == i
				       && _publicPropertyField == o;
			}

		}

		public ObjectWalkerTests()
		{

		}

		[Test]
		public void TestWalkPublicProperties()
		{
			ObjectWalker walker = new ObjectWalker();

			// public properties
			walker.IncludeNonPublicFields = false;
			walker.IncludeNonPublicProperties = false;
			walker.IncludePublicFields = false;
			walker.IncludePublicProperties = true;

			List<IObjectMemberContext> members = new List<IObjectMemberContext>(walker.Walk(new Foo()));
			Assert.AreEqual(1, members.Count);
			Assert.AreEqual("PublicProperty", members[0].Member.Name);
			Assert.AreEqual(Foo.SomeObject, members[0].MemberValue);
		}

		[Test]
		public void TestWalkPublicFields()
		{
			ObjectWalker walker = new ObjectWalker();

			// public fields
			walker.IncludeNonPublicFields = false;
			walker.IncludeNonPublicProperties = false;
			walker.IncludePublicFields = true;
			walker.IncludePublicProperties = false;

			List<IObjectMemberContext> members = new List<IObjectMemberContext>(walker.Walk(new Foo()));
			Assert.AreEqual(1, members.Count);
			Assert.AreEqual("PublicField", members[0].Member.Name);
			Assert.AreEqual("hello", members[0].MemberValue);
		}

		[Test]
		public void TestWalkPrivateProperties()
		{
			ObjectWalker walker = new ObjectWalker();

			// private property
			walker.IncludeNonPublicFields = false;
			walker.IncludeNonPublicProperties = true;
			walker.IncludePublicFields = false;
			walker.IncludePublicProperties = false;

			List<IObjectMemberContext> members = new List<IObjectMemberContext>(walker.Walk(new Foo()));
			Assert.AreEqual(1, members.Count);
			Assert.AreEqual("PrivateProperty", members[0].Member.Name);
			Assert.AreEqual(10, members[0].MemberValue);
		}

		[Test]
		public void TestWalkPrivateFields()
		{
			ObjectWalker walker = new ObjectWalker();

			// private field
			walker.IncludeNonPublicFields = true;
			walker.IncludeNonPublicProperties = false;
			walker.IncludePublicFields = false;
			walker.IncludePublicProperties = false;

			List<IObjectMemberContext> members = new List<IObjectMemberContext>(walker.Walk(new Foo()));
			// there will be 3 private fields, becausing of the backing fields for the properties
			Assert.AreEqual(3, members.Count);
			foreach (IObjectMemberContext member in members)
			{
				if(member.Member.Name == "PrivateField")
					Assert.AreEqual(true, member.MemberValue);
			}
			
		}

		[Test]
		public void TestWalkAllMembers()
		{
			ObjectWalker walker = new ObjectWalker();

			// all members
			walker.IncludeNonPublicFields = true;
			walker.IncludeNonPublicProperties = true;
			walker.IncludePublicFields = true;
			walker.IncludePublicProperties = true;

			List<IObjectMemberContext> members = new List<IObjectMemberContext>(walker.Walk(new Foo()));
			// there will be 6 members, becausing of the backing fields for the properties
			Assert.AreEqual(6, members.Count);
		}

		[Test]
		public void TestWalkAllMembersWithFilter()
		{
			// exclude members starting with an underscore
			ObjectWalker walker = new ObjectWalker(delegate(MemberInfo member) { return !member.Name.StartsWith("_"); });

			// all members
			walker.IncludeNonPublicFields = true;
			walker.IncludeNonPublicProperties = true;
			walker.IncludePublicFields = true;
			walker.IncludePublicProperties = true;

			List<IObjectMemberContext> members = new List<IObjectMemberContext>(walker.Walk(new Foo()));
			Assert.AreEqual(4, members.Count);
		}

		[Test]
		public void TestWalkType()
		{
			// exclude members starting with an underscore
			ObjectWalker walker = new ObjectWalker();

			// all members
			walker.IncludeNonPublicFields = true;
			walker.IncludeNonPublicProperties = true;
			walker.IncludePublicFields = true;
			walker.IncludePublicProperties = true;

			// in this case we walk typeof(Foo) rather than an instance of Foo
			List<IObjectMemberContext> members = new List<IObjectMemberContext>(walker.Walk(typeof(Foo)));
			// there will be 6 members, becausing of the backing fields for the properties
			Assert.AreEqual(6, members.Count);
		}

		[Test]
		public void TestMutation()
		{
			// exclude members starting with an underscore
			ObjectWalker walker = new ObjectWalker(delegate(MemberInfo member) { return !member.Name.StartsWith("_"); });

			// all members
			walker.IncludeNonPublicFields = true;
			walker.IncludeNonPublicProperties = true;
			walker.IncludePublicFields = true;
			walker.IncludePublicProperties = true;

			Foo foo = new Foo();
			Foo foo2 = new Foo();

			// in this test we change all the values of foo via the IObjectMemberContext
			foreach (IObjectMemberContext context in walker.Walk(foo))
			{
				if (context.MemberType == typeof(bool))
					context.MemberValue = false;
				if (context.MemberType == typeof(string))
					context.MemberValue = "goodbye";
				if (context.MemberType == typeof(int))
					context.MemberValue = 1;
				if (context.MemberType == typeof(object))
					context.MemberValue = foo2;

			}

			Assert.AreEqual(true, foo.CheckConsistency(false, "goodbye", 1, foo2));
		}

	}
}

#endif
