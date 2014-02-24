#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;
using System.Collections.Generic;
using System.Reflection;
using System.Linq;

namespace uWS.Common.Utilities
{
    /// <summary>
    /// Utilities class that provides a set of convenience methods for working with attributes.
    /// </summary>
    public static class AttributeUtils
    {
        /// <summary>
        /// Searches a type/method/property/field for attributes of the specified type, matching the
        /// specified filter.
        /// </summary>
        /// <typeparam name="TAttribute">The type of attribute (may also be a base class).</typeparam>
        /// <param name="member">The type/method/property/field to find attributes on.</param>
        /// <param name="inherit">True to include inherited attributes in the search.</param>
        /// <param name="filter">A filter that restricts the results of the search.</param>
        /// <returns>A list of matching attributes.</returns>
        public static List<TAttribute> GetAttributes<TAttribute>(MemberInfo member, bool inherit, Predicate<TAttribute> filter)
            where TAttribute : Attribute
        {
            var attrs = member.GetCustomAttributes<TAttribute>(inherit).Where(a => filter(a));

            return attrs.ToList();
        }

        /// <summary>
        /// Searches a type/method/property/field for attributes of the specified type.
        /// </summary>
        /// <typeparam name="TAttribute">The type of attribute (may also be a base class).</typeparam>
        /// <param name="member">The type/method/property/field to find attributes on.</param>
        /// <param name="inherit">True to include inherited attributes in the search.</param>
        /// <returns>A list of matching attributes.</returns>
        public static List<TAttribute> GetAttributes<TAttribute>(MemberInfo member, bool inherit)
            where TAttribute : Attribute
        {
            return GetAttributes<TAttribute>(member, inherit, NullFilter);
        }

        /// <summary>
        /// Searches a type/method/property/field for attributes of the specified type.
        /// </summary>
        /// <typeparam name="TAttribute">The type of attribute (may also be a base class).</typeparam>
        /// <param name="member">The type/method/property/field to find attributes on.</param>
        /// <returns>A list of matching attributes.</returns>
        public static List<TAttribute> GetAttributes<TAttribute>(MemberInfo member)
            where TAttribute : Attribute
        {
            return GetAttributes<TAttribute>(member, false);
        }

        /// <summary>
        /// Searches a type/method/property/field for attributes of the specified type, returning the first match.
        /// </summary>
        /// <typeparam name="TAttribute">The type of attribute (may also be a base class).</typeparam>
        /// <param name="member">The type/method/property/field to find attributes on.</param>
        /// <param name="inherit">True to include inherited attributes in the search.</param>
        /// <param name="filter">A filter that restricts the results of the search.</param>
        /// <returns>The first matching attribute instance, or null if no matches are found.</returns>
        public static TAttribute GetAttribute<TAttribute>(MemberInfo member, bool inherit, Predicate<TAttribute> filter)
            where TAttribute : Attribute
        {
            return (TAttribute)member.GetCustomAttributes(typeof(TAttribute), inherit).
                FirstOrDefault(a => filter(a as TAttribute));
        }

        /// <summary>
        /// Searches a type/method/property/field for attributes of the specified type, returning the first match.
        /// </summary>
        /// <typeparam name="TAttribute">The type of attribute (may also be a base class).</typeparam>
        /// <param name="member">The type/method/property/field to find attributes on.</param>
        /// <param name="inherit">True to include inherited attributes in the search.</param>
        /// <returns>The first matching attribute instance, or null if no matches are found.</returns>
        public static TAttribute GetAttribute<TAttribute>(MemberInfo member, bool inherit)
            where TAttribute : Attribute
        {
            return GetAttribute<TAttribute>(member, inherit, NullFilter);
        }

        /// <summary>
        /// Searches a type/method/property/field for attributes of the specified type, returning the first match.
        /// </summary>
        /// <typeparam name="TAttribute">The type of attribute (may also be a base class).</typeparam>
        /// <param name="member">The type/method/property/field to find attributes on.</param>
        /// <returns>The first matching attribute instance, or null if no matches are found.</returns>
        public static TAttribute GetAttribute<TAttribute>(MemberInfo member)
            where TAttribute : Attribute
        {
            return GetAttribute<TAttribute>(member, false);
        }

        /// <summary>
        /// Tests a type/method/property/field for the presence of an attribute of the specified type, and matching
        /// the specified filter.
        /// </summary>
        /// <typeparam name="TAttribute">The type of attribute (may also be a base class).</typeparam>
        /// <param name="member">The type/method/property/field to find attributes on.</param>
        /// <param name="inherit">True to include inherited attributes in the search.</param>
        /// <param name="filter">A filter that restricts the results of the search.</param>
        /// <returns>True if a match is found, otherwise false.</returns>
        public static bool HasAttribute<TAttribute>(MemberInfo member, bool inherit, Predicate<TAttribute> filter)
            where TAttribute : Attribute
        {
            return member.GetCustomAttributes<TAttribute>(inherit).Any(attr => filter(attr));
        }

        /// <summary>
        /// Tests a type/method/property/field for the presence of an attribute of the specified type.
        /// </summary>
        /// <typeparam name="TAttribute">The type of attribute (may also be a base class).</typeparam>
        /// <param name="member">The type/method/property/field to find attributes on.</param>
        /// <param name="inherit">True to include inherited attributes in the search.</param>
        /// <returns>True if a match is found, otherwise false.</returns>
        public static bool HasAttribute<TAttribute>(MemberInfo member, bool inherit)
            where TAttribute : Attribute
        {
            return HasAttribute<TAttribute>(member, inherit, NullFilter);
        }

        /// <summary>
        /// Tests a type/method/property/field for the presence of an attribute of the specified type.
        /// </summary>
        /// <typeparam name="TAttribute">The type of attribute (may also be a base class).</typeparam>
        /// <param name="member">The type/method/property/field to find attributes on.</param>
        /// <returns>True if a match is found, otherwise false.</returns>
        public static bool HasAttribute<TAttribute>(MemberInfo member)
           where TAttribute : Attribute
        {
            return HasAttribute<TAttribute>(member, false);
        }

        private static bool NullFilter(object obj)
        {
            return true;
        }
    }
}
