using System.Linq;
using System.Text;

namespace uWS.Pacs.BussinessLogic
{
    public static class QueryHelper
    {
        public static string SetStringArrayCondition(string key, string[] vals)
        {
            if (vals == null || vals.Length == 0)
                return string.Empty;

            if (vals.Length == 1)
            {
                return string.Format(@"it.{0} = '{1}'", key, vals[0]);
            }
            else
            {
                var sb = new StringBuilder();
                sb.Append("[");
                foreach (string val in vals)
                {
                    sb.Append( "'" + val + "'" + ",");
                }
                sb.Remove(sb.Length - 1, 1);
                sb.Append("]");

                return string.Format("it.{0} in {1}", key, sb.ToString());
            }
        }

        public static string SetIntArrayCondition(string key, int[] vals)
        {
            if (vals == null || vals.Length == 0)
                return string.Empty;

            if (vals.Length == 1)
            {
                return string.Format("it.{0} = {1}", key, vals[0]);
            }
            else
            {
                var sb = new StringBuilder();
                sb.Append("[");
                foreach (int val in vals)
                {
                    sb.Append(val + ",");
                }
                sb.Remove(sb.Length - 1, 1);
                sb.Append("]");

                return string.Format("it.{0} in {1}", key, sb.ToString());
            }
        }

        public static string SetStringCondition(string key, string val)
        {
            if (val.Length == 0 || SearchValueOnlyWildcard(val, false))
            {
                return string.Empty;
            }

            if (val.Contains("*") || val.Contains("?"))
            {
                string value = val.Replace("%", "[%]").Replace("_", "[_]");
                value = value.Replace('*', '%');
                value = value.Replace('?', '_');

                return string.Format(@"it.{0} like '{1}'", key, value);
            }
            else
            {
                return string.Format(@"it.{0} = '{1}'", key, val);
            }
        }

        public static string SetRangeCondition(string key, string val)
        {
            if (val.Length == 0)
                return string.Empty;
            
            if (val.Contains("-"))
            {
                string[] vals = val.Split(new[] {'-'});
                if (val.IndexOf('-') == 0)
                {
                    // less than    
                    return string.Format(@"it.{0} <= '{1}'", key, vals[1]);
                }
                else if (val.IndexOf('-') == val.Length - 1)
                {
                    // more than
                    return string.Format(@"it.{0} >= '{1}'", key, vals[0]);
                }
                else
                {
                    // between 
                    return string.Format(@"it.{0} between '{1}' and '{2}'", key, vals[0], vals[1]);
                }
            }

            return string.Format(@"it.{0} = '{1}'", key, val);
        }

        /// <summary>
        /// Check to see if the search value only contains wildcard charcters and can be ommited from a select.
        /// </summary>
        /// <param name="val">The value to check</param>
        /// <param name="sqlWildcards"></param>
        /// <returns>True if <see cref="val"/> only contains wildcard charcater(s).</returns>
        private static bool SearchValueOnlyWildcard(string val, bool sqlWildcards)
        {
            string val2 = val.Trim();

            if (sqlWildcards)
            {
                return val2.All(c => c == '*' || c == '%');
            }
            else
            {
                return val2.All(c => c == '*');
            }
            return true;
        }
    }
}