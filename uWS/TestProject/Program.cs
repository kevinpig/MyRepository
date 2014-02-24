using System;
using System.Collections.Generic;
using System.Data.Entity.Core.Objects;
using System.Data.Entity.Infrastructure;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using uWs.PACS.Model;

namespace TestProject
{
    public class Program
    {
        static void Main(string[] args)
        {
            using (var pacsContext = new PacsContext())
            {
                var adapter = (IObjectContextAdapter)pacsContext;
                var query = new ObjectQuery<Patient>("Patients", adapter.ObjectContext);

                var newquery = query.Where("1 = 1").Where("it.PatientName like @name", new ObjectParameter("name", "11"));

                Console.WriteLine(newquery.ToTraceString());
                Console.WriteLine(newquery.CommandText);

                Console.WriteLine("##########################");

                newquery = newquery.OrderBy("it.Id");

                Console.WriteLine(newquery.ToTraceString());
                Console.WriteLine(newquery.CommandText);

                Console.WriteLine("##########################");

                //query = query.Skip("it.Id", "@count", new ObjectParameter("count", 10)).OrderBy("@count");

                //Console.WriteLine(query.ToTraceString());
                //Console.WriteLine(query.ToTraceString());

                //Console.WriteLine("##########################");

                Console.Read();
            }
        }
    }
}
