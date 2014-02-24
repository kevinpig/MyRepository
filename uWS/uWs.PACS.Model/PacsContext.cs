using System.Data.Entity;
using uWs.PACS.Model.Mapping;

namespace uWs.PACS.Model
{
    public partial class PacsContext : DbContext
    {
        public PacsContext()
            : base("Name=PacsContext")
        {
            
        }

        //public DbSet<Patient> Patients { get; set; }
        //public DbSet<Study> Studies { get; set; }
        //public DbSet<Series> Series { get; set; }
        //public DbSet<Instance> Instances { get; set; }

        //public DbSet<File> Files { get; set; } 

        /*
        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            modelBuilder.Configurations.Add(new PatientMap());
            modelBuilder.Configurations.Add(new StudyMap());
            modelBuilder.Configurations.Add(new SeriesMap());
            modelBuilder.Configurations.Add(new InstanceMap());
            modelBuilder.Configurations.Add(new FileMap());
        }
         * */
    }
}
