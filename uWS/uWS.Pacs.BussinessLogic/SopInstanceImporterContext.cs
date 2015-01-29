using uWs.PACS.Model;

namespace uWS.Pacs.BussinessLogic
{
    public class SopInstanceImporterContext
    {
        #region Private Member

        private readonly string _contextId;

        private readonly string _sourceAe;

        private readonly ServerPartition _partition;

        #endregion

        #region Construtor

        /// <summary>
        /// create a instance of <see cref="SopInstanceImporterContext" /> to be used 
        /// by <see cref="SopInstanceImporter"/>
        /// </summary>
        /// <param name="contextID"></param>
        /// <param name="sourceAE">source ae title of the image(s) to be import</param>
        /// <param name="partition">which the image(s) will be import to</param>
        public SopInstanceImporterContext(string contextID, string sourceAE, ServerPartition partition)
        {
            _contextId = contextID;
            _sourceAe = sourceAE;
            _partition = partition;
        }

        #endregion

        /// <summary>
        /// Gets the ID of this context
        /// </summary>
        public string ContextID
        {
            get { return _contextId; }
        }

        /// <summary>
        /// Gets the source AE title where the image(s) are imported from
        /// </summary>
        public string SourceAE
        {
            get { return _sourceAe; }
        }

        /// <summary>
        /// Gets <see cref="ServerPartition"/> where the image(s) will be imported to
        /// </summary>
        public ServerPartition Partition
        {
            get { return _partition; }
        }
    }
}