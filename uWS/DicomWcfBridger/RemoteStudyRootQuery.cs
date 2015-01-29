using System;
using System.Collections.Generic;
using System.ServiceModel;
using uWS.Dicom.Iod;
using uWS.Dicom.ServiceModel;
using uWS.Dicom.ServiceModel.Query;

namespace DicomWcfBridger
{
    public class RemoteStudyRootQuery : IStudyRootQuery, IDisposable
    {
        private readonly IApplicationEntity _remoteServer;
        private DicomStudyRootQuery _real;

        public RemoteStudyRootQuery(IApplicationEntity remoteServer)
        {
            _remoteServer = remoteServer;
            _real = new DicomStudyRootQuery("lrf", remoteServer);
        }

        /// <summary>
        /// Performs a STUDY level query.
        /// </summary>
        /// <exception cref="FaultException{TDetail}">Thrown when some part of the data in the request is poorly formatted.</exception>
        /// <exception cref="FaultException{QueryFailedFault}">Thrown when the query fails.</exception>
        public IList<StudyRootStudyIdentifier> StudyQuery(StudyRootStudyIdentifier queryCriteria)
        {
            var results = _real.StudyQuery(queryCriteria);

            return results;
        }

        /// <summary>
        /// Performs a SERIES level query.
        /// </summary>
        /// <exception cref="FaultException">Thrown when some part of the data in the request is poorly formatted.</exception>
        /// <exception cref="FaultException{QueryFailedFault}">Thrown when the query fails.</exception>
        public IList<SeriesIdentifier> SeriesQuery(SeriesIdentifier queryCriteria)
        {
            var results = _real.SeriesQuery(queryCriteria);

            return results;
        }

        /// <summary>
        /// Performs an IMAGE level query.
        /// </summary>
        /// <exception cref="FaultException{DataValidationFault}">Thrown when some part of the data in the request is poorly formatted.</exception>
        /// <exception cref="FaultException{QueryFailedFault}">Thrown when the query fails.</exception>
        public IList<ImageIdentifier> ImageQuery(ImageIdentifier queryCriteria)
        {
            var results = _real.ImageQuery(queryCriteria);
            
            return results;
        }

        #region IDisposable Members

        public void Dispose()
        {
            if (_real == null)
                return;

            _real.Dispose();
            _real = null;
        }

        #endregion
    }

    public class WcfStudyRootQuery : IStudyRootQuery, IDisposable
    {
        private readonly IApplicationEntity _remoteServer;
        private DicomStudyRootQuery _real;

        public WcfStudyRootQuery( )
        {
            var ae = new ApplicationEntity
                {
                    AETitle = "UIHPACSSERVER",
                    ScpParameters = new ScpParameters("localhost", 3333)
                };
            _remoteServer = ae;
            _real = new DicomStudyRootQuery("lrf", _remoteServer);
        }

        /// <summary>
        /// Performs a STUDY level query.
        /// </summary>
        /// <exception cref="FaultException{TDetail}">Thrown when some part of the data in the request is poorly formatted.</exception>
        /// <exception cref="FaultException{QueryFailedFault}">Thrown when the query fails.</exception>
        public IList<StudyRootStudyIdentifier> StudyQuery(StudyRootStudyIdentifier queryCriteria)
        {
            var results = _real.StudyQuery(queryCriteria);

            return results;
        }

        /// <summary>
        /// Performs a SERIES level query.
        /// </summary>
        /// <exception cref="FaultException">Thrown when some part of the data in the request is poorly formatted.</exception>
        /// <exception cref="FaultException{QueryFailedFault}">Thrown when the query fails.</exception>
        public IList<SeriesIdentifier> SeriesQuery(SeriesIdentifier queryCriteria)
        {
            var results = _real.SeriesQuery(queryCriteria);

            return results;
        }

        /// <summary>
        /// Performs an IMAGE level query.
        /// </summary>
        /// <exception cref="FaultException{DataValidationFault}">Thrown when some part of the data in the request is poorly formatted.</exception>
        /// <exception cref="FaultException{QueryFailedFault}">Thrown when the query fails.</exception>
        public IList<ImageIdentifier> ImageQuery(ImageIdentifier queryCriteria)
        {
            var results = _real.ImageQuery(queryCriteria);
            
            return results;
        }

        #region IDisposable Members

        public void Dispose()
        {
            if (_real == null)
                return;

            _real.Dispose();
            _real = null;
        }

        #endregion
    }
}