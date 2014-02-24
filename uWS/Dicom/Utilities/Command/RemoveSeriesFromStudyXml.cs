#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

using System;
using uWS.Common;
using uWS.Dicom.Utilities.Xml;

namespace uWS.Dicom.Utilities.Command
{
    public class RemoveSeriesFromStudyXml : CommandBase
    {
        private readonly StudyXml _studyXml;
        private readonly string _seriesUid;
        private SeriesXml _oldSeriesXml;
        private readonly string _studyInstanceUid;

        public RemoveSeriesFromStudyXml(StudyXml studyXml, string seriesUid)
            : base(String.Format("Remove series {0} from study XML of study {1}", seriesUid, studyXml.StudyInstanceUid), true)
        {
            _studyXml = studyXml;
            _seriesUid = seriesUid;
            _studyInstanceUid = studyXml.StudyInstanceUid;
        }

        protected override void OnExecute(CommandProcessor theProcessor)
        {
            // backup
            if (_studyXml.Contains(_seriesUid))
            {
                Platform.Log(LogLevel.Info, "Removing series {0} from StudyXML for study {1}", _seriesUid, _studyInstanceUid);
                _oldSeriesXml = _studyXml[_seriesUid];
                if (!_studyXml.RemoveSeries(_seriesUid))
                    throw new ApplicationException(String.Format("Could not remove series {0} from study {1}", _seriesUid, _studyInstanceUid));
            }
        }

        protected override void OnUndo()
        {
            if (_oldSeriesXml != null)
            {
                Platform.Log(LogLevel.Info, "Restoring series {0} in StudyXML for study {1}", _seriesUid, _studyInstanceUid);
                _studyXml[_seriesUid] = _oldSeriesXml;
            }
        }
    }
}
