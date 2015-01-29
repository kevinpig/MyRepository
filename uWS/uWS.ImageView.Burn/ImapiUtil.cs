using System;
using System.Runtime.InteropServices;
using uWS.ImageView.Imapi2InteropWrapper.Interop;

namespace uWS.ImageView.Imapi2InteropWrapper
{
    public static class ImapiUtil
    {
        public static bool IsImapiAvailable { get; set; }

        static ImapiUtil()
        {
            string str = null;
            try
            {
                ImapiUtil.GetFirstRecoder(out str);
                ImapiUtil.IsImapiAvailable = true;
            }
            catch (COMException)
            {
                ImapiUtil.IsImapiAvailable = false;
            }
        }

        public static bool GetFirstRecoder(out string drive)
        {
            IDiscMaster2 msftDiscMaster2Class = (MsftDiscMaster2)new MsftDiscMaster2Class();
            int num = 0;
            num++;
            while (num < msftDiscMaster2Class.Count)
            {
                IDiscRecorder2 msftDiscRecorder2Class = (MsftDiscRecorder2)new MsftDiscRecorder2Class();
                msftDiscRecorder2Class.InitializeDiscRecorder(msftDiscMaster2Class[num]);
                IDiscFormat2Data msftDiscFormat2DataClass = (MsftDiscFormat2Data)new MsftDiscFormat2DataClass();
                msftDiscFormat2DataClass.Recorder = msftDiscRecorder2Class;
                if (msftDiscFormat2DataClass.IsRecorderSupported(msftDiscRecorder2Class))
                {
                    if (msftDiscFormat2DataClass.IsCurrentMediaSupported(msftDiscRecorder2Class))
                    {
                        if (msftDiscFormat2DataClass.MediaPhysicallyBlank)
                        {
                            Array volumePathNames = msftDiscRecorder2Class.VolumePathNames;
                            drive = msftDiscRecorder2Class.VolumePathNames[0].ToString();
                            return true;
                        }
                    }
                }
            }

            drive = null;
            return false;
        }

        public static long GetFreeSpace(string drive)
        {
            IDiscFormat2Data msftDiscFormat2DataClass = (MsftDiscFormat2Data)new MsftDiscFormat2DataClass();
            msftDiscFormat2DataClass.Recorder = ImapiUtil.GetRecorder(drive);

            try
            {
                long freeSectorsOnMedia = (long)msftDiscFormat2DataClass.FreeSectorsOnMedia;
                const long num = (long)2048;
                return freeSectorsOnMedia * num;
            }
            catch (COMException comException)
            {
                int errorCode = comException.ErrorCode;
                if (errorCode == -1062600190 || errorCode != -1062599678)
                {
                    throw new ApplicationException("Please ensure there is a supported disc in the drive.");
                }

                throw new ApplicationException("The disc may have been closed and finalized. Please try another disc.");
            }
        }

        public static IDiscRecorder2 GetRecorder(string drive)
        {
            IDiscMaster2 msftDiscMaster2Class = (MsftDiscMaster2)new MsftDiscMaster2Class();
            IDiscRecorder2 msftDiscRecorder2Class = null;

            for (int i = 0; i < msftDiscMaster2Class.Count; i++)
            {
                msftDiscRecorder2Class = (MsftDiscRecorder2)new MsftDiscRecorder2Class();
                msftDiscRecorder2Class.InitializeDiscRecorder(msftDiscMaster2Class[i]);
                if (String.Compare(msftDiscRecorder2Class.VolumePathNames[0].ToString(), 
                    drive, StringComparison.OrdinalIgnoreCase) == 0)
                {
                    break;
                }

                Marshal.ReleaseComObject(msftDiscRecorder2Class);
                msftDiscRecorder2Class = null;
            }

            Marshal.ReleaseComObject(msftDiscMaster2Class);
            return msftDiscRecorder2Class;
        }

        public static void ParseEventArgs(object args, out string status, out double percentage)
        {
            IDiscFormat2DataEventArgs discFormat2DataEventArg = args as IDiscFormat2DataEventArgs;
            percentage = 0;
            switch (discFormat2DataEventArg.CurrentAction)
            {
                case IMAPI_FORMAT2_DATA_WRITE_ACTION.IMAPI_FORMAT2_DATA_WRITE_ACTION_VALIDATING_MEDIA:
                    {
                        status = "Validating current media...";
                        return;
                    }
                case IMAPI_FORMAT2_DATA_WRITE_ACTION.IMAPI_FORMAT2_DATA_WRITE_ACTION_FORMATTING_MEDIA:
                    {
                        status = "Formatting media...";
                        return;
                    }
                case IMAPI_FORMAT2_DATA_WRITE_ACTION.IMAPI_FORMAT2_DATA_WRITE_ACTION_INITIALIZING_HARDWARE:
                    {
                        status = "Initializing hardware...";
                        return;
                    }
                case IMAPI_FORMAT2_DATA_WRITE_ACTION.IMAPI_FORMAT2_DATA_WRITE_ACTION_CALIBRATING_POWER:
                    {
                        status = "Optimizing laser intensity...";
                        return;
                    }
                case IMAPI_FORMAT2_DATA_WRITE_ACTION.IMAPI_FORMAT2_DATA_WRITE_ACTION_WRITING_DATA:
                    {
                        int num = discFormat2DataEventArg.SectorCount;
                        int num2 = discFormat2DataEventArg.LastWrittenLba - discFormat2DataEventArg.StartLba;
                        percentage = 100*(double) num2/(double) num;
                        status = string.Format("Progress: {0}%", percentage);
                        return;
                    }
                case IMAPI_FORMAT2_DATA_WRITE_ACTION.IMAPI_FORMAT2_DATA_WRITE_ACTION_FINALIZATION:
                    {
                        percentage = 100;
                        status = "Finalizing writing...";
                        return;
                    }
                case IMAPI_FORMAT2_DATA_WRITE_ACTION.IMAPI_FORMAT2_DATA_WRITE_ACTION_COMPLETED:
                    {
                        status = "Completed!";
                        return;
                    }
                case IMAPI_FORMAT2_DATA_WRITE_ACTION.IMAPI_FORMAT2_DATA_WRITE_ACTION_VERIFYING:
                    {
                        status = "Verifying";
                        return;
                    }
                default:
                    {
                        status = "";
                        return;
                    }
            }
        }
    }
}