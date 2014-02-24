using uWS.Dicom.Network.Scu;
using uWs.PACS.Model;

namespace uWS.Pacs.DicomService
{
    public class PacsStorageScu : StorageScu
    {
        #region Private Fileds 

        private readonly Device _remoteDevice;

        #endregion

        #region Constructor

        public PacsStorageScu(ServerPartition partition, Device remoteDevice)
             : base(partition.AeTitle, remoteDevice.AeTitle, remoteDevice.Hostname, remoteDevice.Port)
        {
            _remoteDevice = remoteDevice;
        }

        public PacsStorageScu(ServerPartition partition, Device remoteDevice, string moveOriginatorAe,
                              ushort moveOrginatorMessageId)
            : base(
                partition.AeTitle, remoteDevice.AeTitle, remoteDevice.Hostname, remoteDevice.Port,
                moveOriginatorAe, moveOrginatorMessageId)
        {
            _remoteDevice = remoteDevice;
        }

        #endregion

        #region Public Method 



        #endregion


    }
}