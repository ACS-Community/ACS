/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.cdb.dal;

import com.cosylab.datatypes.AbstractProperty;
import com.cosylab.datatypes.DataExchangeException;

import abeans.core.InitializationException;
import abeans.datatypes.CharacteristicContextUtilities;
import abeans.models.Family;
import abeans.models.channel.StringSeqChannel;
import abeans.pluggable.RemoteInfo;
import abeans.pluggable.acs.cdb.dal.Constants;

/**
 * Specificaton of StringSeqChannel to DAO objects.
 * Channel value returns a string sequence of its childer names.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class DAOChannel extends StringSeqChannel
{

	/**
	 * Constructor for DAOChannel.
	 */
	public DAOChannel()
	{
		super();
	}

	/**
	 * Constructor for DAOChannel.
	 * @param parent
	 * @param info
	 * @throws InitializationException
	 */
	public DAOChannel(Family parent, RemoteInfo info)
		throws InitializationException
	{
		super(parent, info);
	}

	/**
	 * Returns the length of the sequence (number of DAO fields).
	 * <code>getValue()</code> should be 'smartly' cached.
	 * 
	 * @see com.cosylab.datatypes.ntuples.SequenceAccess#getSequenceLength()
	 */
	public int getSequenceLength() throws DataExchangeException
	{
		return getValue().length;
	}

	/**
	 * Returns value of a double characteristic.
	 * @param	name	name of characteristic
	 * @return value of characteristic
	 * @throws DataExchangeException
	 */
	public double getDoubleCharacteristic(String name) throws DataExchangeException {
		return CharacteristicContextUtilities.getDoubleCharacteristic(getFlattenRemoteInfo(name, getRemoteInfo()), getLastName(name), remote, getDefaultTimeout(), this, this);
		//return CharacteristicContextUtilities.getDoubleCharacteristic(getRemoteInfo(), name, remote, getDefaultTimeout(), this, this);
	}
	
	/**
	 * Returns value of a long characteristic.
	 * @param	name	name of characteristic
	 * @return	value of characteristic
	 * @throws DataExchangeException
	 */
	public long getLongCharacteristic(String name) throws DataExchangeException {
		return CharacteristicContextUtilities.getLongCharacteristic(getFlattenRemoteInfo(name, getRemoteInfo()), getLastName(name), remote, getDefaultTimeout(), this, this);
		//return CharacteristicContextUtilities.getLongCharacteristic(getRemoteInfo(), name, remote, getDefaultTimeout(), this, this);
	}

	/**
	 * Returns value of a string characteristic.
	 * @param	name	name of characteristic
	 * @return	value of characteristic
	 * @throws DataExchangeException
	 */
	public String getStringCharacteristic(String name) throws DataExchangeException {
		return CharacteristicContextUtilities.getStringCharacteristic(getFlattenRemoteInfo(name, getRemoteInfo()), getLastName(name), remote, getDefaultTimeout(), this, this);
		//return CharacteristicContextUtilities.getStringCharacteristic(getRemoteInfo(), name, remote, getDefaultTimeout(), this, this);
	}
	
	/**
	 * Returns value of a sequence double characteristic.
	 * @param	name	name of characteristic
	 * @return	value of characteristic
	 * @throws DataExchangeException
	 */
	public double[] getDoubleSeqCharacteristic(String name) throws DataExchangeException {
		return SequenceCharacteristicContextUtilities.getDoubleSeqCharacteristic(getFlattenRemoteInfo(name, getRemoteInfo()), getLastName(name), remote, getDefaultTimeout(), this, this);
		//return SequenceCharacteristicContextUtilities.getDoubleSeqCharacteristic(getRemoteInfo(), name, remote, getDefaultTimeout(), this, this);
	}
	
	/**
	 * Returns value of a sequence long characteristic.
	 * @param	name	name of characteristic
	 * @return	value of characteristic
	 * @throws DataExchangeException
	 */
	/*
	public long[] getLongSeqCharacteristic(String name) throws DataExchangeException {
		return SequenceCharacteristicContextUtilities.getLongSeqCharacteristic(getFlattenRemoteInfo(name, getRemoteInfo()), getLastName(name), remote, getDefaultTimeout(), this, this);
		return SequenceCharacteristicContextUtilities.getLongSeqCharacteristic(getRemoteInfo(), name, remote, getDefaultTimeout(), this, this);
	}
	*/
	
	/**
	 * Returns value of a sequence int characteristic.
	 * @param	name	name of characteristic
	 * @return	value of characteristic
	 * @throws DataExchangeException
	 */
	public int[] getIntegerSeqCharacteristic(String name) throws DataExchangeException {
		return SequenceCharacteristicContextUtilities.getIntegerSeqCharacteristic(getFlattenRemoteInfo(name, getRemoteInfo()), getLastName(name), remote, getDefaultTimeout(), this, this);
		//return SequenceCharacteristicContextUtilities.getIntegerSeqCharacteristic(getRemoteInfo(), name, remote, getDefaultTimeout(), this, this);
	}

	/**
	 * Returns value of a sequence string characteristic.
	 * @param	name	name of characteristic
	 * @return	value of characteristic
	 * @throws DataExchangeException
	 */
	public String[] getStringSeqCharacteristic(String name) throws DataExchangeException {
		return SequenceCharacteristicContextUtilities.getStringSeqCharacteristic(getFlattenRemoteInfo(name, getRemoteInfo()), getLastName(name), remote, getDefaultTimeout(), this, this);
		//return SequenceCharacteristicContextUtilities.getStringSeqCharacteristic(getRemoteInfo(), name, remote, getDefaultTimeout(), this, this);
	}

	/**
	 * Returns value of a sequence string characteristic.
	 * @param	name	name of characteristic
	 * @return	value of characteristic
	 * @throws DataExchangeException
	 */
	public String[] getSubnodes() throws DataExchangeException {
		return SequenceCharacteristicContextUtilities.getStringSeqCharacteristic(getRemoteInfo(), Constants.SUBNODES, remote, getDefaultTimeout(), this, this);
	}

	/**
	 * @see com.cosylab.datatypes.CharacteristicContext#getCharacteristic(java.lang.String)
	 */
	public Object getCharacteristic(String name) throws DataExchangeException {
		if (AbstractProperty.C_DISPLAY_NAME.equals(name))
			return getIdentifier().getShortName();
			
		return CharacteristicContextUtilities.getCharacteristic(getFlattenRemoteInfo(name, getRemoteInfo()), getLastName(name), remote, getDefaultTimeout(), this, this);
		//return CharacteristicContextUtilities.getCharacteristic(getRemoteInfo(), name, remote, getDefaultTimeout(), this, this);
	}


	/**
	 * If name is a hierarchical name, e.g. MOUNT1/Name, MOUNT1 will be added to the newly created remote info. 
	 * @param name	(hierarchical) name.
	 * @param remoteInfo	remote info the be updated.
	 * @return	flattern remote info.
	 */
	private static RemoteInfo getFlattenRemoteInfo(String name, RemoteInfo remoteInfo)
	{
		if (name.indexOf('/') > 0)
		{
			String[] splitNames = name.split("/");
			for (int i = 0; i < splitNames.length - 1; i++)
				remoteInfo = remoteInfo.createHierarchy(splitNames[i]);
		}
		return remoteInfo;
	}
	
	/**
	 * Returns only the last name of the hierarchy, e.g. "MOUNT1/Name" -> "Name".
	 * @param name	(hierarchical) name.
	 * @return the last name of the hierarchy.
	 */
	private static String getLastName(String name)
	{
		int pos = name.lastIndexOf('/'); 
		if (pos > 0)
			return name.substring(pos+1);
		else 
			return name;
	}

	/**
	 * Returns a short summary about this object.
	 * 
	 * @return		the internal state of this
	 */
	public String toString()
	{
		StringBuffer sb = new StringBuffer(500);
		sb.append("DAOChannel = { ");
		if (getRemoteInfo()!=null)
		{
			sb.append("remoteInfo='");
			sb.append(getRemoteInfo().toString());
			sb.append("', ");
		}
		sb.append("destroyed = ");
		sb.append(String.valueOf(isDestroyed()));
		sb.append(" }");
		return new String(sb);
	}

	/**
	 * @see abeans.models.Connectable#reportConnectionStatus(short,
	 *      java.lang.Exception)
	 */
	// TODO remove - this is temporary workaround (to be fixed in abeans, Abean class)
	public void reportConnectionStatus(short connectionStatus, Exception ex)
	{
		if (getConnectionStatus() == CONNECTABLE_DESTROYED &&
			connectionStatus == CONNECTABLE_DISCONNECTED)
			return;
		
		super.reportConnectionStatus(connectionStatus, ex);
	}

}
