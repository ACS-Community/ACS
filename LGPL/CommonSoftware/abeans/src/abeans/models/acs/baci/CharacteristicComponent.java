/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci;

import java.beans.Beans;
import java.beans.PropertyChangeListener;
import java.util.Map;

import com.cosylab.datatypes.CharacteristicContext;
import com.cosylab.datatypes.DataExchangeException;
import com.cosylab.util.NameValueList;

import abeans.core.AssertionFailed;
import abeans.core.InitializationException;
import abeans.models.Family;
import abeans.pluggable.NarrowConstants;
import abeans.pluggable.Proxy;
import abeans.pluggable.RemoteException;
import abeans.pluggable.RemoteInfo;

/**
 * Base class representing a BACI device.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public class CharacteristicComponent extends Component implements CharacteristicContext, NarrowConstants {

	/**
	 * Default constructor.
	 */
	public CharacteristicComponent() {
		super();
	}

	/**
	 * @param parent
	 * @param info
	 * @throws InitializationException
	 */
	public CharacteristicComponent(Family parent, RemoteInfo info) throws InitializationException {
		super(parent, info);
	}

	/**
	 * @see abeans.models.Linkable#initialize(abeans.pluggable.Proxy)
	 */
	public synchronized void initialize(Proxy proxy)
		throws RemoteException, AssertionFailed {
		super.initialize(proxy);
	}

	/**
	 * @see com.cosylab.datatypes.CharacteristicContext#getCharacteristicNames()
	 */
	public String[] getCharacteristicNames() throws DataExchangeException {

		// design time case
		if (Beans.isDesignTime())
			return null;
		
		return (String[])InvokeUtilities.getCharacteristic(getRemoteInfo(), CHARACTERISTICS_QUERY, remote, getDefaultTimeout(), this, this);
	}

	/**
	 * @see com.cosylab.datatypes.CharacteristicContext#getCharacteristics(java.lang.String[])
	 */
	public Map getCharacteristics(String[] names) throws DataExchangeException {

		Map map = new NameValueList();

		if (names != null)
		{
			for (int i = 0; i < names.length; i++)
			{
				try {
					Object value = getCharacteristic(names[i]);
					map.put(names[i], value);
				} catch (DataExchangeException e) {	}
			}
		}

		return map;
	}

	/**
	 * @see com.cosylab.datatypes.CharacteristicContext#getCharacteristic(java.lang.String)
	 */
	public Object getCharacteristic(String name) throws DataExchangeException {
		return InvokeUtilities.getCharacteristic(getRemoteInfo(), name, remote, getDefaultTimeout(), this, this);
	}

	/**
	 * @see com.cosylab.datatypes.CharacteristicContext#addPropertyChangeListener(java.beans.PropertyChangeListener)
	 */
	public void addPropertyChangeListener(PropertyChangeListener l) {
		// noop
	}

	/**
	 * @see com.cosylab.datatypes.CharacteristicContext#removePropertyChangeListener(java.beans.PropertyChangeListener)
	 */
	public void removePropertyChangeListener(PropertyChangeListener l) {
		// noop
	}

	
	
	/**
	 * name read attribute
	 */
	public String name()
		throws abeans.engine.RequestException
	{
		Object __retVal = abeans.models.acs.baci.InvokeUtilities.invokeSync(null, "_get_name", getProxy(), 0, null, null, null);
		return ((String)__retVal);
	}

	/**
	 * componentState read attribute
	 */
	public alma.ACS.abeans.ComponentStates componentState()
		throws abeans.engine.RequestException
	{
		Object __retVal = abeans.models.acs.baci.InvokeUtilities.invokeSync(null, "_get_componentState", getProxy(), 0, null, null, null);
		return ((alma.ACS.abeans.ComponentStates)__retVal);
	}

	/**
	 * Operation get_characteristic_by_name
	 */
	public java.lang.Object get_characteristic_by_name(String name)
		throws abeans.engine.RequestException, alma.ACS.abeans.NoSuchCharacteristic
	{
		Object[] parameters = { name };
		Object __retVal = abeans.models.acs.baci.InvokeUtilities.invokeSync(null, "get_characteristic_by_name", getProxy(), 0, null, null, parameters);
		return ((java.lang.Object)__retVal);
	}

	/**
	 * Operation find_characteristic
	 */
	public String[] find_characteristic(String reg_exp)
		throws abeans.engine.RequestException
	{
		Object[] parameters = { reg_exp };
		Object __retVal = abeans.models.acs.baci.InvokeUtilities.invokeSync(null, "find_characteristic", getProxy(), 0, null, null, parameters);
		return ((String[])__retVal);
	}

	/**
	 * Operation get_all_characteristics
	 */
	public org.omg.CosPropertyService.abeans.PropertySet get_all_characteristics()
		throws abeans.engine.RequestException
	{
		Object __retVal = abeans.models.acs.baci.InvokeUtilities.invokeSync(null, "get_all_characteristics", getProxy(), 0, null, null, null);
		return ((org.omg.CosPropertyService.abeans.PropertySet)__retVal);
	}

	/**
	 * Operation descriptor
	 */
	public alma.ACS.abeans.CharacteristicComponentDesc descriptor()
		throws abeans.engine.RequestException
	{
		Object __retVal = abeans.models.acs.baci.InvokeUtilities.invokeSync(null, "descriptor", getProxy(), 0, null, null, null);
		return ((alma.ACS.abeans.CharacteristicComponentDesc)__retVal);
	}
	
	
}
