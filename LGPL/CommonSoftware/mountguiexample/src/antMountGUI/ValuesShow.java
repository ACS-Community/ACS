/*
 * Created on Feb 17, 2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package antMountGUI;

import java.util.Map;

import com.cosylab.gui.displayers.DataConsumer;
import com.cosylab.gui.displayers.StringConsumer;
import com.cosylab.gui.displayers.DataSourceSupport;
import com.cosylab.gui.displayers.DataState;
import com.cosylab.gui.displayers.LongSeqConsumer;
import com.cosylab.gui.displayers.DoubleConsumer;
import com.cosylab.util.CommonException;

/**
 * @author acaproni
 *
 * Takes the sequence of long and show the number in the degree format
 * 
 * @invariant delegate!=null
 */
public class ValuesShow extends DataSourceSupport implements LongSeqConsumer {

	StringConsumer delegate;


	/** 
	 * Build an object
	 * 
	 * @param disp
	 * 
	 * @pre disp!=null
	 */
	public ValuesShow(StringConsumer disp) {
		super(new Class[] { StringConsumer.class });
		delegate=disp;
		
	}
	
	/* (non-Javadoc)
	 * @see com.cosylab.gui.displayers.LongSeqConsumer#updateValue(long, long[])
	 */
	public void updateValue(long timestamp, long[] seq) throws CommonException {
		delegate.updateValue(timestamp, String.valueOf(seq[0]+"."+seq[1]));
	}

	/* (non-Javadoc)
	 * @see com.cosylab.gui.displayers.DataConsumer#getDataConsumer(java.lang.Class)
	 */
	/**
	 * @pre type!=null
	 */
	public DataConsumer getDataConsumer(Class type) {
		if (!type.isAssignableFrom(this.getClass())) {
			return null;
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.gui.displayers.DataConsumer#getDefaultDataConsumer()
	 */
	public DataConsumer getDefaultDataConsumer() {
		return this;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.gui.displayers.DataConsumer#updateDataState(com.cosylab.gui.displayers.DataState)
	 */
	public void updateDataState(DataState state) {
		delegate.updateDataState(state);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.gui.displayers.DataConsumer#setCharacteristics(java.util.Map)
	 */
	public void setCharacteristics(Map attributes) {
		delegate.setCharacteristics(attributes);

	}

	/* (non-Javadoc)
	 * @see com.cosylab.gui.displayers.DataConsumer#getName()
	 */
	public String getName() {
		return delegate.getName();
	}

	/* (non-Javadoc)
	 * @see com.cosylab.gui.displayers.DataConsumer#getSupportedCharacteristics()
	 */
	public String[] getSupportedCharacteristics() {
		return delegate.getSupportedCharacteristics();
	}

	/* (non-Javadoc)
	 * @see com.cosylab.gui.displayers.DataConsumer#getSupportedConsumerTypes()
	 */
	public Class[] getSupportedConsumerTypes() {
		return new Class[] { DoubleConsumer.class };
	}

}
