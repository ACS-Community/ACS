package alma.acs.tmcdb;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import junit.framework.TestCase;

public class TestPojosPublishingMechanism extends TestCase {

	private boolean changes[] = new boolean[10];

	private class MyPropertyListener implements PropertyChangeListener {
		private int i = 0;
		public void propertyChange(PropertyChangeEvent evt) { changes[i++] = true; }
		public int getI() { return i; }
	}

	public void setUp() {
		for(int i=0; i!=10; i++)
			changes[i] = false;
	}

	public void testChangeListener() throws Exception {

		MyPropertyListener listener = new MyPropertyListener(); 

		// Add 10 times the same listener, for different properties
		Component comp = new Component();
		comp.addPropertyChangeListener("container", listener);
		comp.addPropertyChangeListener("componentType", listener);
		comp.addPropertyChangeListener("configuration", listener);
		comp.addPropertyChangeListener("componentName", listener);
		comp.addPropertyChangeListener("implLang", listener);
		comp.addPropertyChangeListener("realTime", listener);
		comp.addPropertyChangeListener("code", listener);
		comp.addPropertyChangeListener("path", listener);
		comp.addPropertyChangeListener("isAutostart", listener);
		comp.addPropertyChangeListener("isDefault", listener);

		// Set these 10 properties
		comp.setContainer(new Container());
		comp.setComponentType(new ComponentType());
		comp.setConfiguration(new Configuration());
		comp.setComponentName("");
		comp.setImplLang(ComponentImplLang.CPP);
		comp.setRealTime(true);
		comp.setCode("");
		comp.setPath("");
		comp.setIsAutostart(true);
		comp.setIsDefault(true);

		// Check that the listener actually changed them
		assertEquals(10, listener.getI());
		for(int i=0; i!= 10; i++)
			assertTrue(changes[i]);
	}
}
