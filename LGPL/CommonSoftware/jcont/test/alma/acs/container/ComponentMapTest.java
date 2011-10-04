package alma.acs.container;

import java.util.logging.Logger;

import junit.framework.TestCase;

import org.omg.PortableServer.POA;

import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.acs.container.corba.AcsCorba;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;

/**
 * JUnit test for {@link alma.acs.container.ComponentMap}.
 * <p>
 * Tests this class in isolation using dummy implementations of other classes;
 * no ACS runtime environment needed.
 * 
 * @author hsommer
 */
public class ComponentMapTest extends TestCase {

	private ComponentMap componentMap;
	private AcsLogger logger;

	protected void setUp() throws Exception {
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(getName(), true);
		componentMap = new ComponentMap(logger);
		DummyComponentAdapter.resetIndex();
		DummyComponentAdapter.setDummyAcsCorba(new DummyAcsCorba(logger));
	}

	
	public void testInsertionOrderOutput() throws Exception {
		final int numCompAdapters = 21;
		for (int i = 0; i < numCompAdapters; i++) {
			DummyComponentAdapter dummyComponentAdapter = new DummyComponentAdapter(logger);
			componentMap.put(dummyComponentAdapter.getHandle(), dummyComponentAdapter);			
		}
		// now check the order as it comes out of the map (handle == index, by design of DummyComponentAdapter)
		ComponentAdapter[] outCompAdapters = componentMap.getAllComponentAdapters();
		int[] outHandles = componentMap.getAllHandles();
		assertEquals(numCompAdapters, outCompAdapters.length);
		for (int i = 0; i < outCompAdapters.length; i++) {
			assertEquals(i, outCompAdapters[i].getHandle());
			assertEquals(i, outHandles[i]);
		}
	}
	
	public void testSorting() throws Exception {
		final int numCompAdapters = 13;
		for (int i = 0; i < numCompAdapters; i++) {
			DummyComponentAdapter dummyComponentAdapter = new DummyComponentAdapter(logger);
			componentMap.put(dummyComponentAdapter.getHandle(), dummyComponentAdapter);			
		}
		// now sort according to a handle sequence that manager may provide 
		int[] sortedHandles = new int[] {11, 2, 7, 3, 10, 12, 1, 8, 5, 4, 6, 0, 9};
		componentMap.sort(sortedHandles);

		// now check the order as it comes out of the map (handle == index, by design of DummyComponentAdapter)
		ComponentAdapter[] outCompAdapters = componentMap.getAllComponentAdapters();
		int[] outHandles = componentMap.getAllHandles();
		assertEquals(numCompAdapters, outCompAdapters.length);
		assertEquals(numCompAdapters, outHandles.length);
		for (int i = 0; i < sortedHandles.length; i++) {
			assertEquals(sortedHandles[i], outCompAdapters[i].getHandle());
			assertEquals(sortedHandles[i], outHandles[i]);
		}
	}		
	
	private static class DummyComponentAdapter extends ComponentAdapter {
		static int index_count = 0;
		static DummyAcsCorba dummyAcsCorba;
		
		DummyComponentAdapter(AcsLogger logger) throws Exception {
			super("DummyComponent"+index_count, "DummyComponentType"+index_count, "dummycode", 
					index_count, "dummyContainerName", null, null, null, logger, dummyAcsCorba);
			index_count++;
		}

		static void setDummyAcsCorba(DummyAcsCorba dummyAcsCorba) {
			DummyComponentAdapter.dummyAcsCorba = dummyAcsCorba;
		}
		static void resetIndex() {
			index_count = 0;
		}
	}
	
	private static class DummyAcsCorba extends AcsCorba {

		public DummyAcsCorba(AcsLogger logger) {
			super(logger);
		}
		public POA createPOAForComponent(String compName) throws AcsJContainerEx {
			return null;
		}
		
	}
}
