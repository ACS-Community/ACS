package alma.acs.jlog.test;

import java.util.Vector;

import com.cosylab.logging.engine.ACS.EngineCache;

import junit.framework.TestCase;

/**
 * A class to test <code>EngineCache</code>.
 * 
 * @author acaproni
 *
 */
public class EngineCacheTest extends TestCase {
	
	// The size of each file of the cache
	private static final int CACHE_SIZE=30000;
	
	// The cache to test
	private EngineCache cache;
	
	public EngineCacheTest() {
		super("EngineCacheTest");
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		// Build the cache
		cache = new EngineCache(CACHE_SIZE);
		assertNotNull(cache);
		assertEquals(0, cache.getActiveFilesSize());
		assertEquals(0, cache.size());
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#tearDown()
	 */
	@Override
	protected void tearDown() throws Exception {
		cache.close();
		super.tearDown();
	}
	
	/**
	 * Generate the strings to put in the cache.
	 * 
	 * The number of strings to put in the vector depends by the passed
	 * parameter. The sum of the length of all the strings in the
	 * vector is equal or greater to the passed parameter.
	 * In this way it is possible to check if the cache creates/deletes a
	 * file in the right moment.
	 *  
	 * @param size The length of all the strings in the vector
	 * @return A vector of strings
	 * 
	 */
	private Vector<String> generateStrings(int size) {
		Vector<String> strings = new Vector<String>();
		int currentSz=0;
		long t=0;
		while (currentSz<size) {
			String str = ""+(t++);
			currentSz+=str.length();
			strings.add(str);
		}
		return strings;
	}
	
	/**
	 * Test the normal behavior of the cache by pushing and getting
	 * strings into the cache without involving the creation of more
	 * then on file.
	 * 
	 * @throws Exception
	 */
	public void testPushPopSingleFile() throws Exception {
		Vector<String> items = generateStrings(CACHE_SIZE/2);
		// Push the string in the cache
		for (String str: items) {
			cache.push(str);
		}
		assertEquals(items.size(), cache.size());
		assertEquals(1, cache.getActiveFilesSize());
		
		for (int t=0; t<items.size(); t++) {
			String str = cache.pop(200);
			// str is null if the cache is empty
			assertNotNull(str);
			assertEquals(items.get(t), str);
		}
		// No more items and files in cache
		assertEquals(0, cache.size());
		assertEquals(1, cache.getActiveFilesSize());
	}
	
	/**
	 * Test the normal behavior of the cache by pushing and getting
	 * strings into the cache involving the creation of several files.
	 * 
	 * @throws Exception
	 */
	public void testPushPopSeveralFiles() throws Exception {
		// Create a vector of strings the trigger the cache to create 4 files
		Vector<String> items = generateStrings(3*CACHE_SIZE+CACHE_SIZE/2);
		// Push the string in the cache
		for (String str: items) {
			cache.push(str);
		}
		assertEquals(items.size(), cache.size());
		assertEquals(4, cache.getActiveFilesSize());
		
		for (int t=0; t<items.size(); t++) {
			String str = cache.pop(200);
			// str is null if the cache is empty
			assertNotNull(str);
			assertEquals(items.get(t), str);
		}
		// No more items and files in cache
		assertEquals(0, cache.size());
		assertEquals(1, cache.getActiveFilesSize());
	}

}
