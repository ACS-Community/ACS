/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.jlog.test;

import com.cosylab.logging.client.cache.LogMultiFileCache;

import junit.framework.TestCase;

/**
 * Test the LogMultiFileCache class
 * 
 * @author acaproni mcomin
 *
 */
public class MultiFileCacheTest extends TestCase {
	
	// The cache to test
	private LogMultiFileCache cache;
	
	// The size of the cache
	private static int TESTCACHE_SIZE=20000;
	
	/**
	 * Constructor
	 *
	 */
	public MultiFileCacheTest() throws Exception {
		super("MultiFileCacheTest");
	}
	
	
	public void setUp() throws Exception {
		super.setUp();
		cache=new LogMultiFileCache(TESTCACHE_SIZE);
		assertEqual("The cache size is not as expected",TESTCACHE_SIZE,cache.getMaxFileSize());
	}
	
	public void tearDown() throws Exception {
		super.tearDown();
		cache.clear();
		cache=null;
	}
	
	public void testFileCreation() throws Exception {	
	}
	
	public void testFileDeletion() throws Exception {
	}
		

}
