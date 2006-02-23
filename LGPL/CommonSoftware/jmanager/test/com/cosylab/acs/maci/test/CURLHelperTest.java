/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.test;

import java.net.URI;
import java.net.URISyntaxException;

import com.cosylab.acs.maci.manager.CURLHelper;

import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * JUnit Test for CURLHelper class.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class CURLHelperTest extends TestCase
{

	/**
	 */
	public CURLHelperTest(String name)
	{
		super(name);
	}
	
	/**
	 */
	public static TestSuite suite()
	{
		return new TestSuite(CURLHelperTest.class);
	}
	

	/**
	 * Test CURLHelper.createURI() method.
	 */
	public void testCreateURI()
	{
		try
		{
			CURLHelper.createURI(null);
			fail();
		}
		catch (URISyntaxException usi)
		{
			// expected behaviour
		}
		
		try
		{
			CURLHelper.createURI("");
			fail();
		}
		catch (URISyntaxException usi)
		{
			// expected behaviour
		}
		
		// only domain
		try
		{
			URI uri = CURLHelper.createURI("curl://nrao");
			assertTrue(uri.getScheme().equals("curl"));
			assertTrue(uri.getAuthority().equals("nrao"));
			assertEquals(0, uri.getPath().length());

			System.out.println(uri.toString());
		}
		catch (URISyntaxException usi)
		{
			fail(usi.getMessage());
		}

		// only name CURL
		try
		{
			URI uri = CURLHelper.createURI("MOUNT1");
			assertTrue(uri.getScheme().equals(CURLHelper.CURL_SCHEME));
			assertEquals(null, uri.getAuthority());
			assertTrue(uri.getPath().equals("/MOUNT1"));

			System.out.println(uri.toString());
		}
		catch (URISyntaxException usi)
		{
			fail(usi.getMessage());
		}

		// simple non-domain CURL
		try
		{
			URI uri = CURLHelper.createURI("curl:///MOUNT1");
			assertTrue(uri.getScheme().equals(CURLHelper.CURL_SCHEME));
			assertEquals(null, uri.getAuthority());
			assertTrue(uri.getPath().equals("/MOUNT1"));

			System.out.println(uri.toString());
		}
		catch (URISyntaxException usi)
		{
			fail(usi.getMessage());
		}

		// complex CURL
		try
		{
			URI uri = CURLHelper.createURI("curl://user@cosylab.com:1208/MOUNT1/SUBDO");
			assertTrue(uri.getScheme().equals(CURLHelper.CURL_SCHEME));
			assertTrue(uri.getAuthority().equals("user@cosylab.com:1208"));
			assertEquals(1208, uri.getPort());
			assertTrue(uri.getPath().equals("/MOUNT1/SUBDO"));
			
			System.out.println(uri.toString());
		}
		catch (URISyntaxException usi)
		{
			fail(usi.getMessage());
		}

		// only-name, valid hierachical
		try
		{
			URI uri = CURLHelper.createURI("BUILDING1/FRONTDOOR");
			assertTrue(uri.getScheme().equals(CURLHelper.CURL_SCHEME));
			assertEquals(null, uri.getAuthority());
			assertTrue(uri.getPath().equals("/BUILDING1/FRONTDOOR"));
			
			System.out.println(uri.toString());
		}
		catch (URISyntaxException usi)
		{
			fail(usi.getMessage());
		}

		// only-name, obsolete hierachical
		try
		{
			URI uri = CURLHelper.createURI("BUILDING1:FRONTDOOR");
			assertTrue(uri.getScheme().equals(CURLHelper.CURL_SCHEME));
			assertEquals(null, uri.getAuthority());
			assertTrue(uri.getPath().equals("/BUILDING1:FRONTDOOR"));
			
			System.out.println(uri.toString());
		}
		catch (URISyntaxException usi)
		{
			fail(usi.getMessage());
		}

		// only-name, valid hierachical
		try
		{
			URI uri = CURLHelper.createURI("BUILDING1/FRONTDOOR/HANDLE");
			assertTrue(uri.getScheme().equals(CURLHelper.CURL_SCHEME));
			assertEquals(null, uri.getAuthority());
			assertTrue(uri.getPath().equals("/BUILDING1/FRONTDOOR/HANDLE"));
			
			System.out.println(uri.toString());
		}
		catch (URISyntaxException usi)
		{
			fail(usi.getMessage());
		}

		// only-name, obsolete hierachical
		try
		{
			URI uri = CURLHelper.createURI("BUILDING1:FRONTDOOR:HANDLE");
			assertTrue(uri.getScheme().equals(CURLHelper.CURL_SCHEME));
			assertEquals(null, uri.getAuthority());
			assertTrue(uri.getPath().equals("/BUILDING1:FRONTDOOR:HANDLE"));
			
			System.out.println(uri.toString());
		}
		catch (URISyntaxException usi)
		{
			fail(usi.getMessage());
		}

		// valid hierachical non-domain CURL
		try
		{
			URI uri = CURLHelper.createURI("curl:///BUILDING1/FRONTDOOR/HANDLE");
			assertTrue(uri.getScheme().equals("curl"));
			assertEquals(null, uri.getAuthority());
			assertTrue(uri.getPath().equals("/BUILDING1/FRONTDOOR/HANDLE"));
			
			System.out.println(uri.toString());
		}
		catch (URISyntaxException usi)
		{
			fail(usi.getMessage());
		}

		// obsolete hierachical non-domain CURL
		try
		{
			URI uri = CURLHelper.createURI("curl:///BUILDING1:FRONTDOOR:HANDLE");
			assertTrue(uri.getScheme().equals("curl"));
			assertEquals(null, uri.getAuthority());
			assertTrue(uri.getPath().equals("/BUILDING1:FRONTDOOR:HANDLE"));
			
			System.out.println(uri.toString());
		}
		catch (URISyntaxException usi)
		{
			fail(usi.getMessage());
		}

		// valid hierachical non-domain CURL
		try
		{
			URI uri = CURLHelper.createURI("curl://nrao/BUILDING1/FRONTDOOR/HANDLE");
			assertTrue(uri.getScheme().equals("curl"));
			assertTrue(uri.getAuthority().equals("nrao"));
			assertTrue(uri.getPath().equals("/BUILDING1/FRONTDOOR/HANDLE"));
			
			System.out.println(uri.toString());
		}
		catch (URISyntaxException usi)
		{
			fail(usi.getMessage());
		}

	}
	
}
