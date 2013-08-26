/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.test;

import com.cosylab.util.WildcharMatcher;

import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * JUnit Test for <code>WildcharMatcher</code> class.
 * This is only a add-on test, see <code>com.cosylab.util.test.WildcharMatcherTest</code> for full test.
 * 
 * @author		Ales Pucelj (ales.pucelj@cosylab.com)
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class WildcharMatcherTest extends TestCase
{

	/**
	 */
	public WildcharMatcherTest(String name)
	{
		super(name);
	}
	
	/**
	 */
	public static TestSuite suite()
	{
		return new TestSuite(WildcharMatcherTest.class);
	}
	
	/**
	 */
	public void testWildcharMatcher()
	{
		//assertTrue(WildcharMatcher.match(null, ""));
		//assertTrue(WildcharMatcher.match("", null));
		//assertTrue(WildcharMatcher.match(null, null));

		assertTrue(WildcharMatcher.match("", ""));

		assertTrue(WildcharMatcher.match("*", ""));
		assertTrue(WildcharMatcher.match("*", "whatever"));
		assertTrue(WildcharMatcher.match("some*thing", "something"));

		assertTrue(!WildcharMatcher.match("something", "somethingLonger"));
		assertTrue(!WildcharMatcher.match("somethingLonger", "something"));

		assertTrue(!WildcharMatcher.match("?", ""));
		assertTrue(WildcharMatcher.match("?", "a"));
		assertTrue(!WildcharMatcher.match("?", "av"));
		
		assertTrue(!WildcharMatcher.match("[[[*?", ""));
		assertTrue(!WildcharMatcher.match("[[[*?", "a"));

		assertTrue(WildcharMatcher.match("*Mount*", "Mount1"));

		assertTrue(!WildcharMatcher.match("[!abc]*a[def]", "xyzbd"));
		assertTrue(WildcharMatcher.match("[!abc]*a[def]", "xyzad"));
		assertTrue(WildcharMatcher.match("[a-g]l*i?n", "florian"));
		assertTrue(WildcharMatcher.match("[!abc]*e", "smile"));
		assertTrue(WildcharMatcher.match("[-z]", "a"));
		assertTrue(!WildcharMatcher.match("[]", ""));
		assertTrue(WildcharMatcher.match("[a-z]*", "java"));
		assertTrue(WildcharMatcher.match("*.*", "command.com"));
		assertTrue(!WildcharMatcher.match("*.*", "/var/etc"));
		assertTrue(WildcharMatcher.match("**?*x*[abh-]*Q", "XYZxabbauuZQ"));
	}
}
