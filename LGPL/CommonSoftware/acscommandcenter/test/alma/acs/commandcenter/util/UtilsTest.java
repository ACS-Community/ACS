/*
 * Created on Dec 20, 2004 by mschilli
 */
package alma.acs.commandcenter.util;

import junit.framework.TestCase;



/**
 *
 * @author mschilli
 */
public class UtilsTest extends TestCase {

	public static void testStringRingBuffer() {
		StringRingBuffer inst = new StringRingBuffer(5);
		
		inst.add('1');
		assertTrue(inst.equals("1"));
		
		inst.add('2');
		assertTrue(inst.equals("12"));
		
		inst.add('3');
		assertTrue(inst.equals("123"));
		
		inst.add('4');
		assertTrue(inst.equals("1234"));
		
		inst.add('5');
		assertTrue(inst.equals("12345"));
		
		inst.add('6');
		assertTrue(inst.equals("23456"));

		inst.add('7');
		assertTrue(inst.equals("34567"));
		
		inst.add('8');
		assertTrue(inst.equals("45678"));
		
		inst.add('9');
		assertTrue(inst.equals("56789"));
		
		inst.add('A');
		assertTrue(inst.equals("6789A"));
		
		inst.add('B');
		assertTrue(inst.equals("789AB"));
	}
	
	
}

//
//
//
//
//
//
//
//
//
//
//
//