/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2012 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.alarmsystem.dumpers.test;

import java.util.concurrent.TimeUnit;

import alma.alarmsystem.dump.AlarmDumperBase;
import alma.alarmsystem.dump.AlarmSourceDumper;

/**
 * Test the {@link AlarmSourceDumper}.
 * 
 * @see AlarmSenderForDumperTest for a description of the test.
 * 
 * @author acaproni
 *
 */
public class TestSourceDumper {
	
	public static void main(String[] args) {
		System.out.println("TestSourceDumper Started");
		AlarmDumperBase.clientRunner(new String[0], true,30,TimeUnit.SECONDS);
		System.out.println("TestSourceDumper done");
	}
}
