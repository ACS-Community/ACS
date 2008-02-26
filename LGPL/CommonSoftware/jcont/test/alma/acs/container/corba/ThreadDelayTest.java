/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) Universidad Tecnica Federico Santa Maria, 2008
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
package alma.acs.container.corba;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import alma.acs.component.client.ComponentClientTestCase;
import alma.jconttest.DataReceiver;
import alma.jconttest.DataReceiverHelper;

/** 
 * @author rtobar
 * Created on Feb 25, 2008, 11:21:30 PM
 */
public class ThreadDelayTest extends ComponentClientTestCase {

	private Thread[] myThreads;
	private DataReceiver dataReceiver;
	private final String DUMMY_COMPTYPE = "IDL:alma/jconttest/DataReceiver:1.0";
	private final int MAX_THREADS = 100;
	private final int MAX_LOOPS = 100;
	private FileWriter fileWriter;
	
	public ThreadDelayTest(String name) throws Exception {
		super(name);
	}

	protected void setUp() throws Exception {
		super.setUp();
		
		//	Getting the DataReceiver component...
		org.omg.CORBA.Object obj = getContainerServices().getDefaultComponent(DUMMY_COMPTYPE);
		assertNotNull(obj);
		dataReceiver = DataReceiverHelper.narrow(obj);
		assertNotNull(dataReceiver);
		
		//	Creating the threads
		myThreads = new Thread[MAX_THREADS];
		for (int i = 0; i < myThreads.length; i++) {
			myThreads[i] = new Thread(new ClientThread(5,1000),"Thread " + (i+1));
		}
		
		// Let's find a non-existing file for output
		int i = 0;
		File outputFile = null;
		while(true) {
			outputFile = new File("thread-delays-output" + (i > 0 ? i : "") + ".dat");
			
			if(!outputFile.exists()) {
				outputFile.createNewFile();
				fileWriter = new FileWriter(outputFile);
				fileWriter.write("# File created automatically by the ThreadDelayTest\n");
				fileWriter.write("# junit test class (alma.acs.container.corba package)\n\n");
				break;
			}
			i++;
		}
		
	}

	public void testThreadDelays() throws Exception {
		
		// Starting the threads
		for (int i = 0; i < myThreads.length; i++) {
			myThreads[i].start();
		}
		
		// And joining them
		for(int i=0; i != myThreads.length; i++) {
			myThreads[i].join();
		}
		
		// Let's sleep a little so the threads can join OK
		Thread.sleep(100);
		fileWriter.write("Calls/sec: " + dataReceiver.stop());
		fileWriter.close();
	}
	
	protected void tearDown() throws Exception {
		dataReceiver._release();
		super.tearDown();
	}

	private class ClientThread implements Runnable {
		
		private int timeBetweenCalls = 0;
		private String message = null;
		
		public ClientThread(int timeBetweenCalls, int messageSize) {
			this.timeBetweenCalls = timeBetweenCalls;
			StringBuffer buffer = new StringBuffer();
			for(int i=0; i!=messageSize; i++)
				buffer.append(' ');
			
			message = buffer.toString();
		}
		
		public void run() {
			
			int i = 0;
			long initTime;
			double timeInMilli;
			
			while( i!= MAX_LOOPS ) {
				
				initTime = System.nanoTime();
				dataReceiver.storeThis(message);
				timeInMilli = (System.nanoTime() - initTime)/1000000;
				
				try {
					fileWriter.write(String.valueOf(timeInMilli) + "\n");
				} catch (IOException e1) {
					// Should not happen
				}
				
				try {
					Thread.sleep(timeBetweenCalls);
				} catch (InterruptedException e) {
					System.err.println("Couldn't sleep :(");
				}
				i++;
			}
			
		}
	}
}
