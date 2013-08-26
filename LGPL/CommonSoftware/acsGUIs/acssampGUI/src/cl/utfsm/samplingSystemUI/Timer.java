/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package cl.utfsm.samplingSystemUI;
import javax.swing.JOptionPane;

public class Timer extends Thread {
	 protected int rate=50;

	    private   int elapsed;
	    private   int time_ms;
	    private   boolean run = true;

	    public Timer(int time_ms) {
	        this.time_ms = time_ms;
	        elapsed      = 0;
	    }
	   /** starts the timer, if timeout then calls the timeout method*/  
	    public void run() {
	        while(run) {
	            try {
	                Thread.sleep(rate);
	            } catch(InterruptedException e) {
	                continue;
	            }
	            synchronized(this) {
	                elapsed = elapsed + rate;
	                if(elapsed > time_ms) {
	                     timeout();
	                     break;
	                }    

	            }        
	        }

	    }
	    /** this method is called when the time is out!*/
	    public void timeout() {
	        JOptionPane.showMessageDialog(null, "There's no response from the container, maybe the container is down", "Time out!", JOptionPane.ERROR_MESSAGE);
	    }
	    /** reset the timer*/
	    public void reset() {
		elapsed = 0;
	        run = false;
	    }

}
