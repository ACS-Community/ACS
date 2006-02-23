/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) Associated Universities Inc., 2002 
 *    (c) European Southern Observatory, 2002
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
 *
 *    File TestReceiver.java
 *
 */
package alma.demo.test.TestNCReceiver;

import alma.acsnc.EventDescription;

public class TestReceiver {
    private String name;

    public TestReceiver(String receiverName) {
        this.name = receiverName;
    }

    /** 
     * The one function that your receiver's object class must have!
     * When you create a receiver class, the object class which the
     * receiver will give the event to must have a function called
     * receive and its argument must be the same type as the data
     * being received from the event. If the types do not match an
     * error will be thrown when you try to attach to the channel.
     */
    public void receive(EventDescription data) {
        System.out.println("Receiver "+ name +" got an event.");
        System.out.println(data.name);
    }
}

