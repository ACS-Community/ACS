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
 *
 *    File TestLocalNC.java
 *
 */
package alma.demo.test.LocalNC;

import alma.acs.nc.*;
import alma.acsnc.EventDescription;
import alma.demo.test.TestNCReceiver.TestReceiver;

public class TestLocalNC {
    private AbstractNotificationChannel nc;

    /**
     * Creates a local notification channel. 
     * For the local notification channel, the ContainerServices object 
     * is not required so it will always be set to null.
     * 
     */
    public TestLocalNC(String channelName) {
        nc = AbstractNotificationChannel.createNotificationChannel(
            AbstractNotificationChannel.LOCAL, channelName, null);
        System.out.println("Local channel "+ channelName +" created");
    }

    public void publishEvent(EventDescription event) {
        System.out.println("Event sent.");
        nc.publish(event);
    }

    public static void main(String[] args) {
        TestLocalNC test = new TestLocalNC("testchannel");
        
        TestReceiver testReceiver = new TestReceiver("LocalReceiver");
        
        Receiver r = LocalNotificationChannel.getLocalReceiver("testchannel");
        
        r.attach("alma.acsnc.EventDescription",testReceiver);
        r.begin();
        for(int i=0; i < 7; i++) {
            try{
                Thread.sleep(2000);
            }catch(Exception e){}
            test.publishEvent(new EventDescription("local event", 32, 64));
       } 
    } 
}

