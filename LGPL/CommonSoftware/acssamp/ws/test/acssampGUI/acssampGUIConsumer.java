/*
 * acssampGUIConsumer.java
 *
 * Created on January 28, 2004, 9:58 AM
 */

/**
 *
 * @author  alma
 */

import org.omg.CosNotification.*;
import alma.acsnc.*;
import alma.ACSSamp.*;
import alma.ACSSamp.SampObjPackage.*;

public class acssampGUIConsumer extends alma.acs.nc.Consumer {

    /** Total number of events that have been consumed.
     */    
    public int eventCount = 0;
    
    /** Creates a new instance of acssampGUIConsumer */
    public acssampGUIConsumer(String ncChannel) {
        
         super(ncChannel);
    }
    
    public void push_structured_event(StructuredEvent structuredEvent) throws org.omg.CosEventComm.Disconnected {
        
        //Know how many events this instance has received.
        
            eventCount++;
     try
	    {
		System.out.println("The domain is: " +
				   structuredEvent.header.fixed_header.event_type.domain_name);
		System.out.println("The type is: " +
				   structuredEvent.header.fixed_header.event_type.type_name);
		
		sampDataBlock[] sampledData = SampDataBlockSeqHelper.extract(structuredEvent.filterable_data[0].value);
		
		for (int i =0; i< sampledData.length; i++)
		    {
			// extract the time stamp
			long timeVal= sampledData[i].sampTime;
			System.out.println("TIME STAMP: " + timeVal);
   			
			// extract the value
			double extVal=sampledData[i].sampVal.extract_double();
			System.out.println("VALUE: " + extVal);
		    }
		

		
	    }
        catch(Exception e)
	    {
		System.err.println(e);
	    }
        
    }

    
    public void ncDisconnect() {
       
        System.out.println("Received: " + eventCount);
        disconnect();
    }
    
}
