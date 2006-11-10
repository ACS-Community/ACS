
/*
 * acssampConsumer.java
 *
 * Created on January 28, 2004, 9:58 AM
 */

/**
 *
 * @author  alma
 */
////////////////////////////////////////////////////////////////////////////////
package alma.acssamp.jtest;
////////////////////////////////////////////////////////////////////////////////

import org.omg.CosNotification.*;

//import com.cosylab.gui.components.spikechart.TrendDataModel;
/*import alma.acs.nc.*;

import alma.acsnc.*;
import alma.acssamp.*;
import alma.acssamp.SampObjPackage.*;

import abeans.models.acs.baci.util.*;

import alma.acs.container.ContainerServices;*/

import alma.acs.nc.*;
import alma.acssamp.*;
import alma.acssamp.SampObjPackage.*;

import abeans.models.acs.baci.util.*;

import alma.acs.container.ContainerServices;

public class acssampConsumerNew {
   
    private Consumer m_consumer = null;

    public void receive(alma.acssamp.SampObjPackage.SampDataBlockSeqHelper evt)
	{
	    System.out.println("EEEEEEEEEEEEEEEEEEEEE");
	}

    public void disconnect()
	{
	    m_consumer.disconnect();
	}

    public void ready()
	{
	    try
		{
		m_consumer.consumerReady();
		}
	    catch(Exception e)
		{
		String msg = "consumerReady() Error!";
		System.err.println(msg);
		}
	}

    /** Total number of events that have been consumed.
     */    
    public int eventCount = 0;

   
    /** Creates a new instance of acssampConsumer */
    public acssampConsumerNew(String ncChannel, ContainerServices cServices) 
	throws alma.acs.exceptions.AcsJException
	{
 	    m_consumer = new Consumer(ncChannel, cServices);
	    m_consumer.addSubscription(alma.acssamp.SampObjPackage.SampDataBlockSeqHelper.class,this);

	}
    

}
