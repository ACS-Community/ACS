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

import org.omg.CORBA.portable.IDLEntity;
import org.omg.CosNotification.EventType;
import org.omg.CosNotification.StructuredEvent;

import alma.ACSErrTypeCommon.wrappers.AcsJGenericErrorEx;
import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.Consumer;
import alma.acs.util.UTCUtility;
import alma.acsnc.EventDescription;
import alma.acssamp.SampObjPackage.SampDataBlock;
import alma.acssamp.SampObjPackage.SampDataBlockSeqHelper;

public class acssampConsumer extends Consumer {

	/** 
	 * Total number of events that have been consumed.
	 */
	public int eventCount = 0;

	/** 
	 * Creates a new instance of acssampConsumer 
	 */
	public acssampConsumer(String ncChannel, ContainerServices cServices) throws AcsJException {

		super(ncChannel, cServices);
		
		try {
			// We subscribe to *all* events on this channel by using 'null' instead of the event class.
			// This is necessary because we are dealing with the rare and only partially supported case
			// that the event data is a sequence of IDL structs instead of a single IDL struct.
			// Alledgedly subscribing to "SampDataBlockSeq.class" worked before ACS 6.0, but then broke.			
			// As of ACS 6.0.1 it has become pointless to try to get it running this way, because now
			// the Class parameter must represent a subtype of IDLEntity (which all IDL struct derived classes come from).
			// If it should ever be necessary to subscribe to a particular event that has a sequence of IDL structs as attached data,
			// the same workaround as in acssampGui should be used: instead of calling 'addSubscription', 
			// we call the raw Corba subscription_change method:
			//		EventType[] added = {new EventType(alma.acscommon.ALMADOMAIN.value, "SampDataBlockSeq")};
			//		EventType[] removed = {};
			//		m_consumerAdmin.subscription_change(added, removed);
			addSubscription(null);
		} catch (Exception e) {
			String msg = "Failed to subscribe to archive events: ";
			AcsJGenericErrorEx ex2 = new AcsJGenericErrorEx(e);  
			ex2.setErrorDesc(msg);
			throw ex2;
		}
	}

	
	/**
	 * Should never be called as we don't register a receiver callback object with the NC framework.
	 */
	public void receive(SampDataBlock[] sampData) {
		m_logger.info("Got a call to method receive(SampDataBlock[" + sampData.length + "])");	
	}
	
	/**
	 * Currently never called because we override the default impl of push_structured_event.
	 */
	protected void processEvent(IDLEntity corbaData, EventDescription eventDescrip) {
		super.processEvent(corbaData, eventDescrip);
	}

	
	/** 
	 * We directly overload the push_structured_event to steal the raw events before any processing 
	 * by the NC classes can take place. Usually one would overload one of the processEvent methods.  
	 * @see alma.acs.nc.Consumer#push_structured_event(org.omg.CosNotification.StructuredEvent)
	 */
	public void push_structured_event(StructuredEvent structuredEvent)
			throws org.omg.CosEventComm.Disconnected {

		//Know how many events this instance has received.
		eventCount++;
		
		try {
			System.out.println("The domain is: " + structuredEvent.header.fixed_header.event_type.domain_name);
			System.out.println("The type is: " + structuredEvent.header.fixed_header.event_type.type_name);

			SampDataBlock[] sampledData = SampDataBlockSeqHelper.extract(structuredEvent.filterable_data[0].value);

			for (int i = 0; i < sampledData.length; i++) {
				// extract the time stamp
				long timeVal = sampledData[i].sampTime;
				System.out.println("TIME STAMP: " + timeVal);

				// extract the value
				double extVal = sampledData[i].sampVal.extract_double();
				System.out.println("VALUE: " + extVal);

				long javaTime = UTCUtility.utcOmgToJava(timeVal);
				//model.addPoint((double)timeVal/1000.0,extVal);
				//model.addPoint((double)javaTime/1000.0,extVal);
			}

			// now that we've printed some info, the event should be dispatched to the processEvent or receive methods
			// This currently fails because of the sequence data type. 
//			super.push_structured_event(structuredEvent);

		} 
		catch (Exception e) {
			System.err.println(e);
		}
	}

	public void ncDisconnect() {

		System.out.println("Received: " + eventCount);
		disconnect();
	}

	
	/** 
	 * Here we as a smart consumer would have a chance to subscribe to new types as they are being offered.
	 * Instead we just log the information.
	 * @see alma.acs.nc.Consumer#offer_change(org.omg.CosNotification.EventType[], org.omg.CosNotification.EventType[])
	 */
	public void offer_change(EventType[] eventTypesNew, EventType[] eventTypesOnce) {
		String eventTypesNewString = "";
		for (EventType type : eventTypesNew) {
			eventTypesNewString += (type.type_name + " ");
		}
		String eventTypesOnceString = "";
		for (EventType type : eventTypesOnce) {
			eventTypesOnceString += (type.type_name + " ");
		}
		m_logger.info("Supplier announced to send new event types " + eventTypesNewString + ", but to no longer send " + eventTypesOnceString);
	}
}
