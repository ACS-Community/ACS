/*
 * acssampGUIConsumer.java
 *
 * Created on January 28, 2004, 9:58 AM
 */

/**
 *
 * @author  alma
 */
package alma.acssampGUI;

import org.omg.CosNotification.EventType;
import org.omg.CosNotification.StructuredEvent;

import com.cosylab.gui.components.spikechart.TrendDataModel;

import abeans.models.acs.baci.util.TimeConverter;

import alma.acs.container.ContainerServices;
import alma.acssamp.SampObjPackage.SampDataBlock;
import alma.acssamp.SampObjPackage.SampDataBlockSeqHelper;


public class acssampGUIConsumer extends alma.acs.nc.Consumer {

	TrendDataModel model;

	TimeConverter timeConverter;

	/** Total number of events that have been consumed.
	 */
	public int eventCount = 0;

	/** Creates a new instance of acssampGUIConsumer */
	public acssampGUIConsumer(String ncChannel, TrendDataModel model, ContainerServices cServices)
			throws alma.acs.exceptions.AcsJException 
	{

		super(ncChannel, cServices);
		this.model = model;
		timeConverter = new TimeConverter();

		try {
			//Subscribe to events
			EventType[] added = { new EventType(alma.acscommon.ALMADOMAIN.value, "SampDataBlockSeq") };
			EventType[] removed = {};

			//really subscribe to the events
			m_consumerAdmin.subscription_change(added, removed);
		} catch (org.omg.CosNotifyComm.InvalidEventType e) {
			String msg = "'SampDataBlockSeq' event type is invalid for the '" + m_channelName + "' channel!";
			System.err.println(msg);
		}
	}

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

				long javaTime = TimeConverter.toJava(timeVal);
				//model.addPoint((double)timeVal/1000.0,extVal);
				model.addPoint((double) javaTime / 1000.0, extVal);
			}
		} catch (Exception e) {
			System.err.println(e);
		}

	}

	public void ncDisconnect() {

		System.out.println("Received: " + eventCount);
		disconnect();
	}

}
