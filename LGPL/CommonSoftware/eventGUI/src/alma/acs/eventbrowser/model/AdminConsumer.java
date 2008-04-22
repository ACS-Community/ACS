package alma.acs.eventbrowser.model;

import java.util.HashMap;

import org.omg.CosEventComm.Disconnected;
import org.omg.CosNotification.EventType;
import org.omg.CosNotification.StructuredEvent;
import org.omg.CosNotifyComm.InvalidEventType;

import alma.acs.container.ContainerServicesBase;
import alma.acs.eventbrowser.Application;
import alma.acs.eventbrowser.views.EventData;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.Consumer;
import alma.acsnc.EventDescription;
import alma.acsnc.EventDescriptionHelper;

public class AdminConsumer extends Consumer {
	
	private static final EventType et = new EventType("*","*");
	private static final EventType[] eta = {et};
	private static final EventType[] etNone = {};
	private int channelEventCount;
	
	private HashMap<String, Integer> evtCounter;

	public AdminConsumer(String channelName, ContainerServicesBase services)
			throws AcsJException {
		super(channelName, services);
		try {
			evtCounter = new HashMap<String, Integer>();
			m_consumerAdmin.subscription_change(eta, etNone);
		} catch (InvalidEventType e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public AdminConsumer(String arg0, String arg1, ContainerServicesBase arg2)
			throws AcsJException {
		super(arg0, arg1, arg2);
		// TODO Auto-generated constructor stub
	}

	@Override
	public void push_structured_event(StructuredEvent evt) throws Disconnected {
//		super.push_structured_event(evt);
		channelEventCount++;
//		String evtName = evt.header.fixed_header.event_name; // Normally empty, as ACSEventAdmin.py states?
		String evtTypeName = evt.header.fixed_header.event_type.type_name;
		if (evtCounter.containsKey(evtTypeName))
			evtCounter.put(evtTypeName, evtCounter.get(evtTypeName)+1);
		else
			evtCounter.put(evtTypeName, 1);

//		String domainName = evt.header.fixed_header.event_type.domain_name; // Always ALMA?
//		Any data = evt.filterable_data[0].value;
		EventDescription eDescrip = EventDescriptionHelper.extract(evt.remainder_of_body);
//		long timeStamp = eDescrip.timestamp;
//		String component = eDescrip.name;
//		long count = eDescrip.count;
		boolean oresult = Application.equeue.offer(new EventData(eDescrip.timestamp,eDescrip.name,eDescrip.count,evt.header.fixed_header.event_type.type_name,evtCounter.get(evtTypeName)));
		if (!oresult)
			m_logger.severe("Couldn't queue event # "+channelEventCount);
//		m_logger.fine("Time "+eDescrip.timestamp+" "+m_channelName+" "+eDescrip.name+" "+eDescrip.count+" "+channelEventCount+" "
//				+" "+evtTypeName+" "+evtCounter.get(evtTypeName));
		//TODO: Send this info to the viewer
	}

}
