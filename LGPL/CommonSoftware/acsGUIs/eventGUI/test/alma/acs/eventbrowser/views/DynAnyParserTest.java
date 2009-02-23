package alma.acs.eventbrowser.views;

import java.util.logging.Logger;

import org.omg.CORBA.Any;
import org.omg.CORBA.portable.IDLEntity;
import org.omg.CosNotification.EventHeader;
import org.omg.CosNotification.EventType;
import org.omg.CosNotification.FixedEventHeader;
import org.omg.CosNotification.Property;
import org.omg.CosNotification.StructuredEvent;

import alma.acs.eventbrowser.model.EventModel;
import alma.acs.eventbrowser.views.DynAnyParser;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.StructuredEventCreator;
import alma.acs.util.StopWatch;
import tdem.TDEM_TOPICS.actuatorSpace;
import tdem.TDEM_TOPICS.pttDataEvent;
import junit.framework.TestCase;

public class DynAnyParserTest extends TestCase {
	private StructuredEventCreator seCreator;
	private EventModel em;
	private DynAnyParser parser;
	
	private pttDataEvent pde;
	private StructuredEvent se;
	private Any eventAny;

	public DynAnyParserTest(String name) {
		super(name);
		try {
			em = EventModel.getInstance();
			em.getLogger();
		} catch (Exception e) {
			e.printStackTrace();
			fail("Couldn't create the Event Model.");
		}
		pde = new pttDataEvent(new actuatorSpace(new double[2952]), new actuatorSpace(new double[2952]), 25, 32L);
		try {
			seCreator = new StructuredEventCreator(em.getContainerServices());
			se = seCreator.createEvent(pde);
		} catch (AcsJException e) {
			e.printStackTrace();
			fail("Couldn't create Structured Event from pttDataEvent structure");
		}
		eventAny = se.filterable_data[0].value;
	}
	
	public void testBasicParsing() {
		parser = new DynAnyParser(eventAny);
		parser.getParsedResults();

	}

	protected void setUp() throws Exception {
		super.setUp();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}
	


}
