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
	
	private Logger logger;
	
	private pttDataEvent pde;
	private String eventName;
	private StructuredEvent se;
	private Any eventAny;

	public DynAnyParserTest(String name) {
		super(name);
		try {
			em = EventModel.getInstance();
			logger = em.getLogger();
		} catch (Exception e) {
			e.printStackTrace();
			fail("Couldn't create the Event Model.");
		}
		seCreator = new StructuredEventCreator(em.getContainerServices());
	}
	
	/** Contents of ptt data event:
	 *     const long actuatorSpaceLength = 2952;
     *     struct actuatorSpace
     *     {
     *           double ptt[actuatorSpaceLength];
     *     };
     *
	 * 	public tdem.TDEM_TOPICS.actuatorSpace setpoint;
	 *  public tdem.TDEM_TOPICS.actuatorSpace readback;
	 *  public int key;
	 *  public long timestamp;
	 */
	public void testPttDataEventParsing() {
		pde = new pttDataEvent(new actuatorSpace(new double[2952]), new actuatorSpace(new double[2952]), 25, 32L);
		try {
			se = seCreator.createEvent(pde);
		} catch (AcsJException e) {
			e.printStackTrace();
			fail("Couldn't create structured event for pttDataEvent");
		}
		eventName = se.header.fixed_header.event_type.type_name;
		eventAny = se.filterable_data[0].value;
		StopWatch sw = new StopWatch(logger);
		parser = new DynAnyParser(eventAny, eventName);
		ParsedAnyData[] pResults = parser.getParsedResults();
		sw.logLapTime("parse this eventAny");
		for (int i = 0; i<pResults.length; i++) {
			System.out.println(pResults[i].getName()+" "+pResults[i].getType()+" "+pResults[i].getValue());
		}
	}

	protected void setUp() throws Exception {
		super.setUp();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}
	


}
