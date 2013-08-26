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
package alma.acs.eventbrowser.views;

import java.util.logging.Logger;

import junit.framework.TestCase;

import org.omg.CORBA.Any;
import org.omg.CosNotification.StructuredEvent;

import tdem.TDEM_TOPICS.actuatorSpace;
import tdem.TDEM_TOPICS.pttDataEvent;
import alma.acs.eventbrowser.model.EventModel;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.StructuredEventCreator;
import alma.acs.util.StopWatch;
import alma.acs.container.*;

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
		ParsedAnyData[] pResults = parsePttDataEvent();
		for (int i = 0; i < pResults.length; i++) {
			System.out.println(pResults[i].getName() + " "
					+ pResults[i].getType() + " " + pResults[i].getValue());
		}
	}

	public ParsedAnyData[] parsePttDataEvent() {
		pde = new pttDataEvent(new actuatorSpace(new double[2952]),
				new actuatorSpace(new double[2952]), 25, 32L);
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
		ParsedAnyData[] pResults = parser.getParsedResults(null);
		sw.logLapTime("parse this eventAny");
		return pResults;
	}
	
	@Override
	protected void setUp() throws Exception {
		super.setUp();
	}

	@Override
	protected void tearDown() throws Exception {
		super.tearDown();
	}
	


}
