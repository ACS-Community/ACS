package alma.nctest.clients;

import java.util.logging.Level;
import java.util.logging.Logger;

import alma.ACSErr.ErrorTrace;
import alma.ACSErr.Severity;
import alma.ACSErr.NameValue;
import alma.ACSErr.Completion;

import alma.acs.component.ComponentLifecycleException;
import alma.acs.component.client.ComponentClient;
import alma.acs.logging.AcsLogLevel;
import alma.acs.nc.AcsEventPublisher;

/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2016 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
/** 
 * A {@link ComponentClient} that publishes an event containing a Completion
 * (@see ICT-3870).
 * 
 * The test instantiates a java client that builds and poublishes a Completion event.
 * 
 * @author acaproni
 * @since ACS-OCT-2016    
 */
public class CompletionEventPublisherClient extends ComponentClient {

	private final AcsEventPublisher<Completion> m_supplier;

	/**
	 * @param logger
	 * @param managerLoc
	 * @param clientName
	 * @throws Exception
	 */
	public CompletionEventPublisherClient(Logger logger, String managerLoc, String clientName) throws Exception {
		super(logger, managerLoc, clientName);

		m_logger.info("Initializing the supplier");

		// Sets up the SimpleSupplier.
		try {
			// Instantiate our supplier
			m_supplier = getContainerServices().createNotificationChannelPublisher("ICT3870NC", Completion.class);
		} catch (Exception e) {
			e.printStackTrace(System.err);
			throw new ComponentLifecycleException(e);
		}
		publishEvent();
		m_supplier.disconnect();
	}

	/**
	 * Publish a {@link StructWithACompletion}.
	 * 
	 * At the present this tests shall pass because of the workaround described
	 * in the ticket (i.e. added another typedef for for the sequence in the
	 * {@link Completion}).
	 */
	private void publishEvent() {
		m_logger.info("Going to publish an event");

		// Create and initialize an event with a Completion
		Completion c = new Completion();
		c.code = 100;
		c.timeStamp = System.currentTimeMillis();
		c.type = 101;

		// ICT-3870 error happens also without any ErorTrace in the previuError property 
		// of the COmpletion but this way it is more compete
		ErrorTrace event = new ErrorTrace();
		event.previousError = new ErrorTrace[0];
		event.errorCode = 1;
		event.errorType = 2;
		event.file = "filename";
		event.host = "hostname";
		event.lineNum = 3;
		event.process = "processname";
		event.routine = "reoutinename";
		event.severity = Severity.Alert;
		NameValue nv = new NameValue();
		nv.name = "NameValue name";
		nv.value = "NameValue value";
		event.data = new NameValue[0];
		event.shortDescription = "short desc";
		event.sourceObject = "src obj";
		event.thread = "thread name";
		event.timeStamp = System.currentTimeMillis();

		c.previousError = new ErrorTrace[1];
		
		 c.previousError[0] = event;

		try {
			m_supplier.publishEvent(c);

			m_logger.info("Event successfull published.");
		} catch (Throwable t) {
			m_logger.log(AcsLogLevel.ERROR, "Error publishing the event with a Completion", t);
		}

	}

	/**
         * Main entry pont
         */
        public static void main(String[] args) {
                String managerLoc = System.getProperty("ACS.manager");
                if (managerLoc == null) {
                        System.out
                                        .println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
                        System.exit(-1);
                }
                String clientName = "CompletionEventPublisherClient";
                CompletionEventPublisherClient completionClient = null;
                try {
                        completionClient = new CompletionEventPublisherClient(null, managerLoc, clientName);
                }
                catch (Exception e) {
            try {
                Logger logger = completionClient.getContainerServices().getLogger();
                logger.log(Level.SEVERE, "Client application failure", e);
            } catch (Exception e2) {
                e.printStackTrace(System.err);
            }
                }
                finally {
                        if (completionClient != null) {
                                try {
                                        completionClient.tearDown();
                                }
                                catch (Throwable t) {
                                        t.printStackTrace();
                                }
                        }
                }
        }
}
