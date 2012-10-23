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

import java.util.Date;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;

import org.apache.commons.math.stat.descriptive.SummaryStatistics;
import org.omg.CORBA.portable.IDLEntity;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNotification.StructuredEvent;

import alma.ACSErrTypeCommon.wrappers.AcsJGenericErrorEx;
import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.refactored.NCSubscriber;
import alma.acs.util.IsoDateFormat;
import alma.acs.util.UTCUtility;
import alma.acsnc.EventDescription;
import alma.acssamp.SampObjPackage.SampDataBlock;
import alma.acssamp.SampObjPackage.SampDataBlockSeqHelper;

/**
 * @TODO: Merge with {@link acssampSupplier} into one class,
 * validate frequency of sampling data calls received etc.
 */
public class acssampConsumer extends NCSubscriber<IDLEntity>
{
	/**
	 * Differences of adjacent sample data timestamps in ms.
	 */
	public final SummaryStatistics samplingIntervalStats = new SummaryStatistics();

	/**
	 * Differences of callbacks to {@link #push_structured_event_called(StructuredEvent)} in ms.
	 */
	public final SummaryStatistics callbackIntervalStats = new SummaryStatistics();
	
	/**
	 * Lengths of sequences given to {@link #push_structured_event_called(StructuredEvent)}.
	 */
	public final SummaryStatistics sequenceLengthStats = new SummaryStatistics();
	
	private long sampDateJavaOld = -1;
	private long receivedDateJavaOld = -1;
	

	acssampConsumer(String channelName, ContainerServicesBase services, NamingContext namingService)
			throws AcsJException {
		super(channelName, null, services, namingService, acssampConsumer.class.getSimpleName(), IDLEntity.class);

		try {
			// We subscribe to *all* events on this channel by using a generic subscription,
			// to make sure that server-side filtering does not remove any events.
			// Due to the non-standard processing in MyNCSubscriber, the generic callback should never be invoked.
			addGenericSubscription(new GenericCallback() {
				@Override
				public void receiveGeneric(Object event, EventDescription eventDescrip) {
					logger.warning("Unexpected event delivery to 'receiveGeneric' method.");
				}
			});
		} catch (Exception e) {
			String msg = "Failed to subscribe to sampling events: ";
			AcsJGenericErrorEx ex2 = new AcsJGenericErrorEx(e);
			ex2.setErrorDesc(msg);
			throw ex2;
		}
	}

	/**
	 * We directly overload the push_structured_event to steal the raw events before any processing by the NC classes
	 * can take place. Usually one would overload one of the processEvent methods.
	 * 
	 * @see alma.acs.nc.Consumer#push_structured_event(org.omg.CosNotification.StructuredEvent)
	 */
	/* (non-Javadoc)
	 * @see alma.acs.nc.refactored.NCSubscriber#push_structured_event_called(org.omg.CosNotification.StructuredEvent)
	 */
	@Override
	protected boolean push_structured_event_called(StructuredEvent structuredEvent) {
		
		long receivedDateJava = System.currentTimeMillis();
		if (receivedDateJavaOld > 0) {
			callbackIntervalStats.addValue(receivedDateJava - receivedDateJavaOld);
		}
		receivedDateJavaOld = receivedDateJava;
		
		try {
			SampDataBlock[] sampledData = SampDataBlockSeqHelper.extract(structuredEvent.filterable_data[0].value);
			
			String msg = "Sample event: domain = " + structuredEvent.header.fixed_header.event_type.domain_name +
						"; type = " + structuredEvent.header.fixed_header.event_type.type_name +
						"; seqLength=" + sampledData.length;
			logger.info(msg);

			sequenceLengthStats.addValue(sampledData.length);
			
			for (int i = 0; i < sampledData.length; i++) {
				// extract the time stamp
				long dateOmg = sampledData[i].sampTime;
				long dateJava = UTCUtility.utcOmgToJava(dateOmg);
				String dateIso = IsoDateFormat.formatDate(new Date(dateJava));
				System.out.println("TIME STAMP: " + dateOmg + " = " + dateIso);

				// extract the value
				double extVal = sampledData[i].sampVal.extract_double();
				System.out.println("VALUE: " + extVal);

				// jitter statistics
				if (sampDateJavaOld > 0) {
					samplingIntervalStats.addValue(dateJava - sampDateJavaOld);
				}
				sampDateJavaOld = dateJava;
				
//				long javaTime = UTCUtility.utcOmgToJava(timeVal);
				// model.addPoint((double)timeVal/1000.0,extVal);
				// model.addPoint((double)javaTime/1000.0,extVal);
			}
		} catch (Exception ex) {
			this.logger.log(Level.SEVERE, "", ex);
		}
		return false;
	}

}
