package alma.acs.eventbrowser.views;

import java.util.ArrayList;
import java.util.logging.Logger;

import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.Display;

import alma.acs.eventbrowser.Application;
import alma.acs.eventbrowser.model.AdminConsumer;
import alma.acs.eventbrowser.model.EventModel;
import alma.acs.exceptions.AcsJException;
import alma.acs.util.StopWatch;

public class PopulateEventList {

	private static final Runtime runtime = Runtime.getRuntime();
	private static final long MEMORY_MARGIN_IN_BYTES = runtime.maxMemory()/5; // Remove first rows when free memory < this (here 20% of max)
	private static final int CHECK_MEMORY_FREQUENCY = 10;	// Run the memory checker this often
	private static final int QUEUE_DRAIN_LIMIT = 1000;
	private static final int PERCENTAGE_TO_DELETE = 20;	// When memory is low, delete this many rows from start of table

	private Logger logger;
	private TableViewer viewer;
	private ArrayList<AdminConsumer> consumers;

	public PopulateEventList(Logger logger, TableViewer viewer) {
		super();
		this.logger = logger;
		this.viewer = viewer;
	}

	private long cycles = 0;

	private final long max_memory = runtime.maxMemory();

	Thread getThreadForEventList(EventModel em) {
		Runnable t = new Runnable() {
			public Runnable r = new Runnable() {

				private long totalNumberDrained;

				public void run() {
					final Display display = viewer.getControl().getDisplay();
					if (!display.isDisposed()) {
						ArrayList<EventData> c = new ArrayList<EventData>(QUEUE_DRAIN_LIMIT);
						int numberDrained = Application.equeue.drainTo(c, QUEUE_DRAIN_LIMIT);
						if (numberDrained == 0)
							return;
						totalNumberDrained += numberDrained;
						viewer.add(c.toArray()); 
						//					logger.fine("Table item count: "
						//							+ viewer.getTable().getItemCount()
						//							+ "; number of events drained from queue: "+numberDrained);

						if (cycles++%CHECK_MEMORY_FREQUENCY == 0) {
							StopWatch sw = new StopWatch(logger);
							freeMemoryIfNecessary();
							sw.logLapTime("Check free memory");
							logger.fine("Total rows processed so far: "+totalNumberDrained);
						}
					}
				}
			};

			public void run() {
				final Display display = viewer.getControl().getDisplay();

				while (true) {
					if (display.isDisposed())
						return;
					display.syncExec(r);
					//display.asyncExec(r);
					try {
						Thread.sleep(1000);
						//					System.out.println("Event detail iteration " + ++i);
						//					System.out.println("Queue has "
						//							+ Application.equeue.remainingCapacity()
						//							+ " slots left.");
					} catch (InterruptedException e) {
						System.out.println("Monitoring was interrupted!");
						break;
						// Application.setMonitoring(false);
						// startMonitoringAction.setEnabled(true);
					}
				}
				// Application.setMonitoring(false);
				// startMonitoringAction.setEnabled(true);
			}
		};
		final Thread th = new Thread(t, "Event monitoring");
		//	th.start();
		consumers = em.getAllConsumers();

		if (consumers != null) {
			for (AdminConsumer consumer : consumers) {
				try {
					consumer.consumerReady();
				} catch (AcsJException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		return th;
	}

	private void freeMemoryIfNecessary() {
		long ultimateFreeMemory = max_memory - (runtime.totalMemory()-runtime.freeMemory());
		if (ultimateFreeMemory < MEMORY_MARGIN_IN_BYTES) {
			int itemCount = viewer.getTable().getItemCount();
			int number_to_delete = (itemCount*PERCENTAGE_TO_DELETE)/100;
			logger.fine("Now have "+itemCount+" rows in event table.");
			logger.fine("Remaining allocatable memory is: "+ultimateFreeMemory+" bytes. Margin required is "+MEMORY_MARGIN_IN_BYTES);
			logger.fine("Will attempt to delete the first "+number_to_delete+" rows.");
			Object[] els = new Object[number_to_delete];

			for( int i = 0; i < number_to_delete; i++ ) {
				els[i] = viewer.getElementAt(i);
			}

			logger.fine("...removing "+number_to_delete+" of the oldest rows to avoid running out of memory, thread=" + Thread.currentThread().getName());
			viewer.remove(els);
			logger.fine("Remove done!");
			logger.fine("...item count reduced by: "+(itemCount-viewer.getTable().getItemCount()));
			System.gc(); // Force GC so that we'll know how much memory is left the next time around
		}
	}
}
