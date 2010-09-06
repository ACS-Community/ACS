package alma.acs.eventbrowser.views;

import java.util.ArrayList;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.logging.Logger;

import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.widgets.Display;

import alma.acs.eventbrowser.model.EventModel;
import alma.acs.util.StopWatch;

public class PopulateEventList {

	private static final Runtime runtime = Runtime.getRuntime();
	private static final long MEMORY_MARGIN_IN_BYTES = runtime.maxMemory()/5; // Remove first rows when free memory < this (here 20% of max)
	private static final int CHECK_MEMORY_FREQUENCY = 10;	// Run the memory checker this often
	private static final int QUEUE_DRAIN_LIMIT = 1000;
	private static final int PERCENTAGE_TO_DELETE = 20;	// When memory is low, delete this many rows from start of table

	private Logger logger;
	private TableViewer viewer;
	private final Display display;
	private EventModel em;
	private ArrayBlockingQueue<?> queue;
	private final String threadName;

	public PopulateEventList(Logger logger, TableViewer viewer, EventModel em, ArrayBlockingQueue<?> queue, String threadName) {
		super();
		this.logger = logger;
		this.viewer = viewer;
		this.em = em;
		this.queue = queue;
		display = viewer.getControl().getDisplay();
		this.threadName = threadName;
	}

	private long cycles = 0;

	private final long max_memory = runtime.maxMemory();

	Thread getThreadForEventList() {
		Runnable t = new Runnable() {
			public Runnable r = new Runnable() {

				private long totalNumberDrained;

				public void run() {
					//final Display display = viewer.getControl().getDisplay();
					if (!display.isDisposed()) {
						ArrayList c = new ArrayList(QUEUE_DRAIN_LIMIT);
						int numberDrained = queue.drainTo(c, QUEUE_DRAIN_LIMIT);
						if (numberDrained == 0)
							return;
						totalNumberDrained += numberDrained;
						synchronized (this) { // TODO -- figure out why this doesn't work; need same lock on display everywhere?
							if (!display.isDisposed())
								viewer.add(c.toArray());
						}
						if (cycles++%CHECK_MEMORY_FREQUENCY == 0) {
							StopWatch sw = new StopWatch(logger);
							freeMemoryIfNecessary();
							sw.logLapTime("Check free memory");
							logger.fine("Total rows processed so far: "+totalNumberDrained);
							if (threadName.equals("NC Events"))
								logger.info("Average event rate: "+EventData.getAverageRate()+" events/s");
							else
								logger.info("Average archiving rate: "+ArchiveEventData.getAverageRate()+" monitor points/s");
						}
					}
				}
			};

			public void run() {
				//final Display display = viewer.getControl().getDisplay();

				while (!Thread.currentThread().isInterrupted()) {
					if (display.isDisposed())
						return;
					try {
						display.syncExec(r);
					//display.asyncExec(r);

						Thread.sleep(1000);
						//					System.out.println("Event list iteration " + ++i);
						//					System.out.println("Queue has "
						//							+ Application.equeue.remainingCapacity()
						//							+ " slots left.");
					} catch (InterruptedException e) {
						System.out.println("Event monitoring was interrupted!");
						break;
						// Application.setMonitoring(false);
						// startMonitoringAction.setEnabled(true);
					} catch (SWTException e) {
						// eat it
						break;
					}
				}
				// Application.setMonitoring(false);
				// startMonitoringAction.setEnabled(true);
			}
		};
		
		final Thread th = new Thread(t, threadName);

		return th;
	}

	public Thread getChannelRefreshThread() {
		final Thread subscrTh = new Thread(new Runnable() {
			@Override
			public void run() {
				while (!Thread.currentThread().isInterrupted()) {
					logger.info("Refreshing channel subscriptions");
					em.refreshChannelSubscriptions();
					try {
						Thread.sleep(60000); // Check for new channels every minute
					} catch (InterruptedException e) {
						logger.fine("Subscription thread interrupted.");
						break;
					}
				}
			}
		}, "Channel Refresh");
		return subscrTh;
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
