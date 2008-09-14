package alma.acs.eventbrowser.views;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.logging.Logger;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import alma.acs.eventbrowser.Application;
import alma.acs.eventbrowser.model.AdminConsumer;
import alma.acs.eventbrowser.model.EventModel;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.Consumer;
import alma.acs.util.StopWatch;

public class EventDetailView extends ViewPart {
	private static final long MEMORY_MARGIN_IN_BYTES = 850000000; // Remove first rows when free memory < this
	private static final int CHECK_MEMORY_FREQUENCY = 10;	// Run the memory checker this often
	private static final int QUEUE_DRAIN_LIMIT = 1000;
	private static final int NUMBER_TO_DELETE = 1000;	// When memory is low, delete this many rows from start of table
	
	private TableViewer viewer;
	private ArrayList<AdminConsumer> consumers;

	private EventModel em;
	private Action clearEvents;
	private Action printEventDetails;
	
	private long cycles = 0;
	private Runtime runtime;
	private long max_memory;
	private Logger logger;

	public static final String ID = "alma.acs.eventbrowser.views.eventdetail";


	public EventDetailView() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public void createPartControl(Composite parent) {
		final int TABLE_CAPACITY = 10000;
		try {
			em = EventModel.getInstance();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		runtime = Runtime.getRuntime();
		max_memory = runtime.maxMemory();
		logger = em.getLogger();
		viewer = new TableViewer(parent, SWT.MULTI | SWT.H_SCROLL
				| SWT.V_SCROLL | SWT.VIRTUAL);

		Table table = viewer.getTable();
		table.setHeaderVisible(true);
		table.setLinesVisible(true);
//		table.setItemCount(TABLE_CAPACITY);
		/*
		 * "Time "+timeStamp+" "+m_channelName+" "+component+" "+count+"
		 * "+channelEventCount+" " +" "+evtTypeName+"
		 * "+evtCounter.get(evtTypeName)
		 */

		TableViewerColumn tvcol = new TableViewerColumn(viewer, SWT.NONE, 0);
		tvcol.setLabelProvider(new TimeStampLabelProvider());
		TableColumn col = tvcol.getColumn();
		col.setText("Timestamp");
		col.setWidth(180);
		col.setAlignment(SWT.LEFT);

		tvcol = new TableViewerColumn(viewer, SWT.NONE, 1);
		tvcol.setLabelProvider(new EventSourceLabelProvider());
		col = tvcol.getColumn();
		col.setText("Event source");
		col.setWidth(100);
		col.setAlignment(SWT.LEFT);

		tvcol = new TableViewerColumn(viewer, SWT.NONE, 2);
		tvcol.setLabelProvider(new CountLabelProvider());
		col = tvcol.getColumn();
		col.setText("# Events in channel");
		col.setWidth(50);
		col.setAlignment(SWT.LEFT);

		tvcol = new TableViewerColumn(viewer, SWT.NONE, 3);
		tvcol.setLabelProvider(new EventTypeLabelProvider());
		col = tvcol.getColumn();
		col.setText("Event type");
		col.setWidth(50);
		col.setAlignment(SWT.LEFT);

		tvcol = new TableViewerColumn(viewer, SWT.NONE, 4);
		tvcol.setLabelProvider(new EventTypeCountLabelProvider());
		col = tvcol.getColumn();
		col.setText("# Events this type");
		col.setWidth(50);
		col.setAlignment(SWT.LEFT);

		viewer.setContentProvider(new EventDetailViewContentProvider());
		// viewer.setComparator(new ServiceViewerComparator());
		getSite().setSelectionProvider(viewer); // In order to be able to display event detail
		viewer.setInput(getViewSite());

		makeActions();
		hookContextMenu();

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
//						logger.fine("Table item count: "
//								+ viewer.getTable().getItemCount()
//								+ "; number of events drained from queue: "+numberDrained);
						if (cycles++%CHECK_MEMORY_FREQUENCY == 0) {
							StopWatch sw = new StopWatch(logger);
							checkFreeMemory();
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
//						System.out.println("Event detail iteration " + ++i);
//						System.out.println("Queue has "
//								+ Application.equeue.remainingCapacity()
//								+ " slots left.");
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
//		th.start();
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
		th.start();
	}

	private void hookContextMenu() {
		MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				fillContextMenu(manager);
			}
		});
		Menu menu = menuMgr.createContextMenu(viewer.getControl());
		viewer.getControl().setMenu(menu);
		getSite().registerContextMenu(menuMgr, viewer);
	}

	private void fillContextMenu(IMenuManager manager) {
		manager.add(clearEvents);
		manager.add(printEventDetails);
		// Other plug-ins can contribute there actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}

	private void makeActions() {
		clearEvents = new Action() {
			public void run() {
				final Display display = viewer.getControl().getDisplay();
				if (!display.isDisposed())
					display.asyncExec(new Runnable() {
						public void run() {
							viewer.getTable().removeAll();
							viewer.refresh();
						};
					});
			}
		};
		clearEvents.setText("Clear events");
		clearEvents.setToolTipText("Clear all events from the table");
		clearEvents.setImageDescriptor(PlatformUI.getWorkbench()
				.getSharedImages().getImageDescriptor(
						ISharedImages.IMG_TOOL_DELETE));
		printEventDetails = new Action() {
			public void run() {
				AdminConsumer.setPrintDetails(!AdminConsumer.getPrintDetails());
			};
		};
		printEventDetails.setText("Toggle event details");
		printEventDetails.setToolTipText("Toggle display of event details to STDOUT");

	}

	@Override
	public void setFocus() {
		viewer.getControl().setFocus();

	}

	@Override
	public void dispose() {
		super.dispose();
		for (AdminConsumer consumer : consumers) {
			consumer.disconnect();
		}
	}

	private void checkFreeMemory() {
		long ultimateFreeMemory = max_memory - (runtime.totalMemory()-runtime.freeMemory());
		if (ultimateFreeMemory < MEMORY_MARGIN_IN_BYTES) {
			int itemCount = viewer.getTable().getItemCount();
			if (itemCount < NUMBER_TO_DELETE) {
				logger.fine("We're almost out of memory, but there are only: "+itemCount+" rows left!");
				return; // It's hopeless!
			}
			logger.fine("Now have "+itemCount+" rows in event table.");
			Object[] els = new Object[NUMBER_TO_DELETE];

			boolean hasNull = false;
			for( int i = 0; i < NUMBER_TO_DELETE; i++ ) {
			   els[i] = viewer.getElementAt(i);
			   if (els[i] == null && hasNull == false) {
				   logger.fine("Element # "+i+" is null."); // Log first null element found
				   hasNull = true;
			   }
			}
			
			if (hasNull) {
				return; // bailing out...
			}

			logger.fine("...removing "+NUMBER_TO_DELETE+" rows to avoid running out of memory, thread=" + Thread.currentThread().getName());
			viewer.remove(els);
			logger.fine("Remove done!");
			logger.fine("...item count reduced by: "+(itemCount-viewer.getTable().getItemCount()));
		}
	}

}
