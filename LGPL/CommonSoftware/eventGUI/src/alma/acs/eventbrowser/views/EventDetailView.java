package alma.acs.eventbrowser.views;

import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.part.ViewPart;

import alma.acs.eventbrowser.Application;
import alma.acs.eventbrowser.model.AdminConsumer;
import alma.acs.eventbrowser.model.EventModel;
import alma.acs.exceptions.AcsJException;

public class EventDetailView extends ViewPart {
	private TableViewer viewer;
	private AdminConsumer consumer;
	
	private EventModel em;
	
	public static final String ID = "alma.acs.eventbrowser.views.eventdetail";

	
	public EventDetailView() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public void createPartControl(Composite parent) {
		try {
			em = EventModel.getInstance();
			consumer = em.getAdminConsumer();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		viewer = new TableViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		
		Table table = viewer.getTable();
		table.setHeaderVisible(true);
		table.setLinesVisible(true);
		/*
		 * "Time "+timeStamp+" "+m_channelName+" "+component+" "+count+" "+channelEventCount+" "
				+" "+evtTypeName+" "+evtCounter.get(evtTypeName)
		 */
		
		TableViewerColumn tvcol = new TableViewerColumn(viewer,SWT.NONE, 0);
		tvcol.setLabelProvider(new TimeStampLabelProvider());
		TableColumn col = tvcol.getColumn();
		col.setText("Timestamp");
		col.setWidth(110);
		col.setAlignment(SWT.LEFT);

		tvcol = new TableViewerColumn(viewer,SWT.NONE, 1);
		tvcol.setLabelProvider(new ChannelNameLabelProvider());
		col = tvcol.getColumn();
		col.setText("Channel name");
		col.setWidth(50);
		col.setAlignment(SWT.LEFT);
		
		tvcol = new TableViewerColumn(viewer,SWT.NONE, 2);
		tvcol.setLabelProvider(new CountLabelProvider());
		col = tvcol.getColumn();
		col.setText("Count");
		col.setWidth(50);
		col.setAlignment(SWT.LEFT);
		
		viewer.setContentProvider(new EventDetailViewContentProvider());
//		viewer.setComparator(new ServiceViewerComparator());
		viewer.setInput(getViewSite());

		tvcol = new TableViewerColumn(viewer,SWT.NONE, 3);
		tvcol.setLabelProvider(new EventTypeLabelProvider());
		col = tvcol.getColumn();
		col.setText("Event type");
		col.setWidth(50);
		col.setAlignment(SWT.LEFT);
		
		
		Runnable t = new Runnable()  {
			int i = 0;
			public Runnable r = new Runnable() {
				public void run() {
					final Display display = viewer.getControl().getDisplay();
					if (!display.isDisposed()) {
						viewer.add(Application.equeue.toArray());
						Application.equeue.clear();
					}
				}
			};


			
			public void run() {
				final Display display = viewer.getControl().getDisplay();

				while (true) {
					if (!display.isDisposed())
						display.asyncExec(r);
					try {
						Thread.sleep(5000);
						System.out.println("Event detail iteration "+ ++i);
						System.out.println("Queue has "+Application.equeue.remainingCapacity()+" slots left.");
					} catch (InterruptedException e) {
						System.out.println("Monitoring was interrupted!");
						break;
//						Application.setMonitoring(false);
//						startMonitoringAction.setEnabled(true);
					}
				}
//				Application.setMonitoring(false);
//				startMonitoringAction.setEnabled(true);
			}
		};
		Thread th = new Thread(t);
		th.start();
		try {
			consumer.consumerReady();
		} catch (AcsJException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Override
	public void setFocus() {
		viewer.getControl().setFocus();

	}

}
