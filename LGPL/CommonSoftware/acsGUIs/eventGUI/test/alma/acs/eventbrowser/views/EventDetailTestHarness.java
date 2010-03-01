package alma.acs.eventbrowser.views;

/*******************************************************************************
 * Copyright (c) 2006 Tom Schindl and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Tom Schindl - initial API and implementation
 *******************************************************************************/

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.omg.CORBA.Any;
import org.omg.CosNotification.StructuredEvent;

import tdem.TDEM_TOPICS.actuatorSpace;
import tdem.TDEM_TOPICS.pttDataEvent;
import alma.acs.eventbrowser.model.EventModel;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.StructuredEventCreator;
import alma.acs.util.StopWatch;

/**
 * A simple TableViewer to demonstrate usage
 * 
 * @author Tom Schindl <tom.schindl@bestsolution.at>
 * 
 */
public class EventDetailTestHarness {
	private static class MyContentProvider implements IStructuredContentProvider {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.jface.viewers.IStructuredContentProvider#getElements(
		 * java.lang.Object)
		 */
		public Object[] getElements(Object inputElement) {
			return (ParsedAnyData[]) inputElement;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.jface.viewers.IContentProvider#dispose()
		 */
		public void dispose() {

		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse
		 * .jface.viewers.Viewer, java.lang.Object, java.lang.Object)
		 */
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {

		}

	}

	private static Logger logger;
	private static StructuredEventCreator seCreator;
	private TableViewer viewer;
	private Clipboard cb;

	public EventDetailTestHarness(Shell shell) {
		final TableViewer v = new TableViewer(shell, SWT.MULTI | SWT.H_SCROLL
				| SWT.V_SCROLL);
		
		viewer = v;
		
		Table table = v.getTable();
		table.setHeaderVisible(true);
		table.setLinesVisible(true);

		GridLayout gridLayout = new GridLayout();
		gridLayout.marginHeight = 0;
		gridLayout.marginWidth = 0;
		gridLayout.verticalSpacing = 0;
		shell.setLayout(gridLayout);
		
		TableViewerColumn tvcol = new TableViewerColumn(v, SWT.NONE, 0);
		tvcol.setLabelProvider(new DetailNameLabelProvider());
		TableColumn col = tvcol.getColumn();
		col.setText("Name");
		col.setWidth(180);
		col.setAlignment(SWT.LEFT);

		tvcol = new TableViewerColumn(v, SWT.NONE, 1);
		tvcol.setLabelProvider(new DetailTypeLabelProvider());
		col = tvcol.getColumn();
		col.setText("Type");
		col.setWidth(100);
		col.setAlignment(SWT.LEFT);

		tvcol = new TableViewerColumn(v, SWT.NONE, 2);
		tvcol.setLabelProvider(new DetailValueLabelProvider());
		col = tvcol.getColumn();
		col.setText("Value");
		col.setWidth(50);
		col.setAlignment(SWT.LEFT);
		
		GridDataFactory.fillDefaults().grab(true, true).applyTo(v.getTable());

		v.setContentProvider(new MyContentProvider());
		ParsedAnyData[] model = createModel();
		v.setInput(model);
		v.getTable().setLinesVisible(true);
		
		Menu popUpMenu = new Menu(shell,SWT.POP_UP);
		viewer.getControl().setMenu(popUpMenu);
		MenuItem copyItem = new MenuItem(popUpMenu,SWT.PUSH);
		copyItem.setText("Copy");
		cb = new Clipboard(Display.getDefault());
		copyItem.addSelectionListener(new SelectionListener() {
			
			@Override
			public void widgetSelected(SelectionEvent e) {
				copySelection();
				
			}

			@SuppressWarnings("unchecked")
			private void copySelection() {
				List<ParsedAnyData> personList = new ArrayList<ParsedAnyData>();
				ISelection selection = viewer.getSelection();
				if (selection != null && selection instanceof IStructuredSelection) {
					IStructuredSelection sel = (IStructuredSelection) selection;
					for (Iterator iterator = sel
							.iterator(); iterator.hasNext();) {
						ParsedAnyData person = (ParsedAnyData) iterator.next();
						personList.add(person);
					}
				} else
					return;
				StringBuilder sb = new StringBuilder();
				for (ParsedAnyData person : personList) {
					sb.append(parsedAnyDataToString(person));
				}
				TextTransfer textTransfer = TextTransfer.getInstance();
				cb.setContents(new Object[] { sb.toString() },
						new Transfer[] { textTransfer });
			}
			
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {		
			}

			private String parsedAnyDataToString(ParsedAnyData person) {
				return person.getName() + "\t" + person.getType() + "\t"
						+ person.getValue() + System.getProperty("line.separator");
			}
		});
	}
	
	private ParsedAnyData[] createModel() {
		ParsedAnyData[] elements = parsePttDataEvent();
		return elements;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		EventModel em = null;
		try {
			em = EventModel.getInstance();
			logger = em.getLogger();
		} catch (Exception e) {
			e.printStackTrace();
			System.out.println("Couldn't create the Event Model.");
			System.exit(1);
		}
		seCreator = new StructuredEventCreator(em.getContainerServices());
		Display display = new Display();
		Shell shell = new Shell(display);
		shell.setLayout(new FillLayout());
		new EventDetailTestHarness(shell);
		shell.open();

		while (!shell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}

		display.dispose();

	}

	public static ParsedAnyData[] parsePttDataEvent() {
		pttDataEvent pde = new pttDataEvent(
				new actuatorSpace(new double[2952]), new actuatorSpace(
						new double[2952]), 25, 32L);
		StructuredEvent se = null;
		String eventName = null;
		Any eventAny = null;
		try {
			se = seCreator.createEvent(pde);
			eventName = se.header.fixed_header.event_type.type_name;
			eventAny = se.filterable_data[0].value;
		} catch (AcsJException e) {
			e.printStackTrace();
			System.err
					.println("Couldn't create structured event for pttDataEvent");
		}
		StopWatch sw = new StopWatch(logger);
		DynAnyParser parser = new DynAnyParser(eventAny, eventName);
		ParsedAnyData[] pResults = parser.getParsedResults(null);
		sw.logLapTime("parse this eventAny");
		return pResults;
	}

}
