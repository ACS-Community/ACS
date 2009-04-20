package utfsm;


import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.internal.dialogs.ViewContentProvider;
import org.eclipse.ui.part.ViewPart;


@SuppressWarnings("restriction")
public class View extends ViewPart {

	public static final String ID = "utfsm.view";
	
	
private TableViewer viewer;
	
	public void createPartControl(Composite parent) {
		
		FormToolkit toolkit = new FormToolkit(parent.getDisplay());
		// Lets make a layout for the first section of the screen
		GridLayout layout = new GridLayout();
		layout.numColumns = 1;
		layout.marginWidth = 2;
		layout.marginHeight = 2;
		// Creating the Screen
		Section section = toolkit.createSection(parent, Section.DESCRIPTION
				| Section.TITLE_BAR);
		section.setText("Alarm Configuration"); //$NON-NLS-1$
		section.setDescription("Setting Implementation");
		// Composite for storing the data
		Composite client = toolkit.createComposite(section, SWT.WRAP);
		layout = new GridLayout();
		layout.numColumns = 2;
		layout.marginWidth = 2;
		layout.marginHeight = 7;
		client.setLayout(layout);
		Table t = toolkit.createTable(client, SWT.NULL);
		GridData gd = new GridData(GridData.FILL_BOTH);
		gd.heightHint = 20;
		gd.widthHint = 100;
		t.setLayoutData(gd);
		toolkit.paintBordersFor(client);
		final org.eclipse.swt.widgets.Button b = toolkit.createButton(client, "Load", SWT.PUSH);
		gd = new GridData(GridData.GRAB_VERTICAL);		
		b.setLayoutData(gd); 
		b.addSelectionListener(new SelectionAdapter() {
	        public void widgetSelected(SelectionEvent e) {
	        	try {
					Loader l = new Loader("Test");
					l.load();
				} catch (Exception e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
	            }
	        });
		section.setClient(client);
		viewer = new TableViewer(t);
		viewer.setContentProvider(new ViewContentProvider());
//		viewer.setLabelProvider(new ViewLabelProvider());
		viewer.setInput(getViewSite());
		
	}


	public void setFocus() {
	}
}

