package alma.acs.eventbrowser.views;

import java.util.logging.Logger;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.part.ViewPart;

import alma.acs.eventbrowser.model.EventModel;

public class EventDetailView extends ViewPart {

	private TreeViewer viewer;
	
	private EventModel em;
	private Logger logger;
	
	public static final String ID = "alma.acs.eventbrowser.views.eventdetail";


	public EventDetailView() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public void createPartControl(Composite parent) {
		try {
			em = EventModel.getInstance();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		viewer = new TreeViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		viewer.setContentProvider(new DetailContentProvider());
		viewer.setLabelProvider(new DetailLabelProvider());
		
		// we're cooperative and also provide our selection
		// at least for the treeviewer
		getSite().setSelectionProvider(viewer);
		logger = em.getLogger();
		
		ISelectionService selsvc = getSite().getWorkbenchWindow().getSelectionService();
		selsvc.addSelectionListener(alma.acs.eventbrowser.views.EventListView.ID, mylistener);
		
	}

	@Override
	public void setFocus() {
		// TODO Auto-generated method stub

	}
	
    private ISelectionListener mylistener = new ISelectionListener() {
        public void selectionChanged(IWorkbenchPart sourcepart, ISelection selection) {
        if (sourcepart != EventDetailView.this &&
            selection instanceof IStructuredSelection) {
        		IStructuredSelection iss = (IStructuredSelection) selection;
        		Object[] arr = iss.toArray();
        		if (arr[0] instanceof EventData) {
        			EventData ed = (EventData) arr[0];
        			showEventDetails(sourcepart, ed);
        		}

            }
        }
    };
	public void showEventDetails(IWorkbenchPart sourcepart, EventData ed) {
		setContentDescription("Details of " + ed.getEventTypeName());
//		viewer.setInput(input);

	}
	
    public void dispose() {
        ISelectionService s = getSite().getWorkbenchWindow().getSelectionService();
        s.removeSelectionListener(mylistener);
        super.dispose();
    }


}
