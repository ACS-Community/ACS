package alma.acs.eventbrowser.views;

import java.util.ArrayList;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

import alma.acs.eventbrowser.Application;

public class EventDetailViewContentProvider implements IStructuredContentProvider {

	@Override
	public void dispose() {
		// TODO Auto-generated method stub

	}

	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		// TODO Auto-generated method stub

	}

	@Override
	public Object[] getElements(Object inputElement) {
		if (Application.equeue.isEmpty())
			return new EventData[0];
		EventData[] ed = Application.equeue.toArray(new EventData[0]);
		Application.equeue.clear();
		return ed;
	}

}
