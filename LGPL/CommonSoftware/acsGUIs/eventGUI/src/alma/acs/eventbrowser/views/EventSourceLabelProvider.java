package alma.acs.eventbrowser.views;

import org.eclipse.jface.viewers.ColumnLabelProvider;

public class EventSourceLabelProvider extends ColumnLabelProvider {

	@Override
	public String getText(Object element) {
		if (element instanceof EventData)
			return ((EventData) element).getChannelName();
		return super.getText(element);
	}

}
