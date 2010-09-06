package alma.acs.eventbrowser.views;

import org.eclipse.jface.viewers.ColumnLabelProvider;

public class EventSourceLabelProvider extends ColumnLabelProvider {

	@Override
	public String getText(Object element) {
		if (element instanceof EventData)
			return ((EventData) element).getChannelName();
		if (element instanceof ArchiveEventData)
			return ((ArchiveEventData) element).getDevice();
		return super.getText(element);
	}

}
