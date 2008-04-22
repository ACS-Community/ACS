package alma.acs.eventbrowser.views;

import org.eclipse.jface.viewers.ColumnLabelProvider;

public class CountLabelProvider extends ColumnLabelProvider {
	@Override
	public String getText(Object element) {
		if (element instanceof EventData)
			return ""+((EventData) element).getChannelEventCount();
		return super.getText(element);
	}
}
