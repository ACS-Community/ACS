package alma.acs.eventbrowser.views;

import org.eclipse.jface.viewers.ColumnLabelProvider;

public class BaciParameterLabelProvider extends ColumnLabelProvider {
	@Override
	public String getText(Object element) {
		if (element instanceof ArchiveEventData)
			return ((ArchiveEventData) element).getParameter();
		return super.getText(element);
	}

}
