package alma.acs.eventbrowser.views;

import org.eclipse.jface.viewers.ColumnLabelProvider;

public class BaciPropertyValueLabelProvider extends ColumnLabelProvider {

	@Override
	public String getText(Object element) {
		if (element instanceof ArchiveEventData)
			return ((ArchiveEventData) element).getValue().toString();
		return super.getText(element);
	}

}
