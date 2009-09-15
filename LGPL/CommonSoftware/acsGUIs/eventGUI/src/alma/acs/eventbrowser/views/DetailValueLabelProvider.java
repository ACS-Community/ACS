package alma.acs.eventbrowser.views;

// See https://bugs.eclipse.org/bugs/show_bug.cgi?id=246026 for a tip on how to avoid all this, and get labels that are path
// dependent; also http://www.eclipsezone.com/eclipse/forums/t113574.html

import org.eclipse.jface.viewers.ColumnLabelProvider;

public class DetailValueLabelProvider extends ColumnLabelProvider {

	@Override
	public String getText(Object element) {
		if (element instanceof ParsedAnyData) {
			return ((ParsedAnyData)element).getValue();
		}
		return super.getText(element);
	}

}
