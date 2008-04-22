package alma.acs.eventbrowser.views;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import alma.acs.util.UTCUtility;

public class TimeStampLabelProvider extends ColumnLabelProvider {

	@Override
	public String getText(Object element) {
		if (element instanceof EventData)
			return UTCUtility.getUTCDate(UTCUtility.utcOmgToJava(((EventData) element).getTimestamp()));
		return super.getText(element);
	}

}
