package alma.acs.eventbrowser.views;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import alma.acs.util.UTCUtility;

public class TimeStampLabelProvider extends ColumnLabelProvider {

	@Override
	public String getText(Object element) {
		if (element instanceof AbstractEventData)
			return UTCUtility.getUTCDate(UTCUtility.utcOmgToJava(((AbstractEventData) element).getTimestamp()));
		return super.getText(element);
	}

}
