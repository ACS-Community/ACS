package alma.acs.eventbrowser.views;

import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.ui.views.properties.*;

public class EventPropertiesSourceAdapterFactory implements IAdapterFactory {

	@Override
	public Object getAdapter(Object adaptableObject, Class adapterType) {
		if (adapterType == IPropertySource.class)
			return new EventPropertySource((EventData)adaptableObject);
		return null;
	}

	@Override

	public Class[] getAdapterList() {
		return new Class[] {IPropertySource.class};
	}

}
