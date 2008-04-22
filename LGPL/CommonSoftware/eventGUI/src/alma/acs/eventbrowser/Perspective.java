package alma.acs.eventbrowser;

import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

import alma.acs.eventbrowser.views.ChannelTreeView;
import alma.acs.eventbrowser.views.EventDetailView;
import alma.acs.eventbrowser.views.ServiceSummaryView;


public class Perspective implements IPerspectiveFactory {

	public void createInitialLayout(IPageLayout layout) {
		layout.setEditorAreaVisible(false);
		layout.addView(ChannelTreeView.ID, IPageLayout.LEFT, 1.0f, layout.getEditorArea());
		layout.addView(ServiceSummaryView.ID, IPageLayout.RIGHT, 0.25f, ChannelTreeView.ID);
		layout.addShowViewShortcut(ChannelTreeView.ID);
		layout.addShowViewShortcut(ServiceSummaryView.ID);
		layout.addView(EventDetailView.ID, IPageLayout.RIGHT, 0.5f, ServiceSummaryView.ID);
	}
}
