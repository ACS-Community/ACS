package alma.acs.eventbrowser;

import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

import alma.acs.eventbrowser.views.ChannelTreeView;


public class Perspective implements IPerspectiveFactory {

	public void createInitialLayout(IPageLayout layout) {
		layout.setEditorAreaVisible(false);
		layout.addView(ChannelTreeView.ID, IPageLayout.LEFT, 1.0f, layout.getEditorArea());
	}
}
