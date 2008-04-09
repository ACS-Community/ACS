package alma.acs.eventbrowser;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.ActionFactory.IWorkbenchAction;
import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;

public class ApplicationActionBarAdvisor extends ActionBarAdvisor {

    private IWorkbenchAction preferencesAction;

	public ApplicationActionBarAdvisor(IActionBarConfigurer configurer) {
        super(configurer);
    }

    protected void makeActions(IWorkbenchWindow window) {
        preferencesAction = ActionFactory.PREFERENCES.create(window);
        register(preferencesAction);

    }

    protected void fillMenuBar(IMenuManager menuBar) {
    	MenuManager eventGuiMenu = new MenuManager("Event Browser", "eventbrowser");
    	eventGuiMenu.add(preferencesAction);
    	menuBar.add(eventGuiMenu);
    }
    
}
