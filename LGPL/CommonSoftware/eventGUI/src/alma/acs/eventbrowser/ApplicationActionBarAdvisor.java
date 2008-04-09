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
	private IWorkbenchAction exitAction;

	public ApplicationActionBarAdvisor(IActionBarConfigurer configurer) {
        super(configurer);
    }

    protected void makeActions(IWorkbenchWindow window) {
        preferencesAction = ActionFactory.PREFERENCES.create(window);
        register(preferencesAction);
        exitAction = ActionFactory.QUIT.create(window);
        register(exitAction);

    }

    protected void fillMenuBar(IMenuManager menuBar) {
    	MenuManager eventGuiMenu = new MenuManager("Event Browser", "eventbrowser");
    	eventGuiMenu.add(preferencesAction);
    	eventGuiMenu.add(exitAction);
    	menuBar.add(eventGuiMenu);
    }
    
}
