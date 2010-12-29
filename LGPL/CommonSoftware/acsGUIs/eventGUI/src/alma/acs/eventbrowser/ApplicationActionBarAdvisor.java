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
	private IWorkbenchAction aboutAction;
	private IWorkbenchAction helpAction;
//	private IWorkbenchAction showViewsAction;

	public ApplicationActionBarAdvisor(IActionBarConfigurer configurer) {
        super(configurer);
    }

    protected void makeActions(IWorkbenchWindow window) {
        preferencesAction = ActionFactory.PREFERENCES.create(window);
        register(preferencesAction);
        exitAction = ActionFactory.QUIT.create(window);
        register(exitAction);
        aboutAction = ActionFactory.ABOUT.create(window);
        register(aboutAction);
        helpAction = ActionFactory.HELP_CONTENTS.create(window);
        register(helpAction);
//        showViewsAction = ActionFactory.SHOW_VIEW_MENU.create(window);
    }

    protected void fillMenuBar(IMenuManager menuBar) {
    	MenuManager eventGuiMenu = new MenuManager("Event Browser", "eventbrowser");
    	eventGuiMenu.add(preferencesAction);
    	eventGuiMenu.add(exitAction);
    	menuBar.add(eventGuiMenu);
    	MenuManager helpMenu = new MenuManager("&Help", "help");
    	helpMenu.add(helpAction);
    	helpMenu.add(aboutAction);
    	menuBar.add(helpMenu);
//    	MenuManager windowMenu = new MenuManager("Window", "window");
//    	windowMenu.add(showViewsAction);
//    	menuBar.add(windowMenu);
    }
    
}
