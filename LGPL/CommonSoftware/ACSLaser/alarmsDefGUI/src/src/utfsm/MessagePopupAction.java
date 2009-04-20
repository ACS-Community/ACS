package utfsm;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.IWorkbenchWindow;


public class MessagePopupAction extends Action {

    private final IWorkbenchWindow window;


    MessagePopupAction(String text, IWorkbenchWindow window) {
        super(text);
        this.window = window;
        // The id is used to refer to the action in a menu or toolbar
        setId(ICommandIds.CMD_OPEN_MESSAGE);
        // Associate the action with a pre-defined command, to allow key bindings.
        setActionDefinitionId(ICommandIds.CMD_OPEN_MESSAGE);	
    }

    public void run() {
     
				try {   
					Loader l =  new Loader("Test");
					l.load();
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				MessageDialog.openInformation(window.getShell(), "Information", "Refreshing");
            
    }
    
    }
