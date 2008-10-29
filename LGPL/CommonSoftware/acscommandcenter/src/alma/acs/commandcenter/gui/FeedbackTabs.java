/*
 * Created on Apr 6, 2004 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.awt.Component;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;

import javax.swing.JFileChooser;
import javax.swing.JTabbedPane;

import alma.acs.commandcenter.engine.NativeCommand;

/**
 * A JTabbedPane holding a FeedbackArea as the content of each tab. For each
 * tab, a Task.Listener is held that is resonsible for filling the FeedbackArea.
 * Thus we have (1 Tab : 1 FeedbackArea : 1 Listener). The storage key for all
 * is the Tab-name.
 */
public class FeedbackTabs extends JTabbedPane {

    
    protected CommandCenterGui master;

    public FeedbackTabs(CommandCenterGui master, int orientation) {
        super(orientation);
        this.master = master;
    }

    protected HashMap<String, NativeCommand.Listener> tabTitle2Listener = new HashMap<String, NativeCommand.Listener>();

    public NativeCommand.Listener viewTab(String tabTitle) {
        // search specified tab
        int tabIndex = super.indexOfTab(tabTitle);
        // if tab doesn't exist, create a tab + a listener
        if (tabIndex < 0) {
            FeedbackArea feedbackArea = new FeedbackArea(master, this, tabTitle);
            super.addTab(tabTitle, feedbackArea);
            tabTitle2Listener.put(tabTitle,
                    master.new WriteToFeedbackAreaTaskListener(feedbackArea));
            tabIndex = super.getTabCount() - 1;
        }
        // have tab displayed
        super.setSelectedIndex(tabIndex);
        // return a reference to the corresponding listener
        return (NativeCommand.Listener) tabTitle2Listener.get(tabTitle);
    }

    public void removeTab(String tabTitle) {
        try {
            tabTitle2Listener.remove(tabTitle);
            super.remove(super.indexOfTab(tabTitle));
        } catch (IndexOutOfBoundsException exc) {
        }
    }

    public void clearAllTabs() {
    	Component[] tabs = super.getComponents();
    	for (int i = 0; i < tabs.length; i++) {
    		try{
    			((FeedbackArea)tabs[i]).clear();
    		}catch(Exception exc){}
}
    }

    
    protected JFileChooser fileChooser;
    
    public void saveTab (FeedbackArea x) {

    	// lazy creation
    	if (fileChooser == null) {
    		fileChooser = new JFileChooser();
    	}

    	// show save dialog
    	int answer = fileChooser.showSaveDialog(this);
    	if (answer == JFileChooser.APPROVE_OPTION) {

    		File f = null;
    		FileWriter fw = null;

    		try {
       		// write text to file
    			f = fileChooser.getSelectedFile();
    			fw = new FileWriter(f);
				fw.write(x.outputArea.getText());
				fw.flush();

    		} catch (IOException exc) {
				master.log.fine("couldn't save log '"+x.surroundingTabTitle+"' to file '"+f+"' due to "+exc);
			
    		} finally {
				try {
					fw.close();
				}catch(Exception exc){}
			}
    		
    	}
    }
    
}

////////////////////////////////////////////////////////
/// ------------------- API ------------------------ ///
////////////////////////////////////////////////////////

////////////////////////////////////////////////////////
/// ----------------- Internal --------------------- ///
////////////////////////////////////////////////////////

//
//
//
//
//
//
//
//
//
//
//
//