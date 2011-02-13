package cern.laser.guiplatform.actions;


import java.awt.EventQueue;

import org.apache.log4j.Logger;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;
import org.openide.util.actions.CallableSystemAction;
import org.openide.windows.TopComponent;

import alma.acs.container.ContainerServicesBase;

import cern.laser.guiplatform.util.AppRegister;
import cern.laser.guiplatform.util.Constants;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windows.search.CategorySelectorWindow;


/** 
 * @author woloszyn
 */
public class SearchTrial extends CallableSystemAction {
    
    private Logger logger = LogFactory.getLogger(SearchTrial.class.getName());
    
    private final ContainerServicesBase contSvcs;
    
    public SearchTrial(ContainerServicesBase contSvcs) {
    	this.contSvcs=contSvcs;
    }
    
    public void performAction() {
        try {
            final TopComponent top = new CategorySelectorWindow(AppRegister.getInstance().getSearchCategories(), Constants.SEARCH_WINDOW_MODE_GET_ALARMINFO,contSvcs);
            top.open();
            final Runnable doLoadAllCategoriesInTreeExplorer = new Runnable() {
                public void run() {
                    ((CategorySelectorWindow)top).loadAllCategoriesInTreeExplorer();
                }
            };
            
            EventQueue.invokeLater(doLoadAllCategoriesInTreeExplorer);
        }
        catch( Exception e) {
            LogFactory.logException(logger,e);
        }
    }
    
    public String getName() {
        return NbBundle.getMessage(SearchTrial.class, "LBL_SearchTrial");
    }
    
    protected String iconResource() {
        return "cern/laser/guiplatform/images/eyes.gif";
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(TestAction.class);
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(TestAction.class, "HINT_Action"));
     * }
     */
    
}
