/*
 * ACExplorer.java
 *
 * Created on June 3, 2004, 10:28 PM
 */

package cern.laser.guiplatform.explorer;

import org.apache.log4j.Logger;
import org.openide.explorer.ExplorerPanel;
import org.openide.nodes.Node;
import org.openide.nodes.PropertySupport;
import org.openide.util.NbBundle;

import cern.laser.guiplatform.alarms.AlarmNodeManager;
import cern.laser.guiplatform.util.Constants;
import cern.laser.guiplatform.util.LogFactory;


/**
 *
 * @author  woloszyn
 */
public abstract class ACExplorer extends ExplorerPanel {
    private ACTreeTableView view = null;
    private static Logger logger = LogFactory.getLogger(ACExplorer.class.getName());    
    
    /** Creates a new instance of ACExplorer */
    public ACExplorer( AlarmNodeManager nodeManager, String [] columnsToDisplay ) {
        super(nodeManager.getExplorerManager() );
        logger.debug("<begin> constructor");        
        Node root = nodeManager.getRootNode();
        view = new ACTreeTableView(root);
        view.setRootVisible(false);        
        setTableColumns( columnsToDisplay );
        setLayout(new java.awt. BorderLayout());       
        add("Center", view);        
        logger.debug("<end> constructor");                
    }
    public void setTableColumns(String[] propNames) {
        logger.debug("<begin> setTableColumns");
        
        String[] aleStr = new String[8];
        aleStr[0]="date";
        aleStr[1]="time";
        aleStr[2]="faultFamily";
        aleStr[3]="faultMember";
        aleStr[4]="faultCode";
        //aleStr[5]="systemName";
        aleStr[5]="identifier";
        aleStr[6]="priority";
        //aleStr[8]="problemDescription";
        aleStr[7]="sourceName";
        propNames=aleStr;
        Node.Property [] properties = new Node.Property[propNames.length];        
        
        for( int i=0; i<propNames.length; i++) {  
            Class type;
            if( propNames[i].equalsIgnoreCase("faultCode") || propNames[i].equalsIgnoreCase("priority") ){
                type = Integer.class;
                logger.debug("property="+propNames[i]+" set to Integer class");
            }
            else{
                type = String.class;
            }            
            properties[i] = new PropertySupport.ReadOnly(
            propNames[i],
            type,
            NbBundle.getMessage(Constants.class, propNames[i] +"_displayName"),
            NbBundle.getMessage(cern.laser.guiplatform.util.Constants.class, propNames[i] +"_displayName") 
            ){
                public Object getValue() {
                    return "";
                }
            };
                         
        }  
        view.setProperties( properties );
        view.setRenderers();
        logger.debug("<end> setTableColumns");                
    } 
        
    public ACTreeTableView getView() {
        return view;
    }
}
