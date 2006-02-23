/*
 * FilterItemBean.java
 *
 * Created on March 19, 2003, 1:48 PM
 */

package cern.laser.guiplatform.filters;

import cern.gp.beans.PropertyInfo;
import cern.gp.beans.PropertyInfoSupport;
import cern.laser.client.services.selection.Filter;

//import cern.gp.beans.editors.support.ColorMaster;

/**
 * Class stores inforamtion about filter and is used to display filters in 
 * TableListExplorer.
 * This class stores inforamation as name/value pairs. (i. e. fault_family like XXXX).
 *
 * This class could be wrapper for FilterBean from business layer.
 *
 * @author  pawlowsk
 */
public class FilterItemBean extends cern.gp.beans.BeanSupport {
   
    /** name */
    //private String name = null;
   
    /** operator */
    //private String operator = null;
   
    /** value */
    //private String value = null;
  
    /** filter from business layer */
    private Filter filter = null;


    /** node actions */
    static String [] nodeActions = null;

    /** default node action */
    private String defaultNodeAction = 
        //"cern.laser.guiplatform.actions.filters.DeleteAllFiltersAction";
        //"cern.laser.guiplatform.actions.filters.DeleteFilterAction";
        "cern.laser.guiplatform.actions.filters.EditFilterAction";

    private static final String [] EMPTY_NODE_ACTIONS = new String [] {};
    
    /** Creates a new instance of FilterItemBean */
    /*
    public FilterItemBean(String name, String operator, String value) {
        this.name = name;
        this.operator = operator;
        this.value = value;

        
        nodeActions =  new String[] {          
            "cern.laser.guiplatform.actions.filters.EditFilterAction",
            "cern.laser.guiplatform.actions.filters.DeleteFilterAction",
            "cern.laser.guiplatform.actions.filters.DeleteAllFiltersAction"
        };
        
    }
    */
    /** Creates a new instance of FilterItemBean 
     * !!! WARNING !!!!!
     * this constructor should be used only when one wants to set table 
     * columns for CreatedFilterExplorer
     */
    public FilterItemBean() {
        //this.name = "";
        //this.operator = "";
        //this.value = "";
       nodeActions = EMPTY_NODE_ACTIONS;
       defaultNodeAction = null;

   }
     
    /**
     * @param bean filter bean from business layer
     *
     */
    public FilterItemBean(Filter filter) {
        this.filter = filter;

        nodeActions =  new String[] {          
            "cern.laser.guiplatform.actions.filters.EditFilterAction",
            "cern.laser.guiplatform.actions.filters.DeleteFilterAction",
            "cern.laser.guiplatform.actions.filters.DeleteAllFiltersAction"
        };
 
    }
   
    public Filter getFilter() {
       return filter;
    } 
   
    // ----------------------------------------- set/get mehtod
    //public void setName(String name) {
    //    this.name = name;
    //}
    public String getName() {
        return filter.getProperty();//name;
    }
   
    //public void setOperator(String operator) {
    //    this.operator = operator;
    //}
    public String getOperator() {
        return filter.getOperator();
    }

    //public void setValue(String value) {
    //    this.value = value;
    //}
    public String getValue() {
        return filter.getValue();
    }

    // ---------------------------------- methods from BeanSupport
    public String[] getNodeActions() {
        return nodeActions;
    }
    
    public String getNodeDefaultAction() {
        return defaultNodeAction;
    }
    
    public java.awt.Image getNodeIcon() {
        return super.getNodeIcon();
    }
    
    public cern.gp.beans.PropertyInfo[] getPropertyInfo() {
        
        return new PropertyInfo[] {
            new PropertyInfoSupport("operator", "Operator"),
            new PropertyInfoSupport("value", "Value"),
            
            
        };
    }
}   // end FilterItemBean class
