/*
 * CategoryTest.java
 *
 * Created on March 3, 2003, 10:50 AM
 */

package cern.laser.guiplatform.category;

import java.awt.Image;

import org.apache.log4j.Logger;
import org.openide.util.Utilities;

import cern.gp.beans.BeanSupport;
import cern.gp.beans.PropertyInfo;
import cern.laser.client.data.Category;
import cern.laser.guiplatform.actions.category.AddCategoryAction;
import cern.laser.guiplatform.actions.category.AddSubCategoriesAction;
import cern.laser.guiplatform.actions.category.DeleteAllChoosenCategoryAction;
import cern.laser.guiplatform.actions.category.DeleteChoosenCategoryAction;
import cern.laser.guiplatform.actions.category.RemoveCategoryAction;
import cern.laser.guiplatform.actions.category.RemoveSubCategoriesAction;
import cern.laser.guiplatform.actions.category.ShowAlarmByCategoryAction;
import cern.laser.guiplatform.util.LogFactory;


/**
 *
 * @author  pawlowsk
 */
public class CategoryBean extends BeanSupport implements Cloneable {
   
    /** logger */
    static Logger logger = LogFactory.getLogger(CategoryBean.class.getName());
    
    /** category */
    private Category category = null; 
        
    private static final String [] nodeActions = new String[] {       
                    AddCategoryAction.class.getName(),
                    AddSubCategoriesAction.class.getName(),
                    null,
                    RemoveCategoryAction.class.getName(),
                    RemoveSubCategoriesAction.class.getName(),
                    null,
                    ShowAlarmByCategoryAction.class.getName(), };
 
    private static final String [] clonedNodeActions = new String[] {          
                    DeleteChoosenCategoryAction.class.getName(),
                    DeleteAllChoosenCategoryAction.class.getName(),
                    null,
                    ShowAlarmByCategoryAction.class.getName(), };
 

    private static final String defaultNodeActionSelected = RemoveCategoryAction.class.getName();
    
    private static final String defaultNodeActionNotSelected = AddCategoryAction.class.getName();

    private static final String defaultClonedNodeAction = RemoveCategoryAction.class.getName();


    /** indicates that this bean is cloned or not */
    private boolean isCloned = false;
    
    public static final int STATE_CHILDREN_NOT_SELECTED = 1;
    public static final int STATE_CHILDREN_PARTIALY_SELECTED = 2;
    public static final int STATE_CHILDREN_ALL_SELECTED = 3;
    
    private int childrenSelectionState = STATE_CHILDREN_NOT_SELECTED;    
    private boolean isSelected = false;        

    /** Creates a new instance of CategoryTest */
    public CategoryBean(Category category) {
        this.category = category;
    }

    public void setSelected( boolean flag ) {
        isSelected = flag;
        fireNodeIconPropertyChange( getNodeIcon() );
    }
    public boolean isSelected() {
        return isSelected;
    }
    public void setChildrenSelectionState( int state ) {
        childrenSelectionState = state;
        fireNodeIconPropertyChange( getNodeIcon() );
    }
    public int getChildrenSelectionState() {
        return childrenSelectionState;
    }
    public Category getCategory() {
        return category;
    }
    
    //
    // -- Category methods ------------------------------------------
    //
    /*
    public java.util.Collection getChildren() {
        return children;
    }
    
    public String getDescription() {
        return description;
    }
    
    public int getIdentifier() {
        return identifier;
    }
     
   
    public cern.laser.client.services.selection.Category getParent() {
        return null;
    }
    
    public boolean isLeaf() {
        return true;
    }
    */ 
    
    //
    // --   method from BeanSupport -------------------------------------------
    //
    public java.awt.Image getNodeIcon() {
        if ( isCloned ) {
            return Utilities.loadImage("cern/laser/guiplatform/images/category_selected_nochildren.gif");
        }
        Image image = null;
        switch( getChildrenSelectionState() ) {
            case STATE_CHILDREN_NOT_SELECTED :
                if ( isSelected() ) {
                    image = Utilities.loadImage("cern/laser/guiplatform/images/category_selected_nochildren.gif");
                }
                else {
                    image = Utilities.loadImage("cern/laser/guiplatform/images/category_notselected_nochildren.gif");
                }
                break;
            case STATE_CHILDREN_PARTIALY_SELECTED :
                if ( isSelected() ) {
                    image = Utilities.loadImage("cern/laser/guiplatform/images/category_selected_partchildren.gif");
                }
                else {
                    image = Utilities.loadImage("cern/laser/guiplatform/images/category_notselected_partchildren.gif");
                }
                break;
            case STATE_CHILDREN_ALL_SELECTED :
                if ( isSelected() ) {
                    image = Utilities.loadImage("cern/laser/guiplatform/images/category_selected_allchildren.gif");
                }
                else {
                    image = Utilities.loadImage("cern/laser/guiplatform/images/category_notselected_allchildren.gif");
                }
                break;
        }
        if ( image == null) {
            return super.getNodeIcon();
        }
        return image;
    }
  
    
    public String getName() {
        return category.getName();
    }
    public String getPath() {
        return category.getPath();
    }

    /*
    public void setName(String name) {
        this.name = name;
    }
    */
    
    public String getDisplayName() {
        return category.getName();
    }
   
    public Integer getCategoryId() {
       return category.getCategoryId();
    } 
    
    public String getShortDescription() {
        return category.getDescription();
    }
    
    public String[] getNodeActions() {
        /*return new String[] {          
            "cern.laser.guiplatform.categorybeans.CategoryCopyOverNodesAction"
            //,"cern.gp.actions.PropertiesAction"
            //,"cern.gp.actions.OpenLocalExplorerAction"
        };
         */

        if ( !isCloned )
            return nodeActions;
        else
            return clonedNodeActions;

    }
    /* 
    public void setNodeActions(String [] nodeActions) {
        this.nodeActions = nodeActions;
        super.firePropertyChange(super.NODE_ACTIONS_PROPERTY_NAME, null, nodeActions);
    }
    */
    
    public String getNodeDefaultAction() {
        if ( !isCloned ) {
            if ( ! isSelected() )
                return defaultNodeActionNotSelected;
            else
                return defaultNodeActionSelected;
        }
        else {
            return defaultClonedNodeAction;
        }
    } 

    /*
    public void setNodeDefaultAction(String nodeDefaultAction) {
        this.defaultNodeAction = nodeDefaultAction;
        super.fireNodeDefaultActionPropertyChange(nodeDefaultAction);
        //super.firePropertyChange(super.NODE_DEFAULT_ACTION_PROPERTY_NAME, null, nodeDefaultAction);
    }
    */

    /**
     * from Cloneable interface
     */
    public Object clone() throws CloneNotSupportedException {
        //return super.clone();
        CategoryBean cloned = new CategoryBean(this.category);
        cloned.setCloned();
        //test.setNodeActions(this.nodeActions);
        //test.setNodeDefaultAction(this.defaultNodeAction);
        //test.setChildren(this.children);

        return cloned;
    }

    public void setCloned() {
        isCloned = true;
    }

    //
    // ----------------------- customize bean
    //
    public PropertyInfo[] getPropertyInfo() {
        
        return new PropertyInfo[] {
            //new PropertyInfoSupport("niceExercise", "Are the exercises nice?"
            //           ,test.MyEditor.class),
            //new PropertyInfoSupport("defaultNodeAction", true),
            //new PropertyInfoSupport("name", "Name"),
            //new PropertyInfoSupport("shortDescription", true),
            //new PropertyInfoSupport("displayName", true),
            
        };
    }
    /*    
    private String nice = "";
    public void setNiceExercise (String val) {
        nice = val;
    }
    public String getNiceExercise () {
        return nice;
    }
    */
}
