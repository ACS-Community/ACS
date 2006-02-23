/*
 * CategoryHandler.java
 *
 * Created on July 5, 2003, 4:15 PM
 */

package cern.laser.guiplatform.category.helpers;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;

import cern.laser.client.data.Category;
import cern.laser.client.services.browsing.CategoryBrowsingHandler;
import cern.laser.guiplatform.util.LogFactory;

/**
 * This class is used for test indead of business CategoryBrowsinngHandler
 *
 * @author  pawlowsk
 */
public class CategoryHandler extends CategoryBrowsingHandler {
    
    /** logger */
    static Logger logger = LogFactory.getLogger(CategoryHandler.class.getName());
    
    private static CategoryBrowsingHandler INSTANCE = null;
    
    static Category categoryRoot = null;
    static String catName = "cern";
    static String catDesc = "Category description";
    
    static int maxCategoryCounter = 3;
    
    static Map children = null;    

    static {
        categoryRoot = new CategoryImpl(catName, catDesc, 1, "path1", false);
       
        int internalIdent = 100;     
        ArrayList list = new ArrayList();
        for (int i = 0; i < maxCategoryCounter; i++) {
            list.add(new CategoryImpl(catName + i, catDesc, internalIdent + i,
                        "path", false));
        }           
        
        String internalName = catName + maxCategoryCounter;
        internalIdent += maxCategoryCounter;
        
        children = new HashMap();
        
        children.put(categoryRoot.getCategoryId(), list);
        
        List listTemp = null;
        for (Iterator iter = list.iterator(); iter.hasNext(); ) {
            Category catTemp = (Category) iter.next();
            listTemp = new ArrayList();
            for (int i = 0; i < maxCategoryCounter; i++) {
                listTemp.add(new CategoryImpl(internalName + i, catDesc, internalIdent + i,
                            "path", true));
            }           
            internalName += maxCategoryCounter;
            internalIdent += maxCategoryCounter;
            
            children.put(catTemp.getCategoryId(), listTemp);
        }
    }
    
    
    /** Creates a new instance of CategoryHandler */
    public CategoryHandler() {
    }
    
    public Category getCategoryById(Integer param) {
        throw new UnsupportedOperationException("this method is not implemented");
    }
    
    public Category getCategoryByPath(String str) {
        throw new UnsupportedOperationException("this method is not implemented");
    }
    
    public Category getCategoryTreeRoot() {
        return categoryRoot;
    }
    
    public java.util.Collection getChildren(Category category) {
        logger.debug("category Id: " + category.getCategoryId());
        return getChildren(category.getCategoryId());
    }
    
    public java.util.Collection getChildren(Integer nodeId) {
        //throw new UnsupportedOperationException("this method is not implemented");
        return (Collection) children.get(nodeId);
    }
    
    public Category getParent(Category category) {
        throw new UnsupportedOperationException("this method is not implemented");
    }
    
    public Category getParent(Integer param) {
        throw new UnsupportedOperationException("this method is not implemented");
    }
    
    public Collection getCategories() {
        throw new UnsupportedOperationException("this method is not implemented");
    }
    
    //
    // -- sigleton methods -------------
    //
    public static CategoryBrowsingHandler get() {
        if ( INSTANCE == null ) 
            INSTANCE = new CategoryHandler();
        
        return INSTANCE;
    }
}
