/*
 * CategoriesPreLoader.java
 *
 * Created on November 4, 2004, 11:40 AM
 */

package cern.laser.guiplatform.category;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;

import org.apache.log4j.Logger;

import cern.laser.client.LaserException;
import cern.laser.client.data.Category;
import cern.laser.guiplatform.util.LogFactory;

/**
 *
 * @author  woloszyn
 */
public class CategoriesPreLoader {
    private static CategoriesPreLoader INSTANCE = null;
    private Logger logger = LogFactory.getLogger(CategoriesPreLoader.class.getName());
    private HashMap categories = null;
    
    /** Creates a new instance of CategoriesPreLoader */
    public CategoriesPreLoader() {
        categories = new HashMap();
        loadCategories();
    }
    public static synchronized CategoriesPreLoader getInstance() {
        if ( INSTANCE == null )
            INSTANCE = new CategoriesPreLoader();
        
        return INSTANCE;
    }
    public boolean isLoaded(Category category) {
        return categories.containsKey( category.getCategoryId());
    }
    public void loadCategories() {
        final Runnable doLoadAllCategories = new Runnable() {
            public void run() {
                try {
                    Category categoryRoot =
                    CategoryBrowsingHandlerFactory.getHandler().getCategoryTreeRoot();
                    loadCategory( categoryRoot );
                    logger.debug("Categories Pre Loading finished");
                }
                catch ( LaserException le) {
                    logger.debug(le.getMessage(), le);
                }
            }
        };
        
        new Thread(doLoadAllCategories).start();
    }
    
    public Collection getChildren(Category parentCategory) {
        CategoryItem item = (CategoryItem) categories.get( parentCategory.getCategoryId() );
        return (Collection) item.getChildren();
    }
    
    private void loadCategory(Category parent) {
        try {
            Collection children = CategoryBrowsingHandlerFactory.getHandler().getChildren( parent );
            CategoryItem item = new CategoryItem(parent, children);
            categories.put(parent.getCategoryId(), item );
            Iterator iter = children.iterator();
            while ( iter.hasNext() ) {
                Category cat = (Category) iter.next();
                loadCategory( cat );
            }
        }
        catch ( LaserException le) {
            logger.debug(le.getMessage(), le);
        }
    }
    
    class CategoryItem {
        private Category category;
        private Collection children;
        
        public CategoryItem(Category category, Collection children) {
            this.category = category;
            this.children = children;
        }
        
        public Collection getChildren() {
            return children;
        }
    }
}
