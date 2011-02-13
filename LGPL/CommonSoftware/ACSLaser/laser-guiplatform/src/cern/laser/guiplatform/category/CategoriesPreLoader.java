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

import alma.acs.container.ContainerServicesBase;

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
    
    private final ContainerServicesBase contSvcs;
    
    /** Creates a new instance of CategoriesPreLoader */
    public CategoriesPreLoader(ContainerServicesBase contSvcs) {
        categories = new HashMap();
        this.contSvcs=contSvcs;
        loadCategories();
    }
    public static synchronized CategoriesPreLoader getInstance(ContainerServicesBase contSvcs) {
        if ( INSTANCE == null )
            INSTANCE = new CategoriesPreLoader(contSvcs);
        
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
                    CategoryBrowsingHandlerFactory.getHandler(contSvcs).getCategoryTreeRoot();
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
            Collection children = CategoryBrowsingHandlerFactory.getHandler(contSvcs).getChildren( parent );
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
