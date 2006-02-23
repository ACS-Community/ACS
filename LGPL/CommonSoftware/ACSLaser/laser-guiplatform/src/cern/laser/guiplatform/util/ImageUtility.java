/*
 * ImageUtility.java
 *
 * Created on February 20, 2003, 3:20 PM
 */

package cern.laser.guiplatform.util;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import org.apache.log4j.Logger;

/**
 *
 * @author  pawlowsk
 */
/**
 * This is utility class, which allows to play with images
 */
public class ImageUtility {
    
    /** logger */
    private static Logger logger = 
        LogFactory.getLogger(ImageUtility.class.getName());
    
   
    /**
     * This method creates icon from given path
     *
     * @param path for gif, path should start with "/"
     *
     * @return if path is null returns null
     *          else retrn <code>Icon</code> for given path
     */
    public static Icon getPanelIcon(Object obj, String path) {
        return getPanelIcon(obj, path, null);
    }
    
    /**
     * This method creates icon from given path
     *
     * @param path for gif, path should start with "/"
     *
     * @return if path is null returns null
     *          else retrn <code>Icon</code> for given path
     */
    public static Icon getPanelIcon(Object obj, String path, String imageDescription) {
        if ( path == null ) return null;
        
        java.net.URL iconPath = obj.getClass().getResource(path);
        
        if ( iconPath == null ) {
            logger.debug("icon path is null ");
        }
        
        ImageIcon icon = new ImageIcon(iconPath, imageDescription);
        
        return icon;
    }

}
