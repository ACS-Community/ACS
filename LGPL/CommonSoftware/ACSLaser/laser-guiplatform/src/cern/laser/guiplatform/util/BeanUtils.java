/*
 * BeanUtils.java
 *
 * Created on April 14, 2003, 3:45 PM
 */

package cern.laser.guiplatform.util;

import org.openide.util.Utilities;

/**
 *
 * @author  pawlowsk
 */
public class BeanUtils {
    
    /** Creates a new instance of BeanUtils */
    private BeanUtils() {
    }
    
    /**
     * This method
     * @param imagePathname 
     */
    public static final java.awt.Image loadImage(final String imagePathname, 
                                                final Class relatedClass) {
         return Utilities.loadImage(imagePathname);
         /*
         try {
            java.awt.image.ImageProducer ip = 
                (java.awt.image.ImageProducer) java.security.AccessController.doPrivileged(
                    new java.security.PrivilegedAction() {
                        public Object run() {
                            java.net.URL url = 
                                relatedClass.getClassLoader().getResource(
                                                                imagePathname);
                                
                            if (url == null)
                                return null;
                            try {
                                return url.getContent();
                            } catch (java.io.IOException ioe) {
                                return null;
                            }
                            
                        } 
                    });
            if (ip == null)
                return null;
            java.awt.Toolkit tk = java.awt.Toolkit.getDefaultToolkit();
            return tk.createImage(ip);
        } catch (Exception ex) {
            return null;
        }
        */
                                                    
        /*
        System.out.println(imagePathname);
        java.net.URL url =
            relatedClass.getClassLoader().getResource(imagePathname); 

        if (url == null)
            return null;
        javax.swing.ImageIcon icon = new javax.swing.ImageIcon(url);
        return icon.getImage();
        */
    } 
}
