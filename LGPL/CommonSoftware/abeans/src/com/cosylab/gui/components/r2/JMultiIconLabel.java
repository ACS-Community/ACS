package com.cosylab.gui.components.r2;

import javax.swing.Icon;
import javax.swing.ImageIcon;
/**
 * This class extends <code>javax.swing.JLabel</code> so that it allows
 * display of multiple icons. It maintains internal list of icons that
 * associates integer values with <code>Icon</code> objects. This index
 * is guaranteed to be valid even if the desired icon could not be found,
 * is not valid or is null. In such cases the corresponding icon will not be
 * displayed.<p>
 * Icons can be provided by either the calling class via the
 * <code>javax.swing.Icon</code> object or they can be loaded from the
 * projects resource automatically.
 * Two methods are provided to add the icons: <code>addIcon, addIcons</code>
 * The later is preffered when adding multiple icons at the same time. <br>
 * Use the <code>setIconByIndex</code> method to set which icon should be
 * displayed.<br>
 * The disabled state icon of the JLabel is not overriden and can be
 * still specified.
 * <p>
 * Creation date: (12/24/2001 15:57:21)
 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
 */
public class JMultiIconLabel extends javax.swing.JLabel {
    private final java.util.Vector iconList = new java.util.Vector();
/**
 * Default constructor for JMultiIconLabel.
 */
public JMultiIconLabel() {
	super();
}
/**
 * Constructs a JMultiIconLabel and loads the icons from the specified 
 * resource names. 
 * @param text java.lang.String
 */
public JMultiIconLabel(String[] resourceNames) {
	this();
	addIcons(resourceNames);
}
/**
 * Constructs a JMultiIconLabel and loads the icons from the specified 
 * resource names. Also sets the horizontal alignement of the text.<p>
 * @param text java.lang.String
 * @param horizontalAlignement int.
 */
public JMultiIconLabel(String[] resourceNames, int horizontalAlignement) {
	this();
	addIcons(resourceNames);
	setHorizontalAlignment(horizontalAlignement);
}
/**
 * Constructs a JMultiIconLabel and adds the icons to the icon list.
 * @param image javax.swing.Icon
 */
public JMultiIconLabel(javax.swing.Icon[] icons) {
	this();
	addIcons(icons);
}
/**
 * Constructs a JMultiIconLabel and adds the icons to the icon list. Also
 * sets the horizontal alignement of the text.
 * @param image javax.swing.Icon
 * @param horizontalAlignment int
 */
public JMultiIconLabel(javax.swing.Icon[] icons, int horizontalAlignment) {
	this(icons);
	setHorizontalAlignment(horizontalAlignment);
}
/**
 * Appends the icon specified by resourceName to the end of internal list of
 * icons. This method calls addIcon(Icon) and loadFromResource(String). If
 * resourceName does not exist in the projects resources, method adds empty
 * icon.
 * <p>
 * Creation date: (12/24/2001 16:14:15)
 * @param resourceName java.lang.String
 */
public void addIcon(String resourceName) {
	addIcon(loadFromResource(resourceName));	
}
/**
 * Appends the icon to the list of icons.
 * <p>
 * Creation date: (12/24/2001 16:02:13)
 * @param icon javax.swing.Icon
 */
public void addIcon(javax.swing.Icon icon) {
	iconList.add(icon);	
}
/**
 * Appends all icons specified by resourceNames to the end of internal list of
 * icons. For adding multiple icons, this method outperforms individual calls
 * to <code>addIcon</code> method.<p>
 * After calling this method, the list of icons will grow by the size
 * of the array, regardless of its contentrs. If any of the icons is null
 * or of invalid type, the associated icon will not be displayed.
 * <p>
 * Creation date: (12/24/2001 16:21:36)
 * @param resourceNames java.lang.String[]
 */
public void addIcons(String[] resourceNames) {
	if (resourceNames != null) {
		for (int i = 0; i < resourceNames.length; i++) {
			addIcon(resourceNames[i]);
		}
	}
	
}
/**
 * Appends all icons specified by icons parameter to the internal list of
 * icons. For adding multiple icons, this method outperforms individual calls
 * to <code>addIcon</code> method.<p>
 * After calling this method, the list of icons will grow by the size
 * of the array, regardless of its contentrs. If any of the icons is null
 * or of invalid type, the associated icon will not be displayed.
 * <p>
 * Creation date: (12/24/2001 16:17:36)
 * @param icons javax.swing.Icon[]
 */
public void addIcons(Icon[] icons) {
    if (icons != null) {
        for (int i = 0; i < icons.length; i++) {
            addIcon(icons[i]);
        }
    }
}
/**
 * Returns the Icon resource assigned to the specified index. If the icon has
 * not been defined, returns null.
 * <p>
 * Creation date: (12/24/2001 16:10:44)
 * @return javax.swing.Icon
 * @param index int
 */
public Icon getIconByIndex(int index) {
    if ((index >= 0) && (index < iconList.size()))
        return (Icon) iconList.get(index);

    return null;
}
/**
 * Loads the Icon from the projects resource. Returns null if the resource
 * does not exist.
 * <p>
 * Creation date: (12/24/2001 16:04:52)
 * @return javax.swing.Icon
 * @param resourceName java.lang.String
 */
protected Icon loadFromResource(String resourceName) {
	ImageIcon icon;
    try {
        icon = new ImageIcon(getClass().getResource(resourceName));
        return icon;
    } catch (Exception e) {
	    if (resourceName.length() > 0)
	        System.out.println("Icon resource not found: " + resourceName);
    }
    return null;
}
/**
 * Displays the icon specified by index. If the icon with this index has 
 * not been defined, no icon will be displayed.
 * <p>
 * Creation date: (12/24/2001 16:09:33)
 * @param index int
 */
public void setIconByIndex(int index) {
	setIcon(getIconByIndex(index));		
}
}
