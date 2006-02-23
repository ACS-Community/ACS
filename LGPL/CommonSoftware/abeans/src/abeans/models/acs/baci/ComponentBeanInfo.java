/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci;

import java.awt.Image;
import java.beans.IntrospectionException;
import java.beans.PropertyDescriptor;
import java.beans.SimpleBeanInfo;

import com.cosylab.gui.components.util.ImageHelper;

/**
 * Component bean info.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public class ComponentBeanInfo extends SimpleBeanInfo {

	/**
	 * BeanInfo class.
	 */
	private final static Class beanClass = Component.class;

	/**
	 * @see java.beans.BeanInfo#getIcon(int)
	 */
	public Image getIcon(int iconKind) {
		return ImageHelper.createImage("Resources/icons/development/Bean16.gif");
	}

	/**
	 * @see java.beans.BeanInfo#getPropertyDescriptors()
	 */
	public PropertyDescriptor[] getPropertyDescriptors() {
		try {  
			PropertyDescriptor applicationContext = 
				new PropertyDescriptor("applicationContext", beanClass);
			applicationContext.setExpert(true);
			applicationContext.setShortDescription("Application context where this bean lives in.");
				
			PropertyDescriptor remoteName =
				new PropertyDescriptor("remoteName", beanClass);
			remoteName.setPreferred(true);
			remoteName.setShortDescription("Remote name of this bean.");
				
			PropertyDescriptor remoteInfo = 
				new PropertyDescriptor("remoteInfo", beanClass);
			remoteInfo.setExpert(true);
			remoteInfo.setShortDescription("Remote info of this bean.");
 
			PropertyDescriptor retVal[] = { applicationContext, remoteName, remoteInfo };
			return retVal;
			
		} catch (IntrospectionException e) {
			 throw new Error(e.toString());
		}
	}

}
