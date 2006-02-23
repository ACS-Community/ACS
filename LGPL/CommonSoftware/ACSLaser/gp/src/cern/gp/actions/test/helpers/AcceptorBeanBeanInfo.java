/*
 * AcceptorBeanBeanInfo.java
 *
 * Created on October 4, 2002, 4:30 PM
 */

package cern.gp.actions.test.helpers;

import cern.gp.beans.BeanTagger;
import cern.gp.beans.BeanInfoSupport;
import java.beans.BeanDescriptor;

/**
 * @author Vito Baggiolini
 * @version $Revision: 1.1 $ $Date: 2005/06/07 03:26:13 $
 */
public class AcceptorBeanBeanInfo extends BeanInfoSupport {
  
  /** Creates a new instance of AcceptorBeanBeanInfo */
  public AcceptorBeanBeanInfo() {
  }
  
  protected BeanDescriptor getBeanDescriptorLazy() {
    BeanDescriptor bd = new BeanDescriptor(AcceptorBean.class);
    BeanTagger.addActions(bd, new String[] { "cern.gp.actions.AcceptAction" });
    return bd;
  }
}
