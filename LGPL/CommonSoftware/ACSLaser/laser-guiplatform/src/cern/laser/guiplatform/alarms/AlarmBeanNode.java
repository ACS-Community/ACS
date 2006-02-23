/*
 * AlarmBeanNode.java
 *
 * Created on March 24, 2004, 11:56 AM
 */

package cern.laser.guiplatform.alarms;

import java.beans.IntrospectionException;
import java.beans.PropertyChangeListener;

import org.apache.log4j.Logger;
import org.openide.nodes.BeanNode;
import org.openide.nodes.Children;
import org.openide.util.actions.SystemAction;

import cern.laser.guiplatform.util.LogFactory;

/**
 *
 * @author  woloszyn
 */
public class AlarmBeanNode extends BeanNode implements PropertyChangeListener, Comparable {
    private static final Logger logger = LogFactory.getLogger(AlarmBeanNode.class.getName());
    public AlarmBeanNode(Object bean) throws IntrospectionException {
        super(bean);
        registerPropertyChangeListener(bean);
    }
    public AlarmBeanNode(Object bean, Children children)
    throws IntrospectionException {
        super(bean, children);
        registerPropertyChangeListener(bean);
    }
    public SystemAction[] createActions() {
        try {
            AlarmBean bean = (AlarmBean) getBean();
            return bean.getActions();
        }
        catch (ClassCastException cce) {
            return new SystemAction [] {SystemAction.get(cern.laser.guiplatform.actions.TestAction.class)};
        }
    }
    public String getName() {
        try {
            AlarmBean bean = (AlarmBean) getBean();
            if ( isLeaf()!=true) {
                return "Alarm";
            }
            else {
                return bean.getDisplayName();
                //return bean.getAlarmId().toString();
            }
        }
        catch (ClassCastException cce) {
            return "Alarm";
        }
    }
    public Object getBean(){
        return super.getBean();
    }
    
    public void propertyChange(java.beans.PropertyChangeEvent evt) {
        //logger.debug("propertyChange, event="+evt);
        
        if(evt.getPropertyName().equals("name") ) {
            super.fireNameChange(null, (String) evt.getNewValue() );
        }
        else {
            super.firePropertyChange(null, null, null);            
        }
    }
    private void registerPropertyChangeListener(Object bean) {
        try{
            ((AlarmBean)bean).registerPropertyChangeListener(this);
        }
        catch (ClassCastException cce) {

        }
    }
    
    public int compareTo(Object o) {
        AlarmBeanNode node = (AlarmBeanNode) o;
        return ((AlarmBean)node.getBean()).compareTo((AlarmBean) getBean());
    }
    
}
