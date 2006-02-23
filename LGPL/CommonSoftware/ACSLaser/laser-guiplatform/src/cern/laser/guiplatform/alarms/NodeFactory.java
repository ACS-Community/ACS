/*
 * NodeFactory.java
 *
 * Created on May 25, 2004, 4:35 PM
 */

package cern.laser.guiplatform.alarms;

import java.beans.IntrospectionException;

import org.openide.nodes.BeanNode;
import org.openide.nodes.Children;

import cern.laser.guiplatform.explorer.ACChildrenMap;

/**
 *
 * @author  woloszyn
 */
public class NodeFactory {
    
    /** Creates a new instance of NodeFactory */
    private NodeFactory() {
    }
    
    public static BeanNode createNode( Object bean ) throws IntrospectionException {
        return new AlarmBeanNode( bean, Children.LEAF );
    }
    public static BeanNode createNode( Object bean, Children children ) throws IntrospectionException {
        return new AlarmBeanNode( bean, children );
    }
    public static BeanNode createRootNode( ) throws IntrospectionException {
        //ACChildrenMap map = new ACChildrenMap();
        //Node [] nodes = new Node[5];
        //for (int i=0; i<5; i++){
        //    nodes[i] = NodeFactory.createNode( new AlarmBean(new CommentedAlarm() ) );
        //}
        //map.add( nodes );
        //return new AlarmBeanNode( new Object() , map );
        return new AlarmBeanNode( new Object() , new ACChildrenMap() );
    }
    
}
