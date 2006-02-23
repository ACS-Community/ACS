/*
 * GPMainTests.java
 *
 * Created on July 14, 2003, 10:40 AM
 */

package cern.laser.test;

import java.awt.BorderLayout;
import java.beans.IntrospectionException;

import org.openide.windows.TopComponent;

import cern.gp.explorer.ListExplorer;
import cern.gp.nodes.GPNode;
import cern.gp.nodes.NodeFactory;
import cern.gp.windows.WindowUtils;

/**
 *
 * @author  pawlowsk
 */
public class GPMainTests {
    
    /** Creates a new instance of GPMainTests */
    public GPMainTests() {
    }
    
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) throws IntrospectionException {
        
        // start: Vito's version
        
        SimpleDemoBean[] beans = new SimpleDemoBean[] {
            new SimpleDemoBean("bean1"), new SimpleDemoBean("bean2"), new SimpleDemoBean("bean3"),
        };
        ListExplorer expl = new ListExplorer();
        GPNode[] nodes = NodeFactory.createNode(beans);
        expl.setListNodes(nodes);
        // TreeExplorer expl = new TreeExplorer();
        // GPNode root = NodeFactory.createNode(new SimpleDemoBean("parent"), new SimpleChildrenListManager());
        //expl.setRootNode(root);
                                                                                                                                         
        //WindowUtils.openInMode(expl, "TreeExplorerDemo");
        //expl.requestFocus();
                                                                                                                                         
                                                                                                                                         
        TopComponent top = new TopComponent();
        top.setLayout(new BorderLayout());
        top.add(expl, BorderLayout.CENTER);
        WindowUtils.openInMode(top, "TreeExplorerDemo");
        top.requestFocus();

        
        
        
        
        
        // end: Vito's version
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        /*
        TreeExplorer expl = null;
        GPNode root = NodeFactory.createNode(new SimpleDemoBean("parent"), new SimpleChildrenListManager());
        expl = new TreeExplorer();
        expl.setRootNode(root);
        //WindowUtils.openInMode(expl, "TreeExplorerDemo");
        //expl.requestFocus();
        
        
        TopComponent top = new TopComponent();
        top.setLayout(new BorderLayout());
        top.add(expl, BorderLayout.CENTER);
        

        JButton button = ActionUtils.createJButton(ShowDetailsAction.class);
        top.add(button, BorderLayout.NORTH);

        //JButton button2 = ActionUtils.createJButton(PropertiesAction.class);
        //top.add(button2, BorderLayout.SOUTH);
        
        
        JPanel panel = ActionUtils.createJButtonPanel(new Class[] { PropertiesAction.class, OpenLocalExplorerAction.class });
        top.add(panel, BorderLayout.SOUTH);
    
        
        
        WindowUtils.openInMode(top, "TreeExplorerDemo");
        top.requestFocus();
        */
        
        
    }
} // end GPMainTests
