package cern.laser.test;

import java.beans.IntrospectionException;
import java.util.Comparator;

import cern.gp.nodes.GPNode;
import cern.gp.nodes.NodeFactory;
import cern.gp.nodes.children.ChildrenListManager;
import cern.gp.nodes.children.NodeList;

public class SimpleChildrenListManager implements ChildrenListManager {
      protected final static int NUMBER_OF_CHILDREN = 3;
      public final static String BEAN_NAME_PREFIX = "bean-";

      public Comparator getComparator() {
        return null; // do not compare
      }

      /**
       * simple implementation of ChildrenListManager.initChildrenList()
       * This method is called by the GP explorer when the parent node in the tree is opened. 
       * It creates NUMBER_OF_CHILDREN children and adds them to nodeList.
       * Each of the children have again a SimpleChildrenListManager objects that creates their own
       * children.
       *
       * @param nodeList the list of nodes to be initialized
       */
      public void initChildrenList(NodeList nodeList) {
        try {
          for (int ix=0; ix<NUMBER_OF_CHILDREN; ix++) {
            GPNode node = NodeFactory.createNode(new SimpleDemoBean(BEAN_NAME_PREFIX + ix), new SimpleChildrenListManager());
            nodeList.addNode(node);
          }
        } catch (IntrospectionException ex) { ex.printStackTrace(); }
      }

    }


