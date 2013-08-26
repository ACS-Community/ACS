/*
 * $Id: TestTreeExplorer.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */

package cern.gp.explorer.test;

import cern.gp.explorer.TreeExplorer;
import cern.gp.explorer.test.helpers.SimpleDemoBean;
import cern.gp.explorer.test.helpers.RecursiveChildrenListManager;
import cern.gp.nodes.NodeFactory;
import cern.gp.windows.WindowUtils;
import java.beans.IntrospectionException;
import cern.gp.nodes.GPNode;

/**
 * A simple test driver for the TreeExplorer
 * @author  Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class TestTreeExplorer {
  
  
  public static TreeExplorer createTreeExplorer() {
    TreeExplorer expl = null;
    try {
      GPNode root = NodeFactory.createNode(new SimpleDemoBean("parent"), new RecursiveChildrenListManager());
      expl = new TreeExplorer();
      expl.setRootNode(root);
    } catch (IntrospectionException ex) { ex.printStackTrace(); }
    return expl;
  }
  
  public static void main(String[] args) {
    WindowUtils.openInMode(createTreeExplorer(), "TestTreeExplorer");
  }
}