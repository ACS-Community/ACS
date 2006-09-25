/*
 * $Id: ChildrenFactory.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes.children;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

import org.openide.nodes.Children;
import org.openide.nodes.Index;
import org.openide.nodes.Node;

import cern.gp.nodes.GPNode;

/**
 * <i><font size="-1" color="#FF0000">**For internal use only** </font></i>
 * Factory for building the right children for a given NodeManager
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public class ChildrenFactory {

  /** cannot be instantiated */
  private ChildrenFactory() {
  }

  /**
   * Instantiates ths right children for the given NodeManager
   * @param nodeManager the nodeManager controlling the children to build
   * @return the children controlled by the given manager
   */
  public static Children createChildren(ChildrenManager nodeManager) {
    Comparator comparator = nodeManager.getComparator();
    if (nodeManager instanceof ChildrenListManager) {
      if (comparator == null) {
        return new ArrayChildren((ChildrenListManager) nodeManager);
      } else {
        return new SortedArrayChildren((ChildrenListManager) nodeManager, comparator);
      }
    } else if (nodeManager instanceof ChildrenMapManager) {
      if (comparator == null) {
        return new MapChildren((ChildrenMapManager) nodeManager);
      } else {
        return new SortedMapChildren((ChildrenMapManager) nodeManager, comparator);
      }
    } else {
      throw new IllegalArgumentException(
        "The class " + nodeManager.getClass().getName() + " is not a supported manager");
    }
  }
  
  
  private static final Node[] convertGPNodeArrayToNodeArray(GPNode[] nodes) {
    Node[] temp = new Node[nodes.length];
    for (int i = 0; i < temp.length; i++) {
      temp[i] = nodes[i].getPeerNode();
    }
    return temp;
  }

  //
  // -- INNER CLASSES -----------------------------------------------
  //

  /**
   *
   * ArrayChildren
   *
   * @author lmestre, vbaggiol
   */
  private static class ArrayChildren extends Index.ArrayChildren implements NodeList { 

    private ChildrenListManager manager;
    private final Node[] nodeArray = new Node[1];

    //
    // -- CONSTRUCTORS -----------------------------------------------
    //

    public ArrayChildren(ChildrenListManager manager) {
      this.manager = manager;
    }

    //
    // -- PUBLIC METHODS -----------------------------------------------
    //

    //
    // -- implements NodeCollection ------------------------------------------------
    //

    public void refreshOrdering() {
    }

    public ChildrenManager getChildrenManager() {
      return manager;
    }

    public GPNode findChildByName(String name) {
      return (GPNode) super.findChild(name);
    }

    //
    // -- implements NodeList ------------------------------------------------
    //

    public GPNode getParentNode() {
      return (GPNode) getNode();
    }

    public void addNode(GPNode node) {
      nodeArray[0] = node.getPeerNode();
      add(nodeArray);
    }

    public void addNodes(GPNode[] gpNodes) {
      add(convertGPNodeArrayToNodeArray(gpNodes));
    }

    public boolean removeNode(GPNode node) {
      nodeArray[0] = node.getPeerNode();
      return remove(nodeArray);
    }

    public boolean removeNodes(GPNode[] gpNodes) {
      return remove(convertGPNodeArrayToNodeArray(gpNodes));
    }

    public void clear() {
      nodes.clear();
      refresh();
    }

    public Iterator iterator() {
      return nodes.iterator();
    }


    //
    // -- PROTECTED METHODS -----------------------------------------------
    //
    protected void addNotify() {
      super.addNotify();
      manager.initChildrenList(this);
    }

    protected void removeNotify() {
      nodes.clear();
      super.removeNotify();
    }

  } // end inner class ArrayChildren

  /**
   *
   * SortedArrayChildren
   *
   * @author lmestre
   */
  private static class SortedArrayChildren extends Children.SortedArray implements NodeList {

    private ChildrenListManager manager;
    private final Node[] nodeArray = new Node[1];

    //
    // -- CONSTRUCTORS -----------------------------------------------
    //

    public SortedArrayChildren(ChildrenListManager manager, Comparator comparator) {
      super(new ArrayList());
      this.manager = manager;
      setComparator(comparator);
    }

    //
    // -- PUBLIC METHODS -----------------------------------------------
    //

    //
    // -- implements NodeCollection ------------------------------------------------
    //

    public void refreshOrdering() {
      refresh();
    }

    public ChildrenManager getChildrenManager() {
      return manager;
    }

    public GPNode findChildByName(String name) {
      return (GPNode) super.findChild(name);
    }

    //
    // -- implements NodeList ------------------------------------------------
    //

    public GPNode getParentNode() {
      return (GPNode) getNode();
    }

    public void addNode(GPNode node) {
      nodeArray[0] = node.getPeerNode();
      add(nodeArray);
    }

    public void addNodes(GPNode[] gpNodes) {
      add(convertGPNodeArrayToNodeArray(gpNodes));
    }

    public boolean removeNode(GPNode node) {
      nodeArray[0] = node.getPeerNode();
      return remove(nodeArray);
    }

    public boolean removeNodes(GPNode[] gpNodes) {
      return remove(convertGPNodeArrayToNodeArray(gpNodes));
    }

    public void clear() {
      nodes.clear();
      refresh();
    }

    public Iterator iterator() {
      return nodes.iterator();
    }

    //
    // -- PROTECTED METHODS -----------------------------------------------
    //

    protected void addNotify() {
      super.addNotify();
      manager.initChildrenList(this);
    }

    protected void removeNotify() {
      nodes.clear();
      super.removeNotify();
    }
  } // end inner class SortedArrayChildren

  /**
   * 
   * MapChildren
   * 
   * @author lmestre
   */
  private static class MapChildren extends Children.Map implements NodeMap {

    private ChildrenMapManager manager;

    //
    // -- CONSTRUCTORS -----------------------------------------------
    //

    public MapChildren(ChildrenMapManager manager) {
      super(new HashMap());
      this.manager = manager;
    }

    //
    // -- PUBLIC METHODS -----------------------------------------------
    //

    //
    // -- implements NodeCollection ------------------------------------------------
    //

    public void refreshOrdering() {
    }

    public ChildrenManager getChildrenManager() {
      return manager;
    }

    public GPNode findChildByName(String name) {
      return (GPNode) super.findChild(name);
    }

    //
    // -- implements NodeMap ------------------------------------------------
    //

    /* (non-Javadoc)
     * @see cern.gp.nodes.children.NodeMap#addNodes(java.util.Map)
     */
    public void addNodes(java.util.Map nodesMap) {      
      putAll(nodes);
    }

    public GPNode getParentNode() {
      return (GPNode) getNode();
    }

    public final void addNode(Object key, GPNode node) {
      put(key, node.getPeerNode());
    }

    public void addNode(GPNode node) {
      put(node.getName(), node.getPeerNode());
    }

    public void addNodes(GPNode[] gpNodes) {
      java.util.Map tempMap = new java.util.HashMap();
      for (int i = 0; i < gpNodes.length; i++) {
        tempMap.put(gpNodes[i].getName(), gpNodes[i].getPeerNode());
      }
      putAll(tempMap);
    }

    public final GPNode removeNode(Object key) {
      GPNode nodeToRemove = (GPNode) getNode(key);
      if (nodeToRemove == null)
        return null;
      remove(key);
      return nodeToRemove;
    }

    public final void removeNodes(Object[] keys) {
      removeAll(java.util.Arrays.asList(keys));
    }

    public final GPNode getNode(Object key) {
      return (GPNode) nodes.get(key);
    }

    public void clear() {
      nodes.clear();
      refresh();
    }

    public Set keySet() {
      return nodes.keySet();
    }

    //
    // -- PROTECTED METHODS -----------------------------------------------
    //
    protected void addNotify() {
      super.addNotify();
      manager.initChildrenMap(this);
    }

    protected void removeNotify() {
      nodes.clear();
      super.removeNotify();
    }

  } // end inner class MapChildren

  /**
   *
   * SortedMapChildren
   *
   * @author lmestre
   */
  private static class SortedMapChildren extends Children.SortedMap implements NodeMap {

    private ChildrenMapManager manager;

    //
    // -- CONSTRUCTORS -----------------------------------------------
    //

    public SortedMapChildren(ChildrenMapManager manager, Comparator comparator) {
      super(new HashMap());
      this.manager = manager;
      setComparator(comparator);
    }

    //
    // -- PUBLIC METHODS -----------------------------------------------
    //

    //
    // -- implements NodeCollection ------------------------------------------------
    //

    public void refreshOrdering() {
      refresh();
    }

    public ChildrenManager getChildrenManager() {
      return manager;
    }

    public GPNode findChildByName(String name) {
      return (GPNode) super.findChild(name);
    }

    //
    // -- implements NodeMap ------------------------------------------------
    //

    public GPNode getParentNode() {
      return (GPNode) getNode();
    }

    public final void addNode(Object key, GPNode node) {
      put(key, node.getPeerNode());
    }

    /* (non-Javadoc)
     * @see cern.gp.nodes.children.NodeMap#addNodes(java.util.Map)
     */
    public void addNodes(java.util.Map nodesMap) {      
      putAll(nodes);
    }

    public void addNode(GPNode node) {
      put(node.getName(), node.getPeerNode());
    }

    public void addNodes(GPNode[] gpNodes) {
      java.util.Map tempMap = new java.util.HashMap();
      for (int i = 0; i < gpNodes.length; i++) {
        tempMap.put(gpNodes[i].getName(), gpNodes[i].getPeerNode());
      }
      putAll(tempMap);
    }

    public final GPNode removeNode(Object key) {
      GPNode nodeToRemove = (GPNode) getNode(key);
      if (nodeToRemove == null)
        return null;
      remove(key);
      return nodeToRemove;
    }

    public final void removeNodes(Object[] keys) {
      removeAll(java.util.Arrays.asList(keys));
    }

    public final GPNode getNode(Object key) {
      return (GPNode) nodes.get(key);
    }

    public void clear() {
      nodes.clear();
      refresh();
    }
    
    public Set keySet() {
      return nodes.keySet();
    }

    //
    // -- PROTECTED METHODS -----------------------------------------------
    //
    protected void addNotify() {
      super.addNotify();
      manager.initChildrenMap(this);
    }

    protected void removeNotify() {
      nodes.clear();
      super.removeNotify();
    }
  } // end inner class SortedMapChildren

}
