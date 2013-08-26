/*
 * $Id: MultiListExplorer.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.explorer;

import org.openide.explorer.ExplorerManager;
import org.openide.explorer.ExplorerPanel;
import org.openide.explorer.view.ListView;
import org.openide.nodes.Children;
import org.openide.nodes.Node;

import cern.gp.nodes.GPNode;
import cern.gp.util.GPManager;

/**
 * MultiListExplorer is an explorer showing several lists linked together allowing to show
 * several levels of depth at once. The first list shows the children of the root node, the
 * second list shows the children of the selected node in the first list, and so on.
 * <p>
 * The number of lists is parameterizable.
 * </p><p>
 * It is possible to register a <code>java.beans.PropertyChangeListener</code> to be notified
 * of the selection in the different list. The method <code>getExplorerManager(int)</code> 
 * allow to register such a listener on one particular list.
 * </p><p>
 * It is possible to customize the top and bottom component of each list by setting a 
 * <code>ComponentCreator</code> for top or bottom before adding the explorer to the gui.
 * </p>
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public class MultiListExplorer extends ExplorerPanel {

  public static final int DEFAULT_WIDTH = 600;
  public static final int DEFAULT_HEIGHT = 400;
  public static final int DEFAULT_LIST_COUNT = 4;

  private ExplorerPanel[] _explorerPanels;
  private int _listCount = DEFAULT_LIST_COUNT;
  private boolean _twoRows;
  private boolean _automaticSelection = true;

  private ComponentCreator _bottomComponentCreator;
  private ComponentCreator _topComponentCreator;
  
  private SelectionListener _selectionListener;

  //
  // -- CONSTRUCTORS -----------------------------------------------
  //

  public MultiListExplorer() {
    this(null, DEFAULT_LIST_COUNT, DEFAULT_WIDTH, DEFAULT_HEIGHT, false);
  }

  public MultiListExplorer(SelectionListener selectionListener, int listCount) {
    this(selectionListener, listCount, DEFAULT_WIDTH, DEFAULT_HEIGHT, false);
  }

  public MultiListExplorer(SelectionListener selectionListener, int width, int height) {
    this(selectionListener, DEFAULT_LIST_COUNT, width, height, false);
  }

  public MultiListExplorer(SelectionListener selectionListener, int listCount, int width, int height, boolean twoRows) {
    this._listCount = listCount;
    this._twoRows = twoRows;
    if (twoRows && listCount % 2 != 0) throw new IllegalArgumentException("listCount must be a multiple of 2 to be on two rows");  
    this._selectionListener = selectionListener;
    setLayout(new java.awt.GridLayout(1, 1));
    setSize(width, height);
    setPreferredSize(new java.awt.Dimension(width, height));
    // make the explorer non-persistent so that it does not leave bad .wstcref files in system dir
    putClientProperty("PersistenceType", "Never");    
  }

  //
  // -- PUBLIC STATIC METHODS -----------------------------------------------
  //

  //
  // -- PUBLIC METHODS -----------------------------------------------
  //

  public void addNotify() {
    super.addNotify();
    _explorerPanels = new ExplorerPanel[_listCount];
    javax.swing.JComponent explorerComponent = null;
    if (_twoRows) {
      explorerComponent = createExplorerPanelRows(getWidth(), getHeight(), _listCount);
    } else {
      explorerComponent = createExplorerPanelRow(getWidth(), getHeight(), _listCount, 0, null);
    }
    add(explorerComponent, java.awt.BorderLayout.CENTER);
    if (_automaticSelection) {
      selectNthChildren(getExplorerManager(), 0);
    }
  }

  public void removeNotify() {
    _explorerPanels = null;
    super.removeNotify();
  }

  /**
   * set the root node of the hiearachy to be explored
   */
  public void setRootNode(GPNode node) {
    getExplorerManager().setRootContext(node.getPeerNode());
  }

  //
  // -- Getters / Setters -----------------------------------------------
  //

  /**
   * Returns the topComponentCreator that is used to create the top component of 
   * each list in the explorer or null if none is set.
   * @return the current topComponentCreator or null
   */
  public ComponentCreator getTopComponentCreator() {
    return _topComponentCreator;
  }

  /**
   * Sets the topComponentCreator that is used to create the top component of 
   * each list in the explorer. Null can be used to remove an existing creator.
   * The topComponentCreator cannot be changed once the explorer has been included
   * in a gui. It can only be changed after instantiation before adding the Explorer
   * into a container.
   * @param componentCreator the componentCreator to use to create the top components
   */
  public void setTopComponentCreator(ComponentCreator componentCreator) {
    this._topComponentCreator = componentCreator;
  }

  /**
   * Returns the bottomComponentCreator that is used to create the top component of 
   * each list in the explorer or null if none is set.
   * @return the current bottomComponentCreator or null
   */
  public ComponentCreator getBottomComponentCreator() {
    return _bottomComponentCreator;
  }

  /**
   * Sets the bottomComponentCreator that is used to create the top component of 
   * each list in the explorer. Null can be used to remove an existing creator.
   * The bottomComponentCreator cannot be changed once the explorer has been included
   * in a gui. It can only be changed after instantiation before adding the Explorer
   * into a container.
   * @param componentCreator the componentCreator to use to create the bottom components
   */
  public void setBottomComponentCreator(ComponentCreator componentCreator) {
    this._bottomComponentCreator = componentCreator;
  }

  /**
   * Sets the number of lists in the explorer. Cannot be inferior to 2.
   * The number cannot be changed once the explorer has been included in a gui.
   * The number can only be changed after instantiation before adding the Explorer
   * into a container.
   * @param listCount the number of ilst in the explorer
   */
  public void setListCount(int listCount) {
    if (_explorerPanels != null)
      throw new IllegalStateException("addNotify already performed");
    if (listCount < 2)
      throw new IllegalArgumentException("The number of list in the Explorer cannot be < 2");
    this._listCount = listCount;
  }

  /**
   * Returns the number of lists in the explorer.
   * @return the number of lists in the explorer.
   */
  public int getListCount() {
    return _listCount;
  }

  /**
   * Sets if the selection in one list triggers the selection in the subsequent lists.
   * This property is true by default.
   * @param automaticSelection whether the selection in one list cascade to the others
   */
  public void setAutomaticSelection(boolean automaticSelection) {
    this._automaticSelection = automaticSelection;
  }

  /**
   * Returns if the selection in one list triggers the selection in the subsequent lists.
   * @return if the selection in one list triggers the selection in the subsequent lists.
   */
  public boolean getAutomaticSelection() {
    return _automaticSelection;
  }
  
  //
  // -- PROTECTED METHODS -----------------------------------------------
  //

  /**
   * Returns the explorer manager of the list of given index
   * @return the explorer manager of the list of given index
   */
  protected ExplorerManager getExplorerManager(int listIndex) {
    if (_explorerPanels == null)
      throw new IllegalStateException("addNotify not yet performed");
    return _explorerPanels[listIndex].getExplorerManager();
  }

  protected static void selectNthChildren(ExplorerManager explorerManager, int n) {
    Node exploredNode = explorerManager.getExploredContext();
    if (exploredNode == null)
      return;
    Children children = exploredNode.getChildren();
    try {
      if (children == null || children.getNodesCount() == 0) {
        explorerManager.setSelectedNodes(new Node[0]);
      } else {
        if (n >= children.getNodesCount()) {
          explorerManager.setSelectedNodes(new Node[] { children.getNodes()[0] });
        } else {
          explorerManager.setSelectedNodes(new Node[] { children.getNodes()[n] });
        }
      }
    } catch (java.beans.PropertyVetoException e) {
      GPManager.notify(GPManager.WARNING, e);
    }
  }

  //
  // -- PRIVATE METHODS -----------------------------------------------
  //

  /**
   * Creates all ExplorerPanels (one per list) making the MultiListExplorer.
   * This method is recursive and creates list from right to left
   * @param width the remaining available width in pixels
   * @param height the available height in pixels
   * @param listCount the counter giving how many list remain to be created
   * @param prevRightPanel the possibly null previous right panel that was created on the previous call
   * @return the new component containing all created explorers
   */
  private javax.swing.JComponent createExplorerPanelRows(int width, int height, int listCount) {
    // create right Panel
    javax.swing.JComponent bottomComponent = createExplorerPanelRow(width, height / 2, listCount / 2,  listCount / 2, null);
    javax.swing.JComponent topComponent = createExplorerPanelRow(width, height / 2, listCount / 2, 0, _explorerPanels[listCount / 2]);
    javax.swing.JSplitPane splitPane =
      new javax.swing.JSplitPane(javax.swing.JSplitPane.VERTICAL_SPLIT, topComponent, bottomComponent);
    splitPane.setPreferredSize(new java.awt.Dimension(width, height));
    splitPane.setDividerLocation(height / 2);
    splitPane.setResizeWeight(0.5);
    splitPane.setOneTouchExpandable(false);
    splitPane.setContinuousLayout(false);
    splitPane.setDividerSize(3);
    return splitPane;
  }

  /**
   * Creates all ExplorerPanels (one per list) making the MultiListExplorer.
   * This method is recursive and creates list from right to left
   * @param width the remaining available width in pixels
   * @param height the available height in pixels
   * @param listCount the counter giving how many list remain to be created
   * @param prevRightPanel the possibly null previous right panel that was created on the previous call
   * @return the new component containing all created explorers
   */
  private javax.swing.JComponent createExplorerPanelRow(
    int width,
    int height,
    int listCount,
    int preceedingExplorerCount,
    ExplorerPanel prevRightPanel) {
    // create right Panel
    ExplorerPanel rightPanel = createExplorerPanel(preceedingExplorerCount + listCount - 1, prevRightPanel);
    // create left Panel
    double weight = (listCount - 1) / (double) listCount;
    int panelWidth = (int) Math.round(weight * width);
    javax.swing.JComponent leftComponent;
    if (listCount > 2) {
      leftComponent = createExplorerPanelRow(panelWidth, height, listCount - 1, preceedingExplorerCount, rightPanel);
    } else {
      leftComponent = createExplorerPanel(preceedingExplorerCount + listCount - 2, rightPanel);
    }
    javax.swing.JSplitPane splitPane =
      new javax.swing.JSplitPane(javax.swing.JSplitPane.HORIZONTAL_SPLIT, leftComponent, rightPanel);
    splitPane.setPreferredSize(new java.awt.Dimension(width, height));
    splitPane.setDividerLocation(panelWidth);
    splitPane.setResizeWeight(weight);
    splitPane.setOneTouchExpandable(false);
    splitPane.setContinuousLayout(false);
    splitPane.setDividerSize(3);
    return splitPane;
  }

  /**
   * Creates one ExplorerPanel representing one list.
   * @param listIndex the index of the list to create (from 0 to n-1)
   * @param prevRightPanel the possibly null previous right panel displaying the content
   * of the node selected in this list.
   * @return the new ExplorerPanel
   */
  private ExplorerPanel createExplorerPanel(
    int listIndex,
    ExplorerPanel prevRightPanel) {
    if (listIndex > 0) {
      _explorerPanels[listIndex] = new ListExplorerPanel(listIndex, prevRightPanel);
    } else {
      _explorerPanels[listIndex] = new ListExplorerPanel(getExplorerManager(), listIndex, prevRightPanel);
    }
    return _explorerPanels[listIndex];
  }

  //
  // -- INNER CLASSES -----------------------------------------------
  //

  public interface SelectionListener {

    /**
     * Notifies that the selection changed in the list number listIndex. The
     * new selection is given by nodes.
     * @param listIndex the index of the list to create (from 0 to n-1)
     * @param nodes the array representing the new selection
     */
    public void selectionChanged(int listIndex, Node[] nodes);
  }

  public interface ComponentCreator {
    /**
     * Creates s component to be linked with one list of the explorer panel.
     * The component can be position on the top or on the bottom of the list
     * @param listIndex the index of the list to create (from 0 to n-1)
     * @return the new component or null if no component should be used
     */
    public javax.swing.JComponent createComponent(int listIndex);
  }

  private class NextListSelectionListener implements java.beans.PropertyChangeListener {

    private ExplorerManager sourceManager;
    private ExplorerManager targetManager;
    private String name;
    private boolean shouldUpdateSelection;

    public NextListSelectionListener(ExplorerPanel source, ExplorerPanel target) {
      this.sourceManager = source.getExplorerManager();
      this.targetManager = target.getExplorerManager();
      this.name = source.getName();
    }

    public void propertyChange(java.beans.PropertyChangeEvent e) {
      String propName = e.getPropertyName();
      if (propName.equals(ExplorerManager.PROP_SELECTED_NODES)) {
        //
        // selected node changed
        //
        updateTargetPanel();
        if (shouldUpdateSelection) {
          shouldUpdateSelection = false;
          selectNthChildren(sourceManager, 0);
        }
      } else if (propName.equals(ExplorerManager.PROP_EXPLORED_CONTEXT)) {
        //
        // explored node changed
        //
        Object old = e.getOldValue();
        if (old != null && (old instanceof Node)) {
          Node oldNode = (Node) old;
          Node currentNode = sourceManager.getExploredContext();
          //log(name+" OLD context ="+oldNode);
          //log(name+" NEW context ="+currentNode);
          if (!oldNode.equals(currentNode)) {
            shouldUpdateSelection = _automaticSelection;
          }
        }
      } else if (propName.equals(ExplorerManager.PROP_NODE_CHANGE)) {
      } else if (propName.equals(ExplorerManager.PROP_ROOT_CONTEXT)) {
      }
    }

    private void updateTargetPanel() {
      Node[] selectedNodes = sourceManager.getSelectedNodes();
      if ((selectedNodes == null) || (selectedNodes.length == 0)) {
        targetManager.setRootContext(Node.EMPTY);
        return;
      }
      Node selectedNode = selectedNodes[0];
      targetManager.setRootContext(selectedNode);
      if (_automaticSelection) {
        selectNthChildren(targetManager, 0);
      }
    }
  } // end inner class NextListSelectionListener

  private class ListExplorerPanel extends ExplorerPanel {

    private java.beans.PropertyChangeListener nextListSelectionListener;
    private java.beans.PropertyChangeListener listSelectionListener;

    //
    // -- CONSTRUCTORS -----------------------------------------------
    //

    /** Creates a new instance of ListPanel */
    public ListExplorerPanel(int index, ExplorerPanel nextExplorer) {
      initialize(index, nextExplorer);
    }

    /** Creates a new instance of ListPanel */
    public ListExplorerPanel(ExplorerManager explorerManager, int index, ExplorerPanel nextExplorer) {
      super(explorerManager);
      initialize(index, nextExplorer);
    }

    private void initialize(int index, ExplorerPanel nextExplorer) {
      setName("List" + index);
      ListView view = new ListView();
      view.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
      view.setPopupAllowed(true);
      add(view, java.awt.BorderLayout.CENTER);
      if (_topComponentCreator != null) {
        javax.swing.JComponent top = _topComponentCreator.createComponent(index);
        if (top != null) {
          add(top, java.awt.BorderLayout.NORTH);
        }
      }
      if (_bottomComponentCreator != null) {
        javax.swing.JComponent bottom = _bottomComponentCreator.createComponent(index);
        if (bottom != null) {
          add(bottom, java.awt.BorderLayout.SOUTH);
        }
      }
      if (nextExplorer != null) {
        nextListSelectionListener = new NextListSelectionListener(this, nextExplorer);
        this.getExplorerManager().addPropertyChangeListener(nextListSelectionListener);
      }
      if (_selectionListener != null) {
        listSelectionListener = new ListSelectionListener(index);
        this.getExplorerManager().addPropertyChangeListener(listSelectionListener);
      }
    }

    //
    // -- PUBLIC METHODS -----------------------------------------------
    //

    public void addNotify() {
      super.addNotify();
    }

    public void removeNotify() {
      if (nextListSelectionListener != null)
        this.getExplorerManager().removePropertyChangeListener(nextListSelectionListener);
      if (listSelectionListener != null)
        this.getExplorerManager().removePropertyChangeListener(listSelectionListener);
      super.removeNotify();
    }

    //
    // -- PROTECTED METHODS -----------------------------------------------
    //
  } // end inner class ListExplorerPanel

  private class ListSelectionListener implements java.beans.PropertyChangeListener {
    private int index;
    public ListSelectionListener(int index) {
      this.index = index;
    }
    public void propertyChange(java.beans.PropertyChangeEvent e) {
      String propName = e.getPropertyName();
      if (propName.equals(ExplorerManager.PROP_SELECTED_NODES)) {
        Node[] nodes = (Node[]) e.getNewValue();
        _selectionListener.selectionChanged(index, nodes);
      }
    }
  } // end inner class ListSelectionListener

}
