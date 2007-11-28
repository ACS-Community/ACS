
/** 
 * @author  Mauricio Araya maray[at]inf.utfsm.cl
 * @author	Jorge Avarias  javarias[at]alumnos.inf.utfsm.cl
 * @version 2
 * @since   
 * 
 * 
 */

package cl.utfsm.acs.ebe;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;


import java.util.TreeMap;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextPane;
import javax.swing.JToolBar;
import javax.swing.JTree;
import javax.swing.ToolTipManager;
import javax.swing.table.DefaultTableModel;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;

import cl.utfsm.acs.ebe.util.EbeOpenFileFilter;
import cl.utfsm.acs.ebe.util.EbeTableModel;
import cl.utfsm.acs.ebe.util.MembersTableModel;
import cl.utfsm.acs.types.ComplexObject;

/**
 * @author Jorge Avarias (javarias[at]inf.utfsm.cl)
 * 
 * This class is teh main Error Browser Editor
 * window GUI. <br />
 * To edit this class you must use edit with Eclipse
 * Visual Editor. It's extremelly insane to use an 
 * text editor to edit this class.
 */
public class ErrorBrowserEditor extends JFrame {

	private JPanel jContentPane = null;
	private JSplitPane jSplitPane = null;
	private JPanel mainPanel = null;
	private JScrollPane jScrollPane = null;
	private static JTextPane textMessagePane = null;
	private JSplitPane jSplitPane1 = null;
	private JTabbedPane jTabbedPane = null;
	private JPanel tableViewPanel = null;
	private JPanel xmlViewPanel = null;
	private JPanel nodesPanel = null;
	private JPanel jPanel = null;
	private JScrollPane jScrollPane2 = null;
	private JTable docTable = null;
	private JScrollPane jScrollPane3 = null;
	private JTable nodeAttributesTable = null;
	private JScrollPane jScrollPane4 = null;
	private JTree nodesTree = null;
	private JScrollPane jScrollPane5 = null;
	private JTable membersTable = null;
	private JPanel jPanel1 = null;
	private JScrollPane jScrollPane1 = null;
	private JTree docsTree = null;
	private JScrollPane jScrollPane6 = null;
	private JEditorPane xmlEditorPane = null;
	private JMenuBar EBEMenuBar = null;
	private JMenu fileMenu = null;
	private JMenuItem exitItem = null;
	private JPanel jPanel2 = null;
	private JButton addButton = null;
	private JButton removeButton = null;
	private JPopupMenu addPopupMenu = null;
	private JMenuItem addFileItem = null;
	private JMenuItem addDirectoryItem = null;
	private JMenuItem addDefaultsItem = null;
	private DefaultMutableTreeNode docNodeSelected = null;
	private EbeDocument docSelected = null;
	private DefaultMutableTreeNode complexNodeSelected = null;
	private ComplexObject complexSelected = null;
	private static EbeDocumentManager manager = null;
	private JPanel nodesEditPanel = null;
	private JButton newNodeButton = null;
	private JButton removeNodeButton = null;
	private JSplitPane jSplitPane2 = null;
	private JToolBar jToolBar = null;
	private JButton editButton = null;
	private JButton saveButton = null;
	private JButton cancelEditButton = null;
	private JLabel jLabel = null;
	private JLabel locationField = null;
	private JButton deleteDocButton = null;
	private JButton newDocButton = null;
	private JPanel jPanel4 = null;
	private JPanel membersEditPanel = null;
	private JButton addMemmberButton = null;
	private JButton removeMemberButton = null;
	private JPopupMenu newNodePopupMenu = null;  //  @jve:decl-index=0:visual-constraint="363,634"
	private JMenuItem newErrorMenuItem = null;
	private JMenuItem newCompletionMenuItem = null;
	private JPopupMenu removePopupMenu = null;  //  @jve:decl-index=0:visual-constraint="134,8"
	private JMenuItem removeSelectedItem = null;
	private JMenuItem cleanAllFilesItem = null;
	public static boolean logInfo=true;
	//  @jve:decl-index=0:visual-constraint=""
	/**
	 * This is the default constructor
	 */
	public ErrorBrowserEditor() {
		super();
		initialize();
		manager = new EbeDocumentManager();
		ArrayList<String> attrs = Member.getClassType().getAttrNames();
		String columnNames[] = new String[attrs.size()];
		attrs.toArray(columnNames);
		DefaultTableModel model = new DefaultTableModel(columnNames,0);
		membersTable.setModel(model);
	}

	/**
	 * This method initializes this class
	 * 
	 * @return void
	 */
	private void initialize() {
		this.setSize(800, 630);
		this.setJMenuBar(getEBEMenuBar());
		this.setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE);
		this.setContentPane(getJContentPane());
		this.setTitle("Error browser and editor");
		this.setJMenuBar(getEBEMenuBar());
	}

	/**
	 * This method initializes jContentPane
	 * 
	 * @return javax.swing.JPanel
	 */
	private JPanel getJContentPane() {
		if(jContentPane == null) {
			jContentPane = new JPanel();
			jContentPane.setLayout(new BorderLayout());
			jContentPane.add(getJSplitPane(), java.awt.BorderLayout.CENTER);
			jContentPane.add(getJToolBar(), java.awt.BorderLayout.NORTH);
		}
		return jContentPane;
	}

	/**
	 * This method initializes jSplitPane	
	 * 	
	 * @return javax.swing.JSplitPane	
	 */
	private JSplitPane getJSplitPane() {
		if (jSplitPane == null) {
			jSplitPane = new JSplitPane();
			jSplitPane.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
			jSplitPane.setTopComponent(getMainPanel());
			jSplitPane.setBottomComponent(getJScrollPane());
			jSplitPane.setDividerLocation(470);
		}
		return jSplitPane;
	}

	/**
	 * This method initializes jPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getMainPanel() {
		if (mainPanel == null) {
			mainPanel = new JPanel();
			mainPanel.setLayout(new BorderLayout());
			mainPanel.add(getJSplitPane1(), java.awt.BorderLayout.CENTER);
		}
		return mainPanel;
	}

	/**
	 * This method initializes jScrollPane	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getJScrollPane() {
		if (jScrollPane == null) {
			jScrollPane = new JScrollPane();
			jScrollPane.setViewportView(getTextMessagePane());
		}
		return jScrollPane;
	}

	/**
	 * This method initializes jTextPane	
	 * 	
	 * @return javax.swing.JTextPane	
	 */
	private static JTextPane getTextMessagePane() {
		if (textMessagePane == null) {
			textMessagePane = new JTextPane();
			textMessagePane.setEditable(false);
		}
		return textMessagePane;
	}

	/**
	 * This method initializes jSplitPane1	
	 * 	
	 * @return javax.swing.JSplitPane	
	 */
	private JSplitPane getJSplitPane1() {
		if (jSplitPane1 == null) {
			jSplitPane1 = new JSplitPane();
			jSplitPane1.setDividerLocation(200);
			jSplitPane1.setLeftComponent(getJPanel1());
			jSplitPane1.setRightComponent(getJTabbedPane());
		}
		return jSplitPane1;
	}

	/**
	 * This method initializes jTabbedPane	
	 * 	
	 * @return javax.swing.JTabbedPane	
	 */
	private JTabbedPane getJTabbedPane() {
		if (jTabbedPane == null) {
			jTabbedPane = new JTabbedPane();
			jTabbedPane.addTab("Table view", null, getTableViewPanel(), null);
			jTabbedPane.addTab("XML View", null, getXmlViewPanel(), null);
		}
		return jTabbedPane;
	}

	/**
	 * This method initializes jPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getTableViewPanel() {
		if (tableViewPanel == null) {
			GridLayout gridLayout = new GridLayout();
			gridLayout.setRows(1);
			tableViewPanel = new JPanel();
			tableViewPanel.setLayout(gridLayout);
			tableViewPanel.add(getNodesPanel(), null);
		}
		return tableViewPanel;
	}

	/**
	 * This method initializes jPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getXmlViewPanel() {
		if (xmlViewPanel == null) {
			xmlViewPanel = new JPanel();
			xmlViewPanel.setLayout(new BorderLayout());
			xmlViewPanel.add(getJScrollPane6(), java.awt.BorderLayout.CENTER);
		}
		return xmlViewPanel;
	}

	/**
	 * This method initializes jPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getNodesPanel() {
		if (nodesPanel == null) {
			nodesPanel = new JPanel();
			nodesPanel.setLayout(new BorderLayout());
			nodesPanel.add(getJPanel(), java.awt.BorderLayout.WEST);
			nodesPanel.add(getJScrollPane2(), java.awt.BorderLayout.NORTH);
			nodesPanel.add(getJSplitPane2(), java.awt.BorderLayout.CENTER);
		}
		return nodesPanel;
	}

	/**
	 * This method initializes jPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel() {
		if (jPanel == null) {
			jPanel = new JPanel();
			jPanel.setLayout(new BorderLayout());
			jPanel.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Errors/Completions", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", java.awt.Font.BOLD, 12), new java.awt.Color(51,51,51)));
			jPanel.setPreferredSize(new java.awt.Dimension(200,300));
			jPanel.add(getJScrollPane4(), java.awt.BorderLayout.CENTER);
			jPanel.add(getNodesEditPanel(), java.awt.BorderLayout.SOUTH);
		}
		return jPanel;
	}

	/**
	 * This method initializes jScrollPane2	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getJScrollPane2() {
		if (jScrollPane2 == null) {
			jScrollPane2 = new JScrollPane();
			jScrollPane2.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Error file attributes", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", java.awt.Font.BOLD, 12), new java.awt.Color(51,51,51)));
			jScrollPane2.setPreferredSize(new java.awt.Dimension(460,175));
			jScrollPane2.setViewportView(getDocTable());
		}
		return jScrollPane2;
	}

	/**
	 * This method initializes jTable	
	 * 	
	 * @return javax.swing.JTable	
	 */
	private JTable getDocTable() {
		if (docTable == null) {
			docTable = new JTable();
			docTable.setModel(new EbeTableModel());
			docTable.setEnabled(true);
		}
		return docTable;
	}

	/**
	 * This method initializes jScrollPane3	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getJScrollPane3() {
		if (jScrollPane3 == null) {
			jScrollPane3 = new JScrollPane();
			jScrollPane3.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Errors/Completions Attributes", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", java.awt.Font.BOLD, 12), new java.awt.Color(51,51,51)));
			jScrollPane3.setPreferredSize(new java.awt.Dimension(100,100));
			jScrollPane3.setViewportView(getNodeAttributesTable());
		}
		return jScrollPane3;
	}

	/**
	 * This method initializes jTable1	
	 * 	
	 * @return javax.swing.JTable	
	 */
	private JTable getNodeAttributesTable() {
		if (nodeAttributesTable == null) {
			nodeAttributesTable = new JTable();
			nodeAttributesTable.setModel(new EbeTableModel());
			nodeAttributesTable.setEnabled(false);
		}
		return nodeAttributesTable;
	}

	/**
	 * This method initializes jScrollPane4	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getJScrollPane4() {
		if (jScrollPane4 == null) {
			jScrollPane4 = new JScrollPane();
			jScrollPane4.setPreferredSize(new java.awt.Dimension(125,363));
			jScrollPane4.setViewportView(getNodesTree());
		}
		return jScrollPane4;
	}

	/**
	 * This method initializes jTree1	
	 * 	
	 * @return javax.swing.JTree	
	 */
	private JTree getNodesTree() {
		if (nodesTree == null) {
			nodesTree = new JTree(new DefaultMutableTreeNode("ROOT"));
			nodesTree.setRootVisible(false);
			nodesTree.setCellRenderer(new ErrorTreeCellRenderer());
			nodesTree.addTreeSelectionListener(new javax.swing.event.TreeSelectionListener() {
				public void valueChanged(javax.swing.event.TreeSelectionEvent e) {
					complexNodeSelected = (DefaultMutableTreeNode)nodesTree.getLastSelectedPathComponent();
					if(complexNodeSelected != null)
						complexSelected = (ComplexObject)complexNodeSelected.getUserObject();
					refreshNodeAttributesTable();
					refreshMembersTable();
				}
			});
		}
		ToolTipManager.sharedInstance().registerComponent(nodesTree);
		return nodesTree;
	}

	/**
	 * This method initializes jScrollPane5	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getJScrollPane5() {
		if (jScrollPane5 == null) {
			jScrollPane5 = new JScrollPane();
			jScrollPane5.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Members", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, null, null));
			jScrollPane5.setPreferredSize(new java.awt.Dimension(10,100));
			jScrollPane5.setViewportView(getMembersTable());
		}
		return jScrollPane5;
	}

	/**
	 * This method initializes jTable2	
	 * 	
	 * @return javax.swing.JTable	
	 */
	private JTable getMembersTable() {
		if (membersTable == null) {
			membersTable = new JTable();
			membersTable.setEnabled(false);
		}
		return membersTable;
	}

	/**
	 * This method initializes jPanel1	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel1() {
		if (jPanel1 == null) {
			jPanel1 = new JPanel();
			jPanel1.setLayout(new BorderLayout());
			jPanel1.add(getJScrollPane1(), java.awt.BorderLayout.CENTER);
			jPanel1.add(getJPanel2(), java.awt.BorderLayout.SOUTH);
		}
		return jPanel1;
	}

	/**
	 * This method initializes jScrollPane1	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getJScrollPane1() {
		if (jScrollPane1 == null) {
			jScrollPane1 = new JScrollPane();
			jScrollPane1.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Error definition files", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", java.awt.Font.BOLD, 12), new java.awt.Color(51,51,51)));
			jScrollPane1.setViewportView(getDocsTree());
		}
		return jScrollPane1;
	}

	/**
	 * This method initializes docsNode	
	 * 	
	 * @return javax.swing.JTree	
	 */
	private JTree getDocsTree() {
		if (docsTree == null) {
			docsTree = new JTree(new DefaultMutableTreeNode("ROOT"));
			docsTree.setRootVisible(false);
			docsTree.setToggleClickCount(1);
			docsTree.addTreeSelectionListener(new javax.swing.event.TreeSelectionListener() {
				public void valueChanged(javax.swing.event.TreeSelectionEvent e) {
					docNodeSelected=(DefaultMutableTreeNode)docsTree.getLastSelectedPathComponent();
					if(docNodeSelected!=null){
						docSelected = (EbeDocument)docNodeSelected.getUserObject();
						locationField.setText(docSelected.getPath());
					}
					else{
						locationField.setText("(none)");
					}
					complexNodeSelected = null;
					complexSelected=null;
					refreshNodesTree();
					refreshDocTable();
					
					try {
						loadXmlView();
					} catch (IOException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
				}
			});
		}
			ToolTipManager.sharedInstance().registerComponent(docsTree);
		return docsTree;
	}

	/**
	 * This method initializes jScrollPane6	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getJScrollPane6() {
		if (jScrollPane6 == null) {
			jScrollPane6 = new JScrollPane();
			jScrollPane6.setViewportView(getXmlEditorPane());
		}
		return jScrollPane6;
	}

	/**
	 * This method initializes jEditorPane	
	 * 	
	 * @return javax.swing.JEditorPane	
	 */
	private JEditorPane getXmlEditorPane() {
		if (xmlEditorPane == null) {
			xmlEditorPane = new JEditorPane();
			xmlEditorPane.setContentType("text/plain");
			xmlEditorPane.setEditable(false);
		}
		return xmlEditorPane;
	}
	
	/**
	 * This method initializes jJMenuBar	
	 * 	
	 * @return javax.swing.JMenuBar	
	 */
	private JMenuBar getEBEMenuBar() {
		if (EBEMenuBar == null) {
			EBEMenuBar = new JMenuBar();
			EBEMenuBar.add(getFileMenu());
		}
		return EBEMenuBar;
	}

	/**
	 * This method initializes jMenu	
	 * 	
	 * @return javax.swing.JMenu	
	 */
	private JMenu getFileMenu() {
		if (fileMenu == null) {
			fileMenu = new JMenu();
			fileMenu.setText("File");
			fileMenu.add(getExitItem());
		}
		return fileMenu;
	}

	/**
	 * This method initializes jMenuItem	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getExitItem() {
		if (exitItem == null) {
			exitItem = new JMenuItem();
			exitItem.setText("Exit");
			exitItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					System.exit(0);
				}
			});
		}
		return exitItem;
	}

	/**
	 * This method initializes jPanel2	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel2() {
		if (jPanel2 == null) {
			GridBagConstraints gridBagConstraints11 = new GridBagConstraints();
			gridBagConstraints11.gridx = 3;
			gridBagConstraints11.gridy = 0;
			GridBagConstraints gridBagConstraints1 = new GridBagConstraints();
			gridBagConstraints1.gridx = 2;
			gridBagConstraints1.fill = java.awt.GridBagConstraints.HORIZONTAL;
			gridBagConstraints1.gridy = 0;
			GridBagConstraints gridBagConstraints = new GridBagConstraints();
			gridBagConstraints.gridy = 0;
			gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
			gridBagConstraints.gridx = 1;
			jPanel2 = new JPanel();
			jPanel2.setLayout(new GridBagLayout());
			jPanel2.add(getAddButton(), gridBagConstraints);
			jPanel2.add(getRemoveButton(), gridBagConstraints1);
		}
		return jPanel2;
	}

	/**
	 * This method initializes jButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getAddButton() {
		if (addButton == null) {
			addButton = new JButton();
			addButton.setText("Load");
			addButton.setFont(new java.awt.Font("Dialog", java.awt.Font.BOLD, 10));
			addButton.addActionListener(new java.awt.event.ActionListener() {   
				public void actionPerformed(java.awt.event.ActionEvent e) {    
					int x = getAddButton().getX();
					int y = getAddButton().getY();
					getAddPopupMenu().show(getAddButton(),x,y);
				}
			
			});
		}
		return addButton;
	}

	/**
	 * This method initializes jButton1	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getRemoveButton() {
		if (removeButton == null) {
			removeButton = new JButton();
			removeButton.setText("Remove");
			removeButton.setFont(new java.awt.Font("Dialog", java.awt.Font.BOLD, 10));
			removeButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					int x = getRemoveButton().getX();
					int y = getRemoveButton().getY();
					getRemovePopupMenu().show(getRemoveButton(),x-40,y);
					
				}
			});
		}
		return removeButton;
	}

	/**
	 * This method initializes jPopupMenu	
	 * 	
	 * @return javax.swing.JPopupMenu	
	 */
	private JPopupMenu getAddPopupMenu() {
		if (addPopupMenu == null) {
			addPopupMenu = new JPopupMenu();
			addPopupMenu.add(getAddFileItem());
			addPopupMenu.add(getAddDirectoryItem());
			addPopupMenu.add(getAddDefaultsItem());
		}
		return addPopupMenu;
	}

	/**
	 * This method initializes jMenuItem	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getAddFileItem() {
		if (addFileItem == null) {
			addFileItem = new JMenuItem();
			addFileItem.setText("File");
			addFileItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					JFileChooser fc = new JFileChooser();
					fc.setFileFilter(new EbeOpenFileFilter());
					int retval = fc.showOpenDialog(ErrorBrowserEditor.this);
					if (retval == JFileChooser.APPROVE_OPTION){
						manager.loadDocument(fc.getSelectedFile().getAbsolutePath());
						refreshDocsTree();
					}
				}
			});
		}
		return addFileItem;
	}

	/**
	 * This method initializes jMenuItem	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getAddDirectoryItem() {
		if (addDirectoryItem == null) {
			addDirectoryItem = new JMenuItem();
			addDirectoryItem.setText("Directory");
			addDirectoryItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					JFileChooser fc = new JFileChooser();
					fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
					int retval = fc.showOpenDialog(ErrorBrowserEditor.this);
					if (retval == JFileChooser.APPROVE_OPTION){
						manager.addDirectory(fc.getSelectedFile().getAbsolutePath());
					/*open a dialog open directory
					manager.addDirectory();
					*/
						refreshDocsTree();
					}
				}
			});
		}
		return addDirectoryItem;
	}

	/**
	 * This method initializes jMenuItem	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getAddDefaultsItem() {
		if (addDefaultsItem == null) {
			addDefaultsItem = new JMenuItem();
			addDefaultsItem.setText("Defaults");
			addDefaultsItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					manager.addDefaults();
					refreshDocsTree();
				}
			});
		}
		return addDefaultsItem;
	}
	
	public void refreshDocsTree(){
		TreeMap<String,EbeDocument> docs = manager.getDocuments();
		DefaultMutableTreeNode root = (DefaultMutableTreeNode)docsTree.getModel().getRoot();
		root.removeAllChildren();
		for(EbeDocument doc: docs.values()){
			root.add(new DefaultMutableTreeNode(doc));
		}
		((DefaultTreeModel)docsTree.getModel()).reload();
	}
	
	public EbeDocument getSelectedDoc(){
		return docSelected;
	}
	
	public Error getSelectedError(){
		return (Error)complexSelected;
	}
	
	private void refreshNodesTree(){
		DefaultTreeModel model = (DefaultTreeModel)nodesTree.getModel();
		DefaultMutableTreeNode root = (DefaultMutableTreeNode) model.getRoot();
		root.removeAllChildren();
		if(docSelected == null){
			model.reload();
			return;
		}
		TreeMap<String,ComplexObject> nodes = docSelected.getNodes();
		for(ComplexObject node: nodes.values()){
			root.add(new DefaultMutableTreeNode(node));
		}
		model.reload();
	}
	
	public void publicRefreshNodeTree(){
		refreshNodesTree();
		complexSelected=null;
		complexNodeSelected=null;
		refreshNodeAttributesTable();
		refreshMembersTable();
		
	}
	
	private void refreshDocTable(){
		if(docSelected==null){
			docTable.setModel(new EbeTableModel());
			return;
		}
		EbeTableModel model = new EbeTableModel(docSelected,this);
		docTable.setModel(model);
		docTable.validate();
		
	}
	
	private void refreshNodeAttributesTable(){
		if(complexSelected==null){
			nodeAttributesTable.setModel(new EbeTableModel());
			return;
		}
		EbeTableModel model = new EbeTableModel(complexSelected,this);
		nodeAttributesTable.setModel(model);
		nodeAttributesTable.validate();
	}
	
	public void refreshMembersTable(){
		/*ArrayList<String> attrs = Member.getClassType().getAttrNames();
		String columnNames[] = new String[attrs.size()];
		attrs.toArray(columnNames);
		DefaultTableModel model = new DefaultTableModel(columnNames,0);*/
		MembersTableModel model;
		if(complexSelected==null || !(complexSelected instanceof Error)){
			model = new MembersTableModel();
			membersTable.setModel(model);
			return;
		}
		
		model = new MembersTableModel(this);
		membersTable.setModel(model);	
		
		/*
		model = ()membersTable.getModel();
		Error error = (Error)complexSelected;
		TreeMap<String,Member> members = error.getMembers();
		for(Member mem: members.values()){
			model.addRow(mem.getAttributes().values().toArray());
		}
		*/
	}
	
	/**
	 * This method initializes jPanel3	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getNodesEditPanel() {
		if (nodesEditPanel == null) {
			nodesEditPanel = new JPanel();
			nodesEditPanel.setVisible(false);
			nodesEditPanel.add(getNewNodeButton(), null);
			nodesEditPanel.add(getRemoveNodeButton(), null);
		}
		return nodesEditPanel;
	}

	/**
	 * This method initializes jButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getNewNodeButton() {
		if (newNodeButton == null) {
			newNodeButton = new JButton();
			newNodeButton.setText("New");
			newNodeButton.setFont(new java.awt.Font("Dialog", java.awt.Font.BOLD, 10));
			newNodeButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					int x = ErrorBrowserEditor.this.getMousePosition().x;
					int y = ErrorBrowserEditor.this.getMousePosition().y;
					getNewNodePopupMenu().show(getNewDocButton(),x,y-50);
				}
			});
		}
		return newNodeButton;
	}

	/**
	 * This method initializes jButton1	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getRemoveNodeButton() {
		if (removeNodeButton == null) {
			removeNodeButton = new JButton();
			removeNodeButton.setText("Remove");
			removeNodeButton.setFont(new java.awt.Font("Dialog", java.awt.Font.BOLD, 10));
			removeNodeButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					if(complexSelected == null)
						return;
					docSelected.getNodes().remove(complexSelected.toString());
					log("\t[Node "+ complexSelected.toString()+" removed]");
					complexNodeSelected=null;
					complexSelected=null;
					refreshNodesTree();
					refreshNodeAttributesTable();
					refreshMembersTable();
				}
			});
		}
		return removeNodeButton;
	}

	/**
	 * This method initializes jSplitPane2	
	 * 	
	 * @return javax.swing.JSplitPane	
	 */
	private JSplitPane getJSplitPane2() {
		if (jSplitPane2 == null) {
			jSplitPane2 = new JSplitPane();
			jSplitPane2.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
			jSplitPane2.setDividerLocation(180);
			jSplitPane2.setOneTouchExpandable(false);
			jSplitPane2.setBottomComponent(getJPanel4());
			jSplitPane2.setTopComponent(getJScrollPane3());
		}
		return jSplitPane2;
	}

	/**
	 * This method initializes jToolBar	
	 * 	
	 * @return javax.swing.JToolBar	
	 */
	private JToolBar getJToolBar() {
		if (jToolBar == null) {
			locationField = new JLabel();
			locationField.setText("(none)");
			locationField.setBackground(Color.WHITE);
			jLabel = new JLabel();
			jLabel.setText("Path:  ");
			jToolBar = new JToolBar();
			jToolBar.setFloatable(false);
			jToolBar.add(getNewDocButton());
			jToolBar.add(getEditButton());
			jToolBar.add(getCancelEditButton());
			jToolBar.add(getSaveButton());
			jToolBar.addSeparator();
			jToolBar.addSeparator();
			jToolBar.addSeparator();
			jToolBar.add(jLabel);
			jToolBar.add(locationField);
			jToolBar.add(getDeleteDocButton());
		}
		return jToolBar;
	}

	/**
	 * This method initializes jButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getEditButton() {
		if (editButton == null) {
			editButton = new JButton();
			editButton.setText("Edit");
			editButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					enterEditMode();
				}
			});
		}
		return editButton;
	}

	/**
	 * This method initializes jButton1	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getSaveButton() {
		if (saveButton == null) {
			saveButton = new JButton();
			saveButton.setText("Save");
			saveButton.setEnabled(false);
			saveButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					exitEditMode();
					try {
						docSelected.save();
					} catch (FileNotFoundException ex) {
						JOptionPane.showMessageDialog(ErrorBrowserEditor.this,
							    "You don't have permission to save the file",
							    "Error",
							    JOptionPane.ERROR_MESSAGE);

					} catch (IOException ex) {
						// TODO Auto-generated catch block
						ex.printStackTrace();
					}
					log("[Document "+docSelected.getPath() +" saved ]");
					docSelected.load();
					refreshDocTable();
					refreshNodesTree();
					refreshNodeAttributesTable();
					refreshMembersTable();
					try {
						loadXmlView();
					} catch (IOException e1) {
						// TODO Auto-generated catch block
						System.out.println("Error");
					}
				}
			});
		}
		return saveButton;
	}

	/**
	 * This method initializes jButton2	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getCancelEditButton() {
		if (cancelEditButton == null) {
			cancelEditButton = new JButton();
			cancelEditButton.setText("Cancel");
			cancelEditButton.setEnabled(false);
			cancelEditButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					exitEditMode();
					try {
						docSelected.load();
					} catch (java.lang.NullPointerException nullException){
						manager.removeDocument(docSelected.getValue());
						docNodeSelected=null;
						docSelected=null;
						complexNodeSelected=null;
						complexSelected=null;
					}
					refreshDocsTree();
					refreshDocTable();
					refreshNodesTree();
					refreshNodeAttributesTable();
					refreshMembersTable();
				}
			});
		}
		return cancelEditButton;
	}

	/**
	 * This method initializes jButton3	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getDeleteDocButton() {
		if (deleteDocButton == null) {
			deleteDocButton = new JButton();
			deleteDocButton.setText("Delete");
			deleteDocButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					if(docSelected==null)
						return;
					int n = JOptionPane.showConfirmDialog(ErrorBrowserEditor.this,
							"Are you sure you want to remove permanently the file from disk?",
							"Nuclear launch detected",JOptionPane.YES_NO_OPTION);
					if(n!=0)
						return;
					manager.deleteDocument(docSelected.toString());
					docSelected=null;
					docNodeSelected=null;
					complexSelected=null;
					complexNodeSelected=null;
					refreshDocsTree();
					refreshDocTable();
					refreshNodesTree();
					refreshNodeAttributesTable();
					refreshMembersTable();
				}
			});
		}
		return deleteDocButton;
	}

	/**
	 * This method initializes jButton4	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getNewDocButton() {
		if (newDocButton == null) {
			newDocButton = new JButton();
			newDocButton.setText("New error file");
			newDocButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					JFileChooser fc = new JFileChooser();
					fc.setFileFilter(new EbeOpenFileFilter());
					fc.setSelectedFile(new File("newErrorDocument.xml"));
					int retval = fc.showDialog(ErrorBrowserEditor.this, "Create new document");
					if(retval == JFileChooser.APPROVE_OPTION){
						if(fc.getSelectedFile().exists()){
							int n = JOptionPane.showConfirmDialog(ErrorBrowserEditor.this,
									"The file already exists, Do you want overwrite the file?",
									"Warning",JOptionPane.YES_NO_OPTION);
							if (n==1)
								return;
						}
						String xmlName=fc.getSelectedFile().getName();
						if(xmlName.indexOf(".xml")==-1)
							fc.setSelectedFile(new File(xmlName + ".xml"));
						xmlName= xmlName.substring(0,xmlName.lastIndexOf('.'));
						manager.newDocument(fc.getSelectedFile().getAbsolutePath(),xmlName);
						refreshDocsTree();
						docSelected=manager.getDocuments().get(xmlName);
						refreshDocTable();
						refreshMembersTable();
						refreshNodeAttributesTable();
						refreshNodesTree();
						enterEditMode();
					}
				}
			});
		}
		return newDocButton;
	}

	/**
	 * This method initializes jPanel4	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel4() {
		if (jPanel4 == null) {
			jPanel4 = new JPanel();
			jPanel4.setLayout(new BorderLayout());
			jPanel4.add(getJScrollPane5(), java.awt.BorderLayout.CENTER);
			jPanel4.add(getMembersEditPanel(), java.awt.BorderLayout.WEST);
		}
		return jPanel4;
	}

	/**
	 * This method initializes jPanel5	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getMembersEditPanel() {
		if (membersEditPanel == null) {
			GridBagConstraints gridBagConstraints3 = new GridBagConstraints();
			gridBagConstraints3.insets = new java.awt.Insets(0,4,0,3);
			gridBagConstraints3.gridy = 1;
			gridBagConstraints3.gridx = 0;
			GridBagConstraints gridBagConstraints2 = new GridBagConstraints();
			gridBagConstraints2.insets = new java.awt.Insets(0,4,0,3);
			gridBagConstraints2.gridy = 0;
			gridBagConstraints2.gridx = 0;
			membersEditPanel = new JPanel();
			membersEditPanel.setLayout(new GridBagLayout());
			membersEditPanel.setVisible(false);
			membersEditPanel.add(getAddMemmberButton(), gridBagConstraints2);
			membersEditPanel.add(getRemoveMemberButton(), gridBagConstraints3);
		}
		return membersEditPanel;
	}

	/**
	 * This method initializes jButton6	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getAddMemmberButton() {
		if (addMemmberButton == null) {
			addMemmberButton = new JButton();
			addMemmberButton.setText("Add");
			addMemmberButton.setFont(new java.awt.Font("Dialog", java.awt.Font.BOLD, 10));
			addMemmberButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					if(!(complexSelected instanceof Error))
					return;
					if (((Error)complexSelected).getMembers().get("NewMember")!=null){
					//TODO: Check this problem at saving
						log("\t[ATENTION: 'NewMember' is not allowed as a valid member name, please change it before adding new members]");

					}
					else{
						Member mem= new Member();
						mem.setValue("NewMember");
						mem.setAttributeValue("name","NewMember");
						((Error)complexSelected).getMembers().put("NewMember",mem);
						log("\t[New member added]");
					}
					refreshMembersTable();
				}
			});
		}
		return addMemmberButton;
	}

	/**
	 * This method initializes jButton7	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getRemoveMemberButton() {
		if (removeMemberButton == null) {
			removeMemberButton = new JButton();
			removeMemberButton.setText("Remove");
			removeMemberButton.setFont(new java.awt.Font("Dialog", java.awt.Font.BOLD, 10));
			removeMemberButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					int selected = membersTable.getSelectedRow();
					if(selected == -1)
						return;
					Error error =(Error)complexSelected;
					TreeMap members = error.getMembers();
					String name = membersTable.getValueAt(selected,0).toString();
					members.remove(name);
					log("\t[Member "+ name +" removed]");
					refreshMembersTable();
				}
			});
		}
		return removeMemberButton;
	}

	/**
	 * This method initializes jPopupMenu	
	 * 	
	 * @return javax.swing.JPopupMenu	
	 */
	private JPopupMenu getNewNodePopupMenu() {
		if (newNodePopupMenu == null) {
			newNodePopupMenu = new JPopupMenu();
			newNodePopupMenu.add(getNewErrorMenuItem());
			newNodePopupMenu.add(getNewCompletionMenuItem());
		}
		return newNodePopupMenu;
	}

	/**
	 * This method initializes jMenuItem	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getNewErrorMenuItem() {
		if (newErrorMenuItem == null) {
			newErrorMenuItem = new JMenuItem();
			newErrorMenuItem.setText("Error");
			newErrorMenuItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {

					if (docSelected.getNode("NewError")!=null){
                                                log("\t[ATENTION: 'NewError' is not allowed as a valid error name, please change it before adding new errors]");
					}
					else{
						ComplexObject newObj = new Error();
						newObj.setValue("NewError");
						newObj.setAttributeValue("name","NewError");
						docSelected.putNode(newObj);
						complexSelected=newObj; 
						log("\t[New Error Added ]");
					}
					refreshNodesTree();
					refreshNodeAttributesTable();
				}
			});
		}
		return newErrorMenuItem;
	}

	/**
	 * This method initializes jMenuItem1	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getNewCompletionMenuItem() {
		if (newCompletionMenuItem == null) {
			newCompletionMenuItem = new JMenuItem();
			newCompletionMenuItem.setText("Completion");
			newCompletionMenuItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
 				        if (docSelected.getNode("NewCompletion")!=null){
                                                log("\t[ATENTION: 'NewCompletion' is not allowed as a valid completion name, please change it before adding new errors]");
                                        }
                                        else{
						ComplexObject newObj = new Completion();
						newObj.setValue("NewCompletion");
						newObj.setAttributeValue("name","NewCompletion");
						docSelected.putNode(newObj);
						complexSelected=newObj;
						log("\t[New Completion Added ]");
					}
					refreshNodesTree();
					refreshNodeAttributesTable();
				}
			});
		}
		return newCompletionMenuItem;
	}

	/**
	 * This method initializes jPopupMenu	
	 * 	
	 * @return javax.swing.JPopupMenu	
	 */
	private JPopupMenu getRemovePopupMenu() {
		if (removePopupMenu == null) {
			removePopupMenu = new JPopupMenu();
			removePopupMenu.add(getRemoveSelectedItem());
			removePopupMenu.add(getCleanAllFilesItem());
		}
		return removePopupMenu;
	}

	/**
	 * This method initializes jMenuItem	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getRemoveSelectedItem() {
		if (removeSelectedItem == null) {
			removeSelectedItem = new JMenuItem();
			removeSelectedItem.setText("Remove selected file");
			removeSelectedItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					if(docSelected==null)
						return;
					manager.removeDocument(docSelected.toString());
					docNodeSelected=null;
					docSelected=null;
					complexNodeSelected=null;
					complexSelected=null;
					refreshDocsTree();
					refreshDocTable();
					refreshMembersTable();
					refreshNodeAttributesTable();
					refreshNodesTree();
				}
			});
		}
		return removeSelectedItem;
	}

	/**
	 * This method initializes jMenuItem1	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getCleanAllFilesItem() {
		if (cleanAllFilesItem == null) {
			cleanAllFilesItem = new JMenuItem();
			cleanAllFilesItem.setText("Clear list");
			cleanAllFilesItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					manager.removeAll();
					docSelected=null;
					docNodeSelected=null;
					refreshDocsTree();
					refreshDocTable();
					refreshNodesTree();
					refreshNodeAttributesTable();
					refreshMembersTable();
				}
			});
		}
		return cleanAllFilesItem;
	}

	public static void main(String[] args){
		ErrorBrowserEditor ebe = new ErrorBrowserEditor();
		ebe.setVisible(true);
	}
	
	private void loadXmlView() throws IOException{
		FileReader inputStream = null;
		String text = "";
		if(docSelected == null){
			xmlEditorPane.setText(text);
			return;
		}
        try {
            inputStream = new FileReader(docSelected.getPath());
            int c;
            //inputStream.reset();
            while ((c = inputStream.read()) != -1) {
                text=text+(char)c;
            }
            xmlEditorPane.setText(text);
        } finally {
            if (inputStream != null) {
                inputStream.close();
            }
        }
	}
	
	private void enterEditMode(){
		boolean exist_flag=true;
		if(docSelected == null)
			return;
		File doc = new File(docSelected.getPath());
		log("[Entering Edit Mode]");
		if(!doc.exists()){
			exist_flag=false;
			try{
			doc.createNewFile();
			} catch (IOException e){
			}
		}
		if(!doc.canWrite()){
			JOptionPane.showMessageDialog(this,"You don't have permission to change this " +
					"document","Warning",JOptionPane.WARNING_MESSAGE);
			log("[Save disabled]");
		}
		else{
			saveButton.setEnabled(true);
		}
		if (!exist_flag)  
			doc.delete();
		editButton.setEnabled(false);
		cancelEditButton.setEnabled(true);
		nodesEditPanel.setVisible(true);
		membersEditPanel.setVisible(true);
		docTable.setEnabled(true);
		nodeAttributesTable.setEnabled(true);
		membersTable.setEnabled(true);
		docsTree.setEnabled(false);
		docsTree.setBackground(Color.LIGHT_GRAY);
		newDocButton.setEnabled(false);
		deleteDocButton.setEnabled(false);
		addButton.setEnabled(false);
		removeButton.setEnabled(false);
	}
	
	private void exitEditMode(){
		editButton.setEnabled(true);
		cancelEditButton.setEnabled(false);
		saveButton.setEnabled(false);
		nodesEditPanel.setVisible(false);
		membersEditPanel.setVisible(false);
		docTable.setEnabled(false);
		nodeAttributesTable.setEnabled(false);
		membersTable.setEnabled(false);
		docsTree.setEnabled(true);
		docsTree.setBackground(Color.WHITE);
		newDocButton.setEnabled(true);
		deleteDocButton.setEnabled(true);
		addButton.setEnabled(true);
		removeButton.setEnabled(true);
		log("[Exiting Edit Mode]");
	}
	
	public static void log(String text){
		if (!logInfo) return;
		getTextMessagePane().setText(getTextMessagePane().getText() +"\n"+text );
		System.out.println(text);
	}
	
	public EbeDocumentManager getManager(){
		return manager;
	}
}


class ErrorTreeCellRenderer extends DefaultTreeCellRenderer {
	
	/*
	 *  ImageIcon tutorialIcon = createImageIcon("images/middle.gif");
     *  if (tutorialIcon != null) {
     *  tree.setCellRenderer(new MyRenderer(tutorialIcon));
     *  
	 */
	private static final ImageIcon errorIcon = createImageIcon("util/images/error.gif");
	private static final ImageIcon completionIcon = createImageIcon("util/images/completion.gif");
	private static final ImageIcon documentIcon = createImageIcon("util/images/document.gif");
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 2773311893986633892L;

    public ErrorTreeCellRenderer() {
        /*Initilize here the icons*/
    }

    public Component getTreeCellRendererComponent(
                        JTree tree,
                        Object value,
                        boolean sel,
                        boolean expanded,
                        boolean leaf,
                        int row,
                        boolean hasFocus) {

        super.getTreeCellRendererComponent(
                        tree, value, sel,
                        expanded, leaf, row,
                        hasFocus);
        if (isError(value)) {
            setIcon(errorIcon);
        } 
        else if (isCompletion(value)){
        	setIcon(completionIcon);
        }
        else if (isDocument(value)){
        	setIcon(documentIcon);
        }
        else {
            setToolTipText("Here comes a Tooltip :)");
        }
        setToolTipText("Here comes a Tooltip :)");

        return this;
    }

    protected boolean isDocument(Object value) {
        DefaultMutableTreeNode node =
                (DefaultMutableTreeNode)value;
        Object nodeInfo =
                (Object)(node.getUserObject());
        
        if (nodeInfo instanceof EbeDocument) {
            return true;
        }

        return false;
    }

    protected boolean isError(Object value) {
        DefaultMutableTreeNode node =
                (DefaultMutableTreeNode)value;
        Object nodeInfo =
                (Object)(node.getUserObject());
        
        if (nodeInfo instanceof Error) {
            return true;
        }

        return false;
    }
    
    protected boolean isCompletion(Object value) {
        DefaultMutableTreeNode node =
                (DefaultMutableTreeNode)value;
        Object nodeInfo =
                (Object)(node.getUserObject());
        
        if (nodeInfo instanceof Completion) {
            return true;
        }

        return false;
    }
    
    /**Returns an ImageIcon, or null if the path was invalid*/
    protected static ImageIcon createImageIcon(String path){
    	java.net.URL imgURL = ErrorBrowserEditor.class.getResource(path);
    	if (imgURL !=null){
    		return new ImageIcon(imgURL);
    	}
    	else{
    		System.err.println("Couldn't find file:" + path);
    		return null;
    	}
    }
    
}
