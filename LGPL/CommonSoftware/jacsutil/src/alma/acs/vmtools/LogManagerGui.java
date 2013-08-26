/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
/*
 * Created on Oct 26, 2005 by mschilli
 */
package alma.acs.vmtools;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.StringTokenizer;
import java.util.Vector;
import java.util.logging.Filter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreePath;



/**
 * Allows a User to configure Loggers at run-time.
 * 
 * For a logger L there are:
 * <ul>  
 * <li> Logger Level: L's level (which can be null, i.e. unset)
 * <li> Active Level: L's level if it is non-null, otherwise the closest ancestor's level 
 * <li> ForceFilter Level: The level of L's forcefilter if L has a forcefilter
 * <li> Effective Level: The stricter one out of {L's active level, L's forcefilter level}
 * </ul> 
 * 
 * @author mschilli
 */
public class LogManagerGui extends JPanel {



	//
	// =============  Stand-Alone Launch==================
	//
	
	public static JFrame openFrame (final LogManagerGui inst) {

		// frame
		final JFrame f = new JFrame(LogManagerGui.class.getName());
		f.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
		f.addWindowListener(new WindowAdapter(){
			@Override
			public void windowClosing(WindowEvent evt){
				int answer = JOptionPane.showConfirmDialog(f, "Really close?", "Close Window", JOptionPane.YES_NO_OPTION);
				if (answer == JOptionPane.YES_OPTION) {
					f.setVisible(false);
					f.dispose();
				}
			}
		});
		f.getContentPane().add(inst);

		f.pack();
		f.setVisible(true);
		
		return f;
	}
	
	
	
	//
	// =============  Instance Implementation ==================
	//

	static final Color BG_NORMAL = Color.white;
	static final Color BG_SELECT = Color.lightGray;

	static final HashMap<Level, Color> level2color = new HashMap<Level, Color>();
	static {
		level2color.put(null, Color.magenta);
		level2color.put(Level.OFF, Color.gray);
		level2color.put(Level.SEVERE, Color.red);
		level2color.put(Level.WARNING, Color.orange);
		level2color.put(Level.INFO, Color.green);
		level2color.put(Level.FINE, Color.green.brighter());
		level2color.put(Level.FINER, Color.green.brighter().brighter());
		level2color.put(Level.FINEST, Color.green.brighter().brighter().brighter());
		level2color.put(Level.CONFIG, Color.pink);
		level2color.put(Level.ALL, Color.blue);
	}

	TreeM model;
	JTree tree;
	LoggerEditor editor;
	QuickLoggerEditor quickEditor;
	JSplitPane splitpane;
	Controls controls;
	
	/**
	 *  
	 */
	public LogManagerGui() {
		super(new BorderLayout());

		model = new TreeM(null); // root will be set in populateModel()
		tree = new JTree(model);
		tree.addTreeSelectionListener(new TreeS());
		tree.setCellRenderer(new TreeR());
		
		quickEditor = new QuickLoggerEditor();
		tree.addMouseListener(new TreeL());
		editor = new LoggerEditor();
		tree.setBackground(Color.white);

		controls = new Controls();
		
		JPanel left = new JPanel(new BorderLayout());
		left.add(new JScrollPane(tree), BorderLayout.CENTER);
		left.add(controls, BorderLayout.SOUTH);

		splitpane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		splitpane.setLeftComponent(left);
		splitpane.setRightComponent(editor);
		add(splitpane);
	}

	protected void message(String msg) {
		String me = LogManagerGui.class.getName();
		me = me.substring(me.lastIndexOf('.')+1);
	   System.out.println(me+": "+msg);
	}

	
	////////////////////////////////////////////////////////
	/// ------------------- API ------------------------ ///
	////////////////////////////////////////////////////////


	public void setFilter (String string) {
		controls.txtNameFilter.setText(string);
	}

	@Override
	public void setVisible (boolean b) {
		if (b == true) {
			populateModel();
		}
		splitpane.setDividerLocation(0.66);
		super.setVisible(b);
	}

	
	public void populateModel () {
		populateModel(new String[]{});
	}
	
	public void populateModel (String[] nameFilter) {

		HashMap<Logger, TreeN> reverse = new HashMap<Logger, TreeN>();

		if (model.root() != null) {
			model.root().removeAllChildren();
		}

		// iterate over all loggers and create nodes
		Enumeration<String> en = LogManager.getLogManager().getLoggerNames();
		while (en.hasMoreElements()) {
			String name = en.nextElement();
			if (passesThroughFilter(nameFilter, name)) {
				Logger elem = LogManager.getLogManager().getLogger(name);
				TreeN node = new TreeN(elem);
				reverse.put(elem, node);
			}
		}

		// iterate over nodes and compose them as tree
		Iterator<Logger> it = reverse.keySet().iterator();
		while (it.hasNext()) {
			Logger elem = it.next();
			TreeN node = (TreeN) reverse.get(elem);
			TreeN pnode = (TreeN) reverse.get(elem.getParent());
			if (pnode == null) {
				// logger has no parent
				model.setRoot(node);
				continue;
			}
			pnode.add(node);
		}

		model.nodeStructureChanged(model.root());
	}

	private boolean passesThroughFilter(String[] nameFilter, String name) {
		for (int i = 0; i < nameFilter.length; i++) {
			if (name.startsWith(nameFilter[i])) {
				return false;
			}
		}
		return true;
	}
	

	////////////////////////////////////////////////////////
	/// ----------------- Internal --------------------- ///
	////////////////////////////////////////////////////////

	Color colorFromLevel (Level x) {
		Color ret = (Color) level2color.get(x);
		if (ret == null) {
			ret = Color.magenta;
			message("no color defined for log-level " + x);
		}
		return ret;
	}

	void editLogger (final Logger x) {
		String level = String.valueOf(x.getLevel());
		if (x.getFilter() instanceof ForceFilter) {
			level += " (filter-level: " + ((ForceFilter) x.getFilter()).level + ")";
		}

		editor.text2.setText(x.toString() //
				+ "\nname=" + x.getName() //
				+ "\nlogger-level=" + level //
				+ "\nactive-level=" + activeLevel(x)
				+ "\neffective-level=" + effectiveLevel(x) //
				+ "\nhandlers=" + Arrays.toString(x.getHandlers()) //
				+ "\nusesParentHandler (i.e. sends output to parent):" + x.getUseParentHandlers() //
				+ "\nparent=" + x.getParent());
		editor.comboBoxActionListener.x = null;
		
		if (x.getLevel() == null) {
			editor.text.setSelectedItem("(inherited)");
		} else {
			editor.text.setSelectedItem(x.getLevel());
		}

		editor.comboBoxActionListener.x = x;
	}

	void quickEditLogger (Point p, final Logger x) {
		quickEditor.use(x);
		quickEditor.show(tree, p.x, p.y);
	}

	/**
	 * @param level Something that can be recognized as (or made) a level
	 */
	Level decodeLevel (Object level) {
		Level toSet;
		if ("(inherited)".equals(level)) {
			toSet = null;
		} else {
			if (LEVELS.contains(level)) {
				toSet = (Level) level;
			} else {
				String s = level.toString();
				try {
					toSet = Level.parse(s);
					/* Level.parse() will create a custom-level if an integer is passed in */
				} catch (Exception exc) {
					message("cannot decode level '" + level + "', using ALL instead");
					toSet = Level.ALL;
				}
			}
		}
		return toSet;
	}
	
	/**
	 * 
	 * @param x
	 */
	void doSetLevel (Logger x, Level toSet) {
		
		if (toSet == null && x.getParent() == null) {
			// leniently ignore attempts to set "(inherited)" on the rootlogger
			return;
		}
		
		message("setting logger '" + x.getName() + "' to level '" + toSet + "'");
		
		//	 PENDING: allow chains of filters instead of overwriting a previous filter here
		if (toSet != null) {
			x.setFilter(new ForceFilter(toSet));	
		} else {
			x.setFilter(null);
			// PENDING: might make forcefilters smart enough to understand inheritance, too  
		}

		x.setLevel(toSet);
		//populateModel();
		tree.repaint();
	}


	////////////////////////////////////////////////////////
	/// ---------------- Inner Types ------------------- ///
	////////////////////////////////////////////////////////


	static final Vector<Level> LEVELS = new Vector<Level>(Arrays.asList(new Level[]{
	//
			Level.OFF,//
			Level.SEVERE,//
			Level.WARNING,//
			Level.INFO, //
			Level.CONFIG,//
			Level.FINE,//
			Level.FINER,//
			Level.FINEST,//
			Level.ALL//
			}));

	
	class QuickLoggerEditor extends JPopupMenu {

		Logger x;
		JMenu handlermenu;

		QuickLoggerEditor() {
			JMenuItem item;

			LoggerLevelAction loggerAction = new LoggerLevelAction();
			item = new JMenuItem("(inherited)");
			item.setActionCommand("(inherited)");
			item.addActionListener(loggerAction);
			super.add(item);
			for (int i = 0; i < LEVELS.size(); i++) {
				String levelName = ((Level) LEVELS.get(i)).getName();
				item = new JMenuItem(levelName);
				item.setActionCommand(levelName);
				item.addActionListener(loggerAction);
				super.add(item);
			}
			
			HandlerLevelAction handlerAction = new HandlerLevelAction();
			handlermenu = new JMenu("Handlers");
			for (int i = 0; i < LEVELS.size(); i++) {
				String levelName = ((Level) LEVELS.get(i)).getName();
				item = new JMenuItem(levelName);
				item.setActionCommand(levelName);
				item.addActionListener(handlerAction);
				handlermenu.add(item);
			}
			super.add(handlermenu);
		}

		void use(Logger x) {
			this.x = x;
			handlermenu.setEnabled(x.getHandlers().length>0);
		}
		
		class LoggerLevelAction implements ActionListener {
			public void actionPerformed (ActionEvent e) {
				if (x != null)
					doSetLevel(x, decodeLevel(e.getActionCommand()));
			}
		}

		class HandlerLevelAction implements ActionListener {
			public void actionPerformed (ActionEvent e) {
				if (x != null)
					for (Handler h : x.getHandlers())
						h.setLevel(decodeLevel(e.getActionCommand()));
			}
		}
		
	}

	class LoggerEditor extends JPanel {

		JTextArea text2;
		JComboBox text;

		ActionLi comboBoxActionListener;

		LoggerEditor() {
			super(new BorderLayout());

			Vector<Object> cmbBoxContents = new Vector<Object>(LEVELS);
			cmbBoxContents.insertElementAt("(inherited)", 0);
			text = new JComboBox(cmbBoxContents);
			this.add(text, BorderLayout.NORTH);

			text2 = new JTextArea();
			this.add(text2, BorderLayout.CENTER);

			comboBoxActionListener = new ActionLi();
			text.addActionListener(comboBoxActionListener);
			text.setEditable(true);
		}

		class ActionLi implements ActionListener {

			Logger x;

			public void actionPerformed (ActionEvent evt) {
				if (x == null) {
					return;
				}

				Object sel = editor.text.getSelectedItem();
				if (sel == null) {
					return;
				}

				doSetLevel(x, decodeLevel(sel));
			}
		}
	}

	class Controls extends JPanel {
		JTextField txtNameFilter;
		Controls() {
			super(new BorderLayout());
			
			this.add(new JLabel("Don't show: "), BorderLayout.WEST);
			this.add(txtNameFilter = new JTextField(), BorderLayout.CENTER);
			
			JButton r = new JButton("Refresh");
			r.addActionListener(new ActionListener() {
				public void actionPerformed (ActionEvent evt) {
					StringTokenizer t = new StringTokenizer(txtNameFilter.getText(), ";, ", false);
					String[] nameFilter = (String[])Collections.list(t).toArray(new String[]{});
					populateModel(nameFilter);
				}
			});
			this.add(r, BorderLayout.EAST);
			
			txtNameFilter.setText("sun., java., javax.");
		}
	}
	

	
	// yields the logger's level, considering inheritance
	Level activeLevel (Logger x) {
		if (x == null) {
			return null;
		}
		if (x.getLevel() == null) {
			return activeLevel(x.getParent());
		}
		return x.getLevel();
	}

	// yields the logger's level, considering inheritance and force-filters
	Level effectiveLevel (Logger x) {
		Level ret = activeLevel(x);
		Filter f = x.getFilter();
		if (f != null && f instanceof ForceFilter) {
			ForceFilter ff = (ForceFilter) f;
			if (ff.level.intValue() > ret.intValue())
				ret = ff.level;
		}
		return ret;
	}

	// There really is a Logger (namely "alma.component") that obviously is reset
	// to Level ALL on each invokation. This filter tries to make up for this annoyance.
	class ForceFilter implements Filter {

		public Level level;
		int threshold;

		ForceFilter(Level level) {
			this.level = level;
			this.threshold = level.intValue();
		}

		public boolean isLoggable (LogRecord record) {
			return threshold <= record.getLevel().intValue();
		}

	}

	class TreeM extends DefaultTreeModel {

		TreeM(TreeN root) {
			super(root);
		}

		TreeN root () {
			return (TreeN) root;
		}

	}

	class TreeN extends DefaultMutableTreeNode {

		TreeN(Logger x) {
			super(x);
		}

		Logger logger () {
			return (Logger) userObject;
		}
	}

	class TreeR implements TreeCellRenderer {

		JLabel cell = new JLabel();
		{
			cell.setBackground(BG_SELECT);
		}

		public Component getTreeCellRendererComponent (JTree tree, Object value, boolean selected, boolean expanded, boolean leaf,
				int row, boolean hasFocus) {
			TreeN node = (TreeN) value;
			Logger logger = node.logger();

			String text = logger.getName();
			if (text.equals("")) {
				text = "<Root Logger>";
			}

			//	if no own level show parent level
			Level activeLevel = activeLevel(logger);
			if (logger.getLevel() != null) {
				text += ": " + logger.getLevel().getName();
			} else {
				text += ": " + activeLevel + " (inherited)";
			}

			Level effectiveLevel = effectiveLevel(logger);
			if (effectiveLevel.intValue() != activeLevel.intValue()) {
				text += ", " + effectiveLevel + " (filtered)";
			}

			if (selected) {
				cell.setOpaque(true);
			} else {
				cell.setOpaque(false);
			}

			Color fg = colorFromLevel(effectiveLevel);
			cell.setForeground(fg);


			cell.setText(text);

			return cell;
		}
	}

	class TreeS implements TreeSelectionListener {

		public void valueChanged (TreeSelectionEvent e) {
			TreeN node = (TreeN) e.getPath().getLastPathComponent();
			editLogger(node.logger());
		}
	}

	class TreeL extends MouseAdapter {

		@Override
		public void mouseClicked (MouseEvent e) {
			if (!SwingUtilities.isRightMouseButton(e))
				return;

			Point p = e.getPoint();
			TreePath tp = tree.getClosestPathForLocation(p.x, p.y);
			tree.setSelectionPath(tp);

			TreeN n = (TreeN) tp.getLastPathComponent();
			quickEditLogger(p, n.logger());
		}
	}

}

//
//
//
//
//
//
//
//
//
//
//
//