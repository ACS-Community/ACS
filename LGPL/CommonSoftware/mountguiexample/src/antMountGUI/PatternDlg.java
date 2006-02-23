/*
 * PatternDlg.java
 *
 * Created on January 27, 2004, 5:17 PM
 * 
 * 
 */

package antMountGUI;

/**
 *
 * @author  acaproni
 */

import java.util.Vector;
import javax.swing.JPanel;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.JTextField;
import javax.swing.JSeparator;
import javax.swing.JComboBox;
import javax.swing.border.TitledBorder;
import javax.swing.JScrollPane;
import javax.swing.JPopupMenu;
import javax.swing.JMenuItem;

import java.awt.Point;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionListener;
import java.awt.event.MouseListener;
import java.awt.event.ItemListener;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.MouseEvent;

/**
 * 
 * @author acaproni
 *
 * The dialog allows the user to create a list of strokes
 * The way each stroke is shown depends on the type of the stroke itself
 *
 * The window is divided in three areas:
 *	1. The button to navigate, add and remove the strokes
 *  2. The fields of the current stoke
 *  3. The buttons to terminate (Done, Apply)
 *
 * While the first and the last area are fixed, the second one depends
 * on the type of the stroke
 * 
 * @invariant Strokes!=null
 * @invariant Strokes.size()>=0
 * @invariant currentStroke>=0 && currentStroke<=Strokes.size()
 * 
 * @version 1.0
 */
public class PatternDlg extends javax.swing.JDialog implements ActionListener, ItemListener, MouseListener {
	
	/**
	 * Creates new (dialog) PatternDlg
	 * 
	 * @param parent The parent frame
	 * @param modal If true the dialog is modal
	 */
	public PatternDlg(java.awt.Frame parent, boolean modal) {
		super(parent, modal);
		initComponents();
		if (Strokes.size()>0) {
			currentStroke=1;
		}
		displayStroke();
		ratioBrowseButtons();
	}
	
	/**
	 * @param args the command line arguments
	 */
	public static void main(String args[]) {
		new PatternDlg(new javax.swing.JFrame(), true).show();
	}
    
    /**
     * The vector of patterns (empty at startup)
     */ 
    private Vector Strokes = new Vector();
    
    /**
     * The index of the currently displayed stroke (0 doesn't exist)
     * It is always between 1 and Strokes.size()
     */
    private int currentStroke = 0 ;
	
	/**
	 * The add button
	 */
    private JButton addB = new JButton();
    
    /** 
     * The begin button
     */
    private JButton beginB = new JButton();
    
    /** The panel that contains all the buttons
     * to navigate through the strokes
     */
    private JPanel browsePanel = new JPanel();
    
    /**
     * Support panel for the buttons to browse the strokes
     */
	private JPanel jPanel1 = new JPanel();
	
	/**
	 * The separator (just a line) between the buttons
	 * to navigate the strokes and the rest of the panel
	 */
	private JSeparator jSeparator1 = new JSeparator();
	
	/**
	 * The less button (move to the previous stroke)
	 */
	private JButton lessB = new JButton();
	
	/**
	 * The button to move to the last stroke
	 */
	private JButton endB = new JButton();
	
	/**
	 * The button to move to the next stroke
	 */
	private JButton moreB = new JButton();
	
	/**
	 * The button to delete the current stroke
	 */
	private JButton removeB = new JButton();
	
	/**
	 * The text field to insert the number of the stroke to move to
	 */
	private JTextField strokesTF = new JTextField();
 
	// GUI variables declaration for the termination side
	
	/**
	 * Another (support) panel for the apply and done buttons
	 */
	private JPanel jPanel3 = new JPanel();
	
	/**
	 * A separator (just a line) upon the Apply and Done buttons
	 */
	private JSeparator jSeparator2 = new JSeparator();
	
	/** 
	 * A panel for the buttons
	 */
	private JPanel buttonPanel = new JPanel();
	
	/**
	 * The apply button
	 */
    private JButton applyButton = new JButton();
    
    /**
     * The Done button
     */
    private JButton doneButton = new JButton();

	// The panel to show the stroke
	// It is built depending on the type of the stroke
	private JPanel strokePanel = new JPanel();
	
	/**
	 * The mode (combo box)
	 */
	private JComboBox modeCB = new JComboBox();
	
	/**
	 * The mode (label)
	 */
    private JLabel modeL = new JLabel();
    
    /**
     * The label for the time
     */
    private JLabel timeL = new JLabel();
    
    /** 
     * The text field for the time
     */
    private JTextField timeTF = new JTextField();
    
    /** 
     * The label for orientation
     */
    private JLabel orientationL = new JLabel();
    
    /**
     * The text field for orientation
     */
    private JTextField orientationTF = new JTextField();
    
    /**
     * Tha label for xArray[0]
     */
    private JLabel X0L = new JLabel();
    
    /**
     * The text field for xArray[0]
     */
    private JTextField X0TF = new JTextField();
    
    /**
     * The label for xArray[1]
     */
    private JLabel X1L = new JLabel();
    
    /**
     * The text field for xArray[1]
     */
    private JTextField X1TF = new JTextField();
     
     /**
      * The label for yArray[0]
      */
    private JLabel Y0L = new JLabel();
    
    /**
     * The text field for yArra[0]
     */
    private JTextField Y0TF = new JTextField();
    
    /** 
     * The label for yArray[1]
     */
    private JLabel Y1L = new JLabel();
    
    /**
     * The text field for yArray[1] 
     */
    private JTextField Y1TF = new JTextField();

	// GUI variables declaration for the ArcStroke panel
	// It shows the same fields of the linear stroke plus the following
    
    /**
     * The label for the longitude center
     */
	private JLabel longCenterL = new JLabel();
	
	/**
	 * The label for the latitude center
	 */
	private JLabel latCenterL = new JLabel();
	
	/**
	 * The label for the polar orientation
	 */
	private JLabel polarOrientL = new JLabel();
	
	/** 
	 * The text field for the longitude center
	 */
	private JTextField longCenterTF = new JTextField();
	
	/**
	 * The text field for the latitude center
	 */
	private JTextField latCenterTF = new JTextField();
	
	/**
	 * The text field for the polar orientation
	 */
	private JTextField polarOrientTF = new JTextField();


	// GUI variables declaration for the CurveStroke panel
	// Apart of time and orientation commmon to the other strokes,
	// it shows its own field
	
	/**
	 * The table model for the table of arrays (it stores the values
	 * inserted by the user)
	 */
	private DefaultTableModel tableModel = new DefaultTableModel(
			new String[][] {{"0.0", "0.0", "0.0"}, {"0.0", "0.0" , "0.0"}}, new String[] {"X (ddmmss.d)", "Y (ddmmss.d/s)", "time rel. (s)"} 
			);
	
	/**
	 * The table that displays the arrays
	 */
	private JTable arrayT = new JTable(tableModel);
	
	/**
	 * The scroll bar of the table of arrays
	 */
	private JScrollPane scrollPane = new JScrollPane(arrayT);
	
	/**
	 * The popup menu that apperas when the user press the right mouse
	 * button over tha table
	 */
	private JPopupMenu arrayPM = new JPopupMenu();
	
	
	/**
	 * Remember the table row over which the user pressed a mouse button
	 */
	private int rowPressed=-1; // Mouse pressed outside a table row
	
	/**
	 * Remember the table col over which the user pressed a mouse button
	 */
	private int colPressed=-1; // Mouse pressed outside a table column
	
	/**
	 * The popup menu item to add a row before that above the mouse pointer
	 */
	private JMenuItem addBeforeMI = new JMenuItem("Add row before");
	
	/**
	 * The popup menu item to add a row after that above the mouse pointer
	 */
	private JMenuItem addAfterMI = new JMenuItem("Add row after");
	
	/**
	 * The popup menu item to append a new row
	 */
	private JMenuItem addMI = new JMenuItem("Add row");
	
	/**
	 * The popup menu item to move up the current line 
	 */
	private JMenuItem moveUpMI =new JMenuItem("Move Up");
	
	/**
	 * The popup menu item to move down the current line
	 */
	private JMenuItem moveDownMI = new JMenuItem("Move Down");
	
	/**
	 * The popup menu item to edit the cell under the mouse pointer
	 */
	private JMenuItem editMI = new JMenuItem("Edit cell");
    
    /**
     * Receive and dispatches the ActionEvents generated by the user 
     *
     * @param event The event
     * 
     */
    public void actionPerformed(ActionEvent event) {
    	/** @assert event!=null*/
    	/** @assert event.getActionCommand()!=null */
    	/** @assert event.getSource()!=null */
    	checkTableEditing();
    	if (event.getSource()==this.strokesTF) {
    		// Check if the source of the event is the JTextField 
    		// to browse the strokes
    		onStrokesTF(); 
    	} else if (event.getSource()==this.addAfterMI) {
    		// The add after popup menu item
    		onAddAfterTableRow();
    	} else if (event.getSource()==this.addBeforeMI) {
    		// The add before popup menu item
    		onAddBeforeTableRow();
    	} else if (event.getSource()==this.addMI) {
    		// The add popup menu item
    		onAddTableRow();
    	} else if (event.getSource()==this.moveUpMI) {
    		// The move up popup menu item
    		onMoveUpTableRow();
    	} else if (event.getSource()==this.editMI) {
    		// The edit popup menu item
    		System.out.println("editMI ["+rowPressed+","+colPressed+"]");
    		if (rowPressed!=1 && colPressed!=1) {
    			/** @ assert arrayT!=null */
    			arrayT.editCellAt(rowPressed, colPressed);
    		}
    	} else if (event.getSource()==this.moveDownMI) {
    		// The move down popup menu item
    		onMoveDownTableRow();
    	} else if (event.getActionCommand().compareToIgnoreCase("|<")==0) {
    		// |< button pressed
    		this.onBeginButton();
    	} else if (event.getActionCommand().compareToIgnoreCase("<")==0) {
    		// < button pressed
    		this.onLessButton(); 
    	} else if (event.getActionCommand().compareToIgnoreCase("Add")==0) {
    		// Add button pressed
    		this.onAddButton(); 
    	} else if (event.getActionCommand().compareToIgnoreCase("Remove")==0) {
    		// Remove button pressed
    		this.onRemoveButton(); 
    	} else if (event.getActionCommand().compareToIgnoreCase(">")==0) {
    		// > button pressed
    		this.onMoreButton(); 
    	} else if (event.getActionCommand().compareToIgnoreCase(">|")==0) {
    		// >| button pressed
    		this.onEndButton(); 
    	} else if (event.getActionCommand().compareToIgnoreCase("Done")==0) {
    		// Done button pressed
    		this.onDone(); 
    	} else {
    		// Error!!!
    		System.err.println("Unrecognized action: "+event.getActionCommand());
    	}
    }

    /**
     * Manages the event generated when the user changes the type of the stroke
     * It build the panel for the right stroke panel
     * 
     * This method is executed when the item changed (the change may be generated
     * either by the user or the process)
     */
    public void itemStateChanged(ItemEvent event) {
    	/** @assert event!=null */
    	buildStrokePanel((String)event.getItem());
    	pack();
    }
    
    /**
     * The user pressed the mouse button
     * 
     *@param e The event
     */
    public void mousePressed(MouseEvent e) {
    	checkPopup(e); 
    }
    
    /**
     * The user released the mouse button
     * 
     * @param The event
     */
    public void mouseReleased(MouseEvent e) { 
    	checkPopup(e); 
    }
    
    /** 
     * The user clicked a mouse button
     * 
     * @param The event
     */
    public void mouseClicked(MouseEvent e) { 
    	checkPopup(e);
    }
    
    /**
     * The mouse enters a component
     * 
     * @param The event
     */
    public void mouseEntered(MouseEvent e) {}
    
    /**
     * The mouse exited a component
     * 
     * @param The event
     */
    public void mouseExited(MouseEvent e) {}
    
    /**
     * Check if the user was editing a cell of the table
     * and eventually terminate the editing (acquiring the data)
     */
    public void checkTableEditing() {
    	/** @assert arrayT!=null */
    	if (arrayT.isEditing()) {
    		System.out.println("The user was editing a cell");
    		int row=arrayT.getEditingRow();
    		int col=arrayT.getEditingColumn();
    		TableCellEditor editor=arrayT.getCellEditor(row, col);
    		/** @assert editor!=null */
    		if (editor!=null) {
    			editor.stopCellEditing();
    		}
    	}
    }
    
    /**
     *  Display the current stroke
     * 
     */
    private void displayStroke() {
    	// NOTE: when the ComboBox setSelected method is called, the combo box
    	// generates an itemChanged event and the correct panel for the type 
    	// of the stroke will be shown
        if (currentStroke==0) {
			// Display an empty stroke (better for the user to see something!)
            enableWidget(false);
        } else {
            // Get the record from the vector and fill the GUI
            enableWidget(true);
            Object stroke = Strokes.get(currentStroke-1);
            /** @assert stroke!=null */
            System.out.println("Obj is:"+stroke.getClass().toString());
            if (stroke.getClass().toString().indexOf("Linear")!=-1) {
            	System.out.println("Linear");
            	modeCB.setSelectedItem("Linear");
            	LinearStroke lStroke=(LinearStroke)stroke;
            	X0TF.setText(Double.toString(lStroke.getX0()));
            	X1TF.setText(Double.toString(lStroke.getX1()));
            	Y0TF.setText(Double.toString(lStroke.getY0()));
            	Y1TF.setText(Double.toString(lStroke.getY1()));
            	timeTF.setText(Double.toString(lStroke.getTime()));
            	orientationTF.setText(Double.toString(lStroke.getOrientation()));
            }else if (stroke.getClass().toString().indexOf("Arc")!=-1) {
            	System.out.println("Arc");
            	modeCB.setSelectedItem("Arc");
            	ArcStroke aStroke=(ArcStroke)stroke;
            	X0TF.setText(Double.toString(aStroke.getX0()));
            	X1TF.setText(Double.toString(aStroke.getX1()));
            	Y0TF.setText(Double.toString(aStroke.getY0()));
            	Y1TF.setText(Double.toString(aStroke.getY1()));
            	longCenterTF.setText(Double.toString(aStroke.getLongCenter()));
            	latCenterTF.setText(Double.toString(aStroke.getLatCenter()));
            	polarOrientTF.setText(Double.toString(aStroke.getPolarOrientation()));
            	timeTF.setText(Double.toString(aStroke.getTime()));
            	orientationTF.setText(Double.toString(aStroke.getOrientation()));
            } else if (stroke.getClass().toString().indexOf("Curve")!=-1) {
            	System.out.println("Curve");
            	modeCB.setSelectedItem("Curve");
            	CurveStroke cStroke=(CurveStroke)stroke;
            	/** @assert arrayT!=null */
            	DefaultTableModel model=(DefaultTableModel)arrayT.getModel();
            	/** @assert model!=null */
            	// Clear the old rows of the table
            	for (int t=0; t<model.getRowCount(); t++) {
            		model.removeRow(0);
            	}
            	double[][] vals = cStroke.getArray();
            	for (int t=0; t<vals.length; t++) {
            		// Build the row
            		String[] rowVals = new String[3];
            		rowVals[0]=Double.toString(vals[t][0]);
            		rowVals[1]=Double.toString(vals[t][1]);
            		rowVals[2]=Double.toString(vals[t][2]);
            		// Add the row to the array
            		model.addRow(rowVals);
            	}
            	
            } else {
            	System.out.println("Unrecognized stroke type!!!");
            	return;
            }
        }
    }
    
    /**
     * Enable or disable the widgets of the window
     * It manages the fields but not the browse buttons
     *
     * @param b The widgets are enabled if the parameter is true 
     */
    private void enableWidget(boolean b) {
        modeCB.setEnabled(b);
        modeL.setEnabled(b);
        X0TF.setEnabled(b);
        X0L.setEnabled(b);
        orientationTF.setEnabled(b);
        orientationL.setEnabled(b);
        X1TF.setEnabled(b);
        X1L.setEnabled(b);
        Y0TF.setEnabled(b);
        Y0L.setEnabled(b);
        Y1TF.setEnabled(b);
        Y1L.setEnabled(b);
        timeTF.setEnabled(b);
        timeL.setEnabled(b);
        orientationTF.setEnabled(b);
        orientationL.setEnabled(b);
		strokePanel.setEnabled(b);
    }

	/** 
	 * Build the panel with the right fields for each kind of stroke
	 * (it selects the items but doesn't fill their fields)
	 * 
	 * @param strokeType The string that describes the type of the stroke
	 * 
	 * @pre strokeType!=null
	 * @pre strokeType.compareToIgnoreCase("Linear")==0 || strokeType.compareToIgnoreCase("Arc")==0 || strokeType.compareToIgnoreCase("Curve")==0
	 *
	 */
	private void buildStrokePanel(String strokeType) {
		strokePanel.removeAll();
		arrayT.removeMouseListener(this);
		if (arrayPM!=null) {
			arrayPM.show(false);
			arrayPM=null;
		}
		if (strokeType.compareToIgnoreCase("Linear")==0) {
	        strokePanel.setLayout(new java.awt.GridLayout(7, 2));
	        strokePanel.add(modeL);
	        strokePanel.add(modeCB);
	        strokePanel.add(X0L);
	        strokePanel.add(X0TF);
	        strokePanel.add(X1L);
	        strokePanel.add(X1TF);
	        strokePanel.add(Y0L);
	        strokePanel.add(Y0TF);
	        strokePanel.add(Y1L);
	        strokePanel.add(Y1TF);
	        strokePanel.add(timeL);
	        strokePanel.add(timeTF);
	        strokePanel.add(orientationL);
	        strokePanel.add(orientationTF);
		} else if (strokeType.compareToIgnoreCase("Arc")==0) {
			strokePanel.setLayout(new java.awt.GridLayout(10, 2));
	        strokePanel.add(modeL);
	        strokePanel.add(modeCB);
	        strokePanel.add(X0L);
	        strokePanel.add(X0TF);
	        strokePanel.add(X1L);
	        strokePanel.add(X1TF);
	        strokePanel.add(Y0L);
	        strokePanel.add(Y0TF);
	        strokePanel.add(Y1L);
	        strokePanel.add(Y1TF);
	        strokePanel.add(timeL);
	        strokePanel.add(timeTF);
	        strokePanel.add(orientationL);
	        strokePanel.add(orientationTF);
			strokePanel.add(longCenterL);
	        strokePanel.add(longCenterTF);
			strokePanel.add(latCenterL);
	        strokePanel.add(latCenterTF);
			strokePanel.add(polarOrientL);
	        strokePanel.add(polarOrientTF);
		} else if (strokeType.compareToIgnoreCase("Curve")==0) {
			strokePanel.setLayout(new java.awt.BorderLayout());
			JPanel upperPanel = new JPanel();
			upperPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
	        upperPanel.add(modeL);
	        upperPanel.add(modeCB);
	        strokePanel.add(upperPanel, BorderLayout.NORTH);
	        
	        // The popup menu
	        arrayPM=new JPopupMenu("Array edit");
	        arrayPM.add(addBeforeMI);
	        arrayPM.add(addAfterMI);
	        arrayPM.add(addMI);
	        arrayPM.addSeparator();
	        arrayPM.add(moveUpMI);
	        arrayPM.add(moveDownMI);
	        arrayPM.addSeparator();
	        arrayPM.add(editMI);
	        arrayT.addMouseListener(this);
	        
	        JPanel centralPanel = new JPanel();
	        centralPanel.setLayout(new BorderLayout());
	        centralPanel.setBorder(new TitledBorder("Array"));
	        centralPanel.add(scrollPane, BorderLayout.CENTER);
	        strokePanel.add(centralPanel, BorderLayout.CENTER);
	        scrollPane.addMouseListener(this);
	        
	        JPanel lowerPanel = new JPanel();
	        lowerPanel.setLayout(new java.awt.GridLayout(2, 2));
			lowerPanel.add(timeL);
	        lowerPanel.add(timeTF);
	        lowerPanel.add(orientationL);
	        lowerPanel.add(orientationTF);
	        strokePanel.add(lowerPanel, java.awt.BorderLayout.SOUTH);
		} else {
			System.err.println("Unrecognized stroke type: "+strokeType);
		}
	}
    
    /**
     * Enable or disable the navigation buttons depending on the available strokes 
     * and the displayed stroke
     */
    private void ratioBrowseButtons() {
        // Update the text field
        String num="";
        if (currentStroke<10) num=num+"0";
        num=num+currentStroke+'/';
        if (Strokes.size()<10) num=num+"0";
        num=num+Strokes.size();
        strokesTF.setText(num);
        strokesTF.setEnabled((Strokes.size()>1));
        // Disable Everything then enable only the needed buttons
        beginB.setEnabled(false);
        lessB.setEnabled(false);
        removeB.setEnabled((Strokes.size()>0));
        moreB.setEnabled(false);
        endB.setEnabled(false);
        if (currentStroke>1) {
            // Enable the button to view the previous items
            lessB.setEnabled(true);
            beginB.setEnabled(true);
        }
        if (currentStroke<Strokes.size()) {
            moreB.setEnabled(true);
            endB.setEnabled(true);
        }
    }
    
    /** 
     * Save the currently displayed stroke in the Stroke vector 
     * The type of the stroke created and stored into the vector
     * depends from the tipe chosen by the user in the combobox
     * 
     * @post Strokes.size() == ($pre(int,Strokes.size()))+1
     **/
    private void saveCurrentStroke() {
        // Read the fields and build a new Stroke object
        double time = Double.parseDouble(timeTF.getText());
        double orient = Double.parseDouble(orientationTF.getText());
        
        String mode = (String)modeCB.getSelectedItem();
        if (mode.compareToIgnoreCase("Linear")==0) {
        	double xA0=Double.parseDouble(X0TF.getText());
        	double xA1=Double.parseDouble(X1TF.getText());
        	double yA0=Double.parseDouble(Y0TF.getText());
        	double yA1=Double.parseDouble(Y1TF.getText());
        	LinearStroke lStroke = new LinearStroke(time, orient, xA0, yA0, xA1, yA1);
        	Strokes.setElementAt(lStroke, currentStroke-1);
        } else if (mode.compareToIgnoreCase("Arc")==0) {
        	double xA0=Double.parseDouble(X0TF.getText());
        	double xA1=Double.parseDouble(X1TF.getText());
        	double yA0=Double.parseDouble(Y0TF.getText());
        	double yA1=Double.parseDouble(Y1TF.getText());
        	double longC = Double.parseDouble(longCenterTF.getText());
        	double latC = Double.parseDouble(latCenterTF.getText());
        	double polar = Double.parseDouble(polarOrientTF.getText());
        	ArcStroke aStroke = new ArcStroke(time, orient, xA0, yA0, xA1, yA1, longC, latC, polar);
        	Strokes.setElementAt(aStroke, currentStroke-1);
        } else if (mode.compareToIgnoreCase("Curve")==0) {
        	DefaultTableModel model =(DefaultTableModel)(arrayT.getModel());
        	/** @assert model!=null */
        	Vector v = model.getDataVector();
        	/** @assert v!=null */
        	double d[][]=new double[v.size()][3];
        	for (int t=0; t<v.size(); t++) {
        		Vector row = (Vector)v.elementAt(t);
        		/** @assert row!=null */
        		d[t][0]=Double.parseDouble((String)row.elementAt(0));
        		d[t][1]=Double.parseDouble((String)row.elementAt(1));
        		d[t][2]=Double.parseDouble((String)row.elementAt(2));
        	}
        	CurveStroke cStroke = new CurveStroke(time, orient, d);
        	Strokes.setElementAt(cStroke, currentStroke-1);
        } else {
        	System.err.println("Unrecognized stroke type!");
        	return;
        }
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {
 
		// Main panel
		setTitle("Pattern");
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                closeDialog(evt);
            }
        });
        getContentPane().setLayout(new java.awt.BorderLayout(0, 5));

        // Browse panel
		browsePanel.setLayout(new java.awt.BorderLayout());

        beginB.setText("|<");
        beginB.addActionListener(this);
        jPanel1.add(beginB);

        lessB.setText("<");
        lessB.addActionListener(this);
        jPanel1.add(lessB);

        addB.setText("Add");
        addB.addActionListener(this);
        jPanel1.add(addB);

        strokesTF.setColumns(5);
        strokesTF.setHorizontalAlignment(javax.swing.JTextField.CENTER);
        strokesTF.setText("00/00");
        strokesTF.addActionListener(this);
        strokesTF.addFocusListener(new java.awt.event.FocusAdapter() {
            public void focusGained(java.awt.event.FocusEvent evt) {
                onStrokesTFFocus();
            }
        });

        jPanel1.add(strokesTF);

        removeB.setText("Remove");
        removeB.addActionListener(this);
        jPanel1.add(removeB);

        moreB.setText(">");
        moreB.addActionListener(this);
        jPanel1.add(moreB);

        endB.setText(">|");
        endB.addActionListener(this);
        jPanel1.add(endB);

        browsePanel.add(jPanel1, java.awt.BorderLayout.CENTER);
        browsePanel.add(jSeparator1, java.awt.BorderLayout.SOUTH);
        getContentPane().add(browsePanel, java.awt.BorderLayout.NORTH);


		// The strokePanel
		modeCB.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Linear", "Curve", "Arc" }));
		modeCB.setSelectedIndex(0);
		modeCB.addItemListener(this);
		buildStrokePanel((String)modeCB.getItemAt(0));

		// Fill the field of the stroke panel
        modeL.setText("Mode");
        X0L.setText("xArray[0] (ddmmss.d)");
        X0TF.setText("000000.0");
        X1L.setText("xArray[1]  (ddmmss.d/s)");
        X1TF.setText("000000.0");
        Y0L.setText("yArray[0] (ddmmss.d)");
        Y0TF.setText("000000.0");
        Y1L.setText("yArray[1] (ddmmss.d/s)");
        Y1TF.setText("000000.0");
        timeL.setText("Time relative (s)");
        timeTF.setText("0");
        orientationL.setText("Descr. orientation (ddmmss.d)");
        orientationTF.setText("000000.0");
		strokePanel.setVisible(true);

		longCenterL.setText("long Center (ddmmss.d)");
		longCenterTF.setText("000000.0");
        latCenterL.setText("lat Center (ddmmss.d)");
		latCenterTF.setText("000000.0");
		polarOrientL.setText("polar Orient. (ddmmss.d)");
		polarOrientTF.setText("000000.0");

		getContentPane().add(strokePanel, java.awt.BorderLayout.CENTER);
		
		// The panel with the termination buttons
        buttonPanel.setLayout(new java.awt.BorderLayout());
        buttonPanel.add(jSeparator2, java.awt.BorderLayout.NORTH);

        applyButton.setText("Apply");
        jPanel3.add(applyButton);

        doneButton.setText("Done");
        doneButton.addActionListener(this);
        jPanel3.add(doneButton);

        buttonPanel.add(jPanel3, java.awt.BorderLayout.SOUTH);

        getContentPane().add(buttonPanel, java.awt.BorderLayout.SOUTH);
        
        // Add the action listener to the popup menu items
        addBeforeMI.addActionListener(this);
        addAfterMI.addActionListener(this);
        addMI.addActionListener(this);
        moveDownMI.addActionListener(this);
        moveUpMI.addActionListener(this);
        editMI.addActionListener(this);

		this.setBounds(50, 50, 50, 50);
        pack();
    }

    /**
     * Read the value inserted by the user and display the requested item
     *
     */
    private void onStrokesTF() {
        String str = strokesTF.getText();
        if (str==null) return;
        if (str.indexOf('/')>0) {
            // The crazy user insert the number in the nn/mm format ;-)
            String[] strings=str.split("/");
            str=strings[0];
        }
        try {
            int n = java.lang.Integer.parseInt(str);
            if (n>0 && n<=Strokes.size()) {
                saveCurrentStroke();
                currentStroke=n;
                displayStroke();
            }
            ratioBrowseButtons();
        } catch(NumberFormatException e) {
            // The user insert a wrong number => we do nothing
        }
    }

    /**
     * Select the text into the field
     */
    private void onStrokesTFFocus() {
        // Add your handling code here:
        strokesTF.selectAll();
    }

    /** 
     * Closes the dialog
     * 
     * @param evt The event
     */
    private void closeDialog(java.awt.event.WindowEvent evt) {
        setVisible(false);
        dispose();
    }
    
    /**
     * Save the current item and go to the beginning of the list
     * 
     * @post currentStroke==1
     *
     */
    private void onBeginButton() {
    	saveCurrentStroke();
    	currentStroke=1;
    	displayStroke();
    	ratioBrowseButtons();
    }

	/**
	 * Save the current item and go to the previous item
	 * 
	 * @post currentStroke==($pre(int,currentStroke))-1
	 *
	 */
	private void onLessButton() {
        // 
        saveCurrentStroke();
        currentStroke--;
        displayStroke();
        ratioBrowseButtons();
    }

	/** 
	 * Executed when the user create a new stroke
     * It instatiates a new (empty) stroke that insert into the vector at the requested position
     * then the new stroke is shown
     * 
     * @post Strokes.size()==($pre(int,Strokes.size()))+1 
     */
    private void onAddButton() {
        if (currentStroke>0) {
        	saveCurrentStroke();
        }
        // Create an empty linear stroke to insert into the vector and display (default)
        LinearStroke s = new LinearStroke(0, 0, 0, 0, 0, 0);
        if (currentStroke>=Strokes.size()) {
        	Strokes.add(s);
        }
        else {
        	Strokes.add(currentStroke, s);
        }
        currentStroke=currentStroke+1;
        displayStroke();
        ratioBrowseButtons();
        // Empty all the fields used by the other types of stroke
        longCenterTF.setText("0.0");
        latCenterTF.setText("0.0");
        polarOrientTF.setText("0.0");
        /** @assert arrayT!=null */
        DefaultTableModel model = (DefaultTableModel)arrayT.getModel();
        /** @assert model!=null */
        for (int t=0; t<model.getRowCount(); t++) {
        	model.removeRow(0);
        }
    }

	/**
	 * The user wants to remove the stroke
	 * 
	 * @post Strokes.size()==($pre(int,Strokes.size())-1) 
	 * @post currentStroke==($pre(int,currentStroke)-1)
	 */
	private void onRemoveButton() {
        Strokes.removeElementAt(currentStroke-1);
        currentStroke--;
        displayStroke();
        ratioBrowseButtons();
    }
	
	/**
	 * Save the current item and go to the next item
	 * 
	 * @post currentStroke==($pre(int,currentStroke))+1
	 */
	private void onMoreButton() {
        saveCurrentStroke();
        currentStroke++;
        displayStroke();
        ratioBrowseButtons();
    }

	/**
	 * Save the current item and go to the end
	 * 
	 * @post currentStroke==Strokes.size()
	 */
	private void onEndButton() {
        saveCurrentStroke();
        currentStroke=Strokes.size();
        displayStroke();
        ratioBrowseButtons();
    }

	/**
	 * The user has just terminated
	 */
	private void onDone() {
        setVisible(false);
        dispose();
    }
	
	/**
	 * The user wants to add a new row
	 *
	 * @post ((DefaultTableModel)arrayT.getModel()).getRowCount() ==($pre(int,((DefaultTableModel)arrayT.getModel()).getRowCount())+1)
	 */
	private void onAddTableRow() {
		/** @assert arrayT!=null */
		DefaultTableModel model = (DefaultTableModel)arrayT.getModel();
		/** @assert model!=null */
		model.addRow(new String[] {"0.0", "0.0", "0.0"});
	}
	
	/**
	 * The user wants to add a new row before the row over the mouse pointer
	 *
	 * @post ((DefaultTableModel)arrayT.getModel()).getRowCount()==($pre(int,((DefaultTableModel)arrayT.getModel()).getRowCount())+1)
	 */
	private void onAddBeforeTableRow() {
		/** @assert arrayT!=null */
		DefaultTableModel model = (DefaultTableModel)arrayT.getModel();
		/** @assert model!=null */
		model.insertRow(rowPressed, new String[] {"0.0", "0.0", "0.0"});
	}
	
	/**
	 * The user wants to add a new row after the row over the mouse pointer
	 *
	 * @post ((DefaultTableModel)arrayT.getModel()).getRowCount()==($pre(int,((DefaultTableModel)arrayT.getModel()).getRowCount())+1)
	 */
	private void onAddAfterTableRow() {
		/** @assert arrayT!=null */
		DefaultTableModel model = (DefaultTableModel)arrayT.getModel();
		/** @assert model!=null */
		model.insertRow(rowPressed+1, new String[] {"0.0", "0.0", "0.0"});
	}
	
	/**
	 * The user wants to move up a row of the table
	 * 
	 * @post ((DefaultTableModel)arrayT.getModel()).getRowCount()==($pre(int,((DefaultTableModel)arrayT.getModel()).getRowCount()))
	 *
	 */
	private void onMoveUpTableRow() {
		DefaultTableModel model = (DefaultTableModel)arrayT.getModel();
		model.moveRow(rowPressed, rowPressed, rowPressed-1);
	}
	
	/**
	 * The user wants to move down a row of the table
	 *
	 * @post ((DefaultTableModel)arrayT.getModel()).getRowCount() ==($pre(int,((DefaultTableModel)arrayT.getModel()).getRowCount()))
	 */
	private void onMoveDownTableRow() {
		DefaultTableModel model = (DefaultTableModel)arrayT.getModel();
		model.moveRow(rowPressed, rowPressed, rowPressed+1);
	}
	
	/**
	 * If the pop menu has to be shown, this method visualizes and hides
	 * the items depending on the position where the mouse button was
	 * pressed and then it alsoi shows the menu
	 * 
	 * @param e The event received 
	 */
	private void checkPopup(MouseEvent e) {
		/** @assert e!=null */
		if (e.isPopupTrigger()) {
			Point point = new Point(e.getX(), e.getY());
			rowPressed=arrayT.rowAtPoint(point);
			/** @assert rowPressed>=-1 */
			colPressed=arrayT.columnAtPoint(point);
			/** @assert colPressed>=-1 */
			if (rowPressed==-1) {
				// Mouse not pressed over a valid rowpressed
				addBeforeMI.setVisible(false);
				addAfterMI.setVisible(false);
				addMI.setVisible(true);
				moveUpMI.setEnabled(false);
				moveDownMI.setEnabled(false);
				editMI.setEnabled(false);
			} else {
				// Mouse pressed over a valid row
				addBeforeMI.setVisible(true);
				addAfterMI.setVisible(true);
				addMI.setVisible(false);
				moveUpMI.setEnabled(rowPressed!=0);
				/** @assert arrayT!=null */
				moveDownMI.setEnabled(rowPressed!=arrayT.getRowCount()-1);
				if (colPressed!=-1) {
					editMI.setEnabled(true);
				}
			}
			arrayPM.show(scrollPane, e.getX(), e.getY());
		} 
	}
}
