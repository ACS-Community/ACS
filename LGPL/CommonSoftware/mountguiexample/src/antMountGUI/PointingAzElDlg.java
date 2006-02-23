/*
 * PointingAzElDlg.java
 *
 * Created on January 27, 2004, 5:42 PM
 */

package antMountGUI;

/**
 * The dialog to point in Azimuth and Elevation
 * 
 * @author  acaproni
 * 
 * @version 1.0
 */
public class PointingAzElDlg extends javax.swing.JDialog {
	
	/**
	 * @param args the command line arguments
	 */
	public static void main(String args[]) {
		new PointingAzElDlg(new javax.swing.JFrame(), true).show();
	}
    
    /**
     * Creates new form PointingAzElDlg
     * 
     * @param parent The parent Frame
     * @param modal If true the dialog is modal
     *
     */ 
    public PointingAzElDlg(java.awt.Frame parent, boolean modal) {
        super(parent, modal);
        initComponents();
    }
    
    /** 
     * This method is called from within the constructor to
     * initialize the form.
     */
    private void initComponents() {
        southPanel = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        strokeValueSlider = new javax.swing.JSlider();
        jPanel11 = new javax.swing.JPanel();
        jPanel13 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jComboBox1 = new javax.swing.JComboBox();
        strokeValueTF = new javax.swing.JTextField();
        jPanel12 = new javax.swing.JPanel();
        jButton5 = new javax.swing.JButton();
        jButton6 = new javax.swing.JButton();
        jSeparator1 = new javax.swing.JSeparator();
        centralPanel = new javax.swing.JPanel();
        gridPanel = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        jPanel3 = new javax.swing.JPanel();
        jButton1 = new javax.swing.JButton();
        jPanel4 = new javax.swing.JPanel();
        jPanel5 = new javax.swing.JPanel();
        jButton2 = new javax.swing.JButton();
        jPanel6 = new javax.swing.JPanel();
        jPanel7 = new javax.swing.JPanel();
        jButton3 = new javax.swing.JButton();
        jPanel8 = new javax.swing.JPanel();
        jPanel9 = new javax.swing.JPanel();
        jButton4 = new javax.swing.JButton();
        jPanel10 = new javax.swing.JPanel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Pointing");
        setFont(new java.awt.Font("A320PIC", 0, 10));
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                closeDialog(evt);
            }
        });

        southPanel.setLayout(new java.awt.BorderLayout());

        jPanel1.setLayout(new java.awt.BorderLayout());

        strokeValueSlider.setMaximum(600);
        strokeValueSlider.setPaintLabels(true);
        strokeValueSlider.setPaintTicks(true);
        strokeValueSlider.setSnapToTicks(true);
        strokeValueSlider.setValue(10);
        strokeValueSlider.setName("strokeSliderValue");
        strokeValueSlider.addChangeListener(new javax.swing.event.ChangeListener() {
            public void stateChanged(javax.swing.event.ChangeEvent evt) {
                onSliderStateChanged(evt);
            }
        });

        jPanel1.add(strokeValueSlider, java.awt.BorderLayout.NORTH);

        jPanel11.setLayout(new java.awt.BorderLayout());

        jPanel13.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.RIGHT));

        jLabel1.setText("Pointing strokes");
        jPanel13.add(jLabel1);

        jComboBox1.setMaximumRowCount(2);
        jComboBox1.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "arcsec", "arcmin" }));
        jPanel13.add(jComboBox1);

        jPanel11.add(jPanel13, java.awt.BorderLayout.EAST);

        strokeValueTF.setColumns(4);
        strokeValueTF.setEditable(false);
        strokeValueTF.setHorizontalAlignment(javax.swing.JTextField.LEFT);
        strokeValueTF.setText("1.0");
        strokeValueTF.setBorder(null);
        strokeValueTF.setName("strokeValueTF");
        jPanel11.add(strokeValueTF, java.awt.BorderLayout.WEST);

        jPanel1.add(jPanel11, java.awt.BorderLayout.SOUTH);

        southPanel.add(jPanel1, java.awt.BorderLayout.NORTH);

        jButton5.setText("Save");
        jPanel12.add(jButton5);

        jButton6.setText("Done");
        jButton6.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                onDone(evt);
            }
        });

        jPanel12.add(jButton6);

        southPanel.add(jPanel12, java.awt.BorderLayout.SOUTH);

        southPanel.add(jSeparator1, java.awt.BorderLayout.CENTER);

        getContentPane().add(southPanel, java.awt.BorderLayout.SOUTH);

        centralPanel.setName("centralPanel");
        gridPanel.setLayout(new java.awt.GridLayout(3, 3));

        gridPanel.add(jPanel2);

        jButton1.setText("El+");
        jPanel3.add(jButton1);

        gridPanel.add(jPanel3);

        gridPanel.add(jPanel4);

        jButton2.setText("Az-");
        jPanel5.add(jButton2);

        gridPanel.add(jPanel5);

        gridPanel.add(jPanel6);

        jButton3.setText("Az+");
        jPanel7.add(jButton3);

        gridPanel.add(jPanel7);

        gridPanel.add(jPanel8);

        jButton4.setText("El-");
        jPanel9.add(jButton4);

        gridPanel.add(jPanel9);

        gridPanel.add(jPanel10);

        centralPanel.add(gridPanel);

        getContentPane().add(centralPanel, java.awt.BorderLayout.CENTER);

        setBounds(50, 50, 50, 50);
        pack();
    }

    /**
     * The user pressed the Done button
     * 
     * @param evt The event
     */
    private void onDone(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_onDone
        // Add your handling code here:
        dispose();
    }//GEN-LAST:event_onDone

    /**
     * The user moved the slider
     * 
     * @param evt The event
     */
    private void onSliderStateChanged(javax.swing.event.ChangeEvent evt) {
        // Add your handling code here:
        int intPart=strokeValueSlider.getValue()/10;
        int decPart=strokeValueSlider.getValue()-intPart*10;
        strokeValueTF.setText(""+intPart+'.'+decPart);
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
     * 
     */
    private javax.swing.JPanel centralPanel;
    
    /**
     * 
     */
    private javax.swing.JPanel gridPanel;
    
    /**
     * 
     */
    private javax.swing.JButton jButton1;
    
    /**
     * 
     */
    private javax.swing.JButton jButton2;
    
    /**
     * 
     */
    private javax.swing.JButton jButton3;
    
    /**
     * 
     */
    private javax.swing.JButton jButton4;
    
    /**
     * 
     */
    private javax.swing.JButton jButton5;
    
    /**
     * 
     */
    private javax.swing.JButton jButton6;
    
    /**
     * 
     */
    private javax.swing.JComboBox jComboBox1;
    
    /**
     * 
     */
    private javax.swing.JLabel jLabel1;
    
    /**
     * 
     */
    private javax.swing.JPanel jPanel1;
    
    /**
     * 
     */
    private javax.swing.JPanel jPanel10;
    
    /**
     * 
     */
    private javax.swing.JPanel jPanel11;
    
    /**
     * 
     */
    private javax.swing.JPanel jPanel12;
    
    /**
     * 
     */
    private javax.swing.JPanel jPanel13;
    
    /**
     * 
     */
    private javax.swing.JPanel jPanel2;
    
    /**
     * 
     */
    private javax.swing.JPanel jPanel3;
    
    /**
     * 
     */
    private javax.swing.JPanel jPanel4;
    
    /**
     * 
     */
    private javax.swing.JPanel jPanel5;
    
    /**
     * 
     */
    private javax.swing.JPanel jPanel6;
    
    /**
     * 
     */
    private javax.swing.JPanel jPanel7;
    
    /**
     * 
     */
    private javax.swing.JPanel jPanel8;
    
    /**
     * 
     */
    private javax.swing.JPanel jPanel9;
    
    /**
     * 
     */
    private javax.swing.JSeparator jSeparator1;
    
    /**
     * 
     */
    private javax.swing.JPanel southPanel;
    
    /**
     * 
     */
    private javax.swing.JSlider strokeValueSlider;
    
    /**
     * 
     */
    private javax.swing.JTextField strokeValueTF;

    
}
