
/*
 * SimpleChoosePanel.java
 *
 * Created on February 18, 2003, 3:15 PM
 */

package cern.laser.guiplatform.windows.behaviour;

import java.awt.GridLayout;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.JRadioButton;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;

import org.apache.log4j.Logger;

import cern.laser.guiplatform.util.LogFactory;
/**
 *
 * @author  pawlowsk
 */
/**
 *
 * This is simple choose panel, which is used in configuration 
 * window. This class has titled panel with 2 radio buttons
 * 
 *
 *
 */
public class SimpleChoosePanel extends javax.swing.JPanel 
    implements ActionListener {
 

    /** logger */
    private static Logger logger = 
        LogFactory.getLogger(SimpleChoosePanel.class.getName());

    /** left rafio button */
    private JRadioButton leftRadio = null;
    
    /** rght rafio button */
    private JRadioButton rightRadio = null;
        
    /** listener which is able to change Save button on ConfigurationPanel */
    private ActionListener confChangeListener = null;

    /** Create simple choose panel with 2 radio buttons */
    public SimpleChoosePanel() {
	    super();
    }
    
    /** Contructor 
     *
     *
     *
     * @param borderTitle border title for this component
     * @param leftRadioTitle
     * @param rightRadioTitle
     */
    public SimpleChoosePanel(String borderTitle, 
                            String leftRadioTitle, String rightRadioTitle) {

        super();
        initComponent(borderTitle, leftRadioTitle, rightRadioTitle, null);

    }

    /** Contructor 
     *
     *
     *
     * @param borderTitle border title for this component
     * @param leftRadioTitle
     * @param rightRadioTitle
     * @param defaultChecked which radio should be checked ("left" or "right")
     */
    public SimpleChoosePanel(String borderTitle, 
                            String leftRadioTitle, String rightRadioTitle,
                            String defaultChecked ) {

        super();
        initComponent(borderTitle, leftRadioTitle, rightRadioTitle, 
                        defaultChecked);
    }

    
    /**
     * This methid initialize all component.
     * Creates TitleBorder, radio button and all necesary components.
     * 
     * @param defaultChecked - which radio button should be default checked
     *                       "left" for left, "right" for right or null
     */
    private void initComponent(String borderTitle, 
                            String leftRadioTitle, String rightRadioTitle,
                            String defaultChecked) {
        Border etched = BorderFactory.createEtchedBorder();
        TitledBorder titleBorder = BorderFactory.createTitledBorder(etched, 
                                                    borderTitle);
        //titleBorder.setTitleFont(new Font(null, Font.BOLD, 12));
        setLayout(new GridLayout(0, 2));
        setBorder(titleBorder);

        // add radio buttons
        leftRadio = new JRadioButton(leftRadioTitle);
        //leftRadio.setFont(new Font(null, Font.PLAIN, 12));
        
        // adding action listener
        leftRadio.addActionListener(this);

        rightRadio = new JRadioButton(rightRadioTitle);
        //rightRadio.setFont(new Font(null, Font.PLAIN, 12));
        // adding action listener
        rightRadio.addActionListener(this);

        if ( defaultChecked != null ) {
            if ( defaultChecked.equals("left") ) leftRadio.setSelected(true);
            else if ( defaultChecked.equals("right") ) rightRadio.setSelected(true);
        }
        add(leftRadio);
        add(rightRadio);

    }

    /**
     * This method initializes component.
     */
    private void initComponent(String borderTitle, 
                            String leftRadioTitle, String rightRadioTitle) {
        initComponent(borderTitle, leftRadioTitle, rightRadioTitle, null);
    }
    
    public void actionPerformed(java.awt.event.ActionEvent actionEvent) {
        logger.debug(" checbox changed !!!!!!! " );
        logger.debug(actionEvent.getActionCommand());
       
        if ( actionEvent.getSource().equals(leftRadio) ) {
            leftRadio.setSelected(true);
            rightRadio.setSelected(false);
        } else if (actionEvent.getSource().equals(rightRadio) ) {
            rightRadio.setSelected(true);
            leftRadio.setSelected(false);
        }

        if ( confChangeListener != null )
            confChangeListener.actionPerformed(actionEvent);
 
    }
  
    /**
     * This method tells, which button is selected.
     * 
     * @return "left" if left radio button is selected
     *          "right" is right radion button is selected
     *
     */
    public String getChoosenButton() {
        if ( leftRadio.isSelected() ) return "left";
        return "right";
    }

    
    /** 
     * This method set checked left of right radio button.
     *
     * @param  whichOne - "left" for left radio button, "right" for right radio button 
     */
    public void setSelected(String whichOne) {
        if ( whichOne.equals("left") ) {
            leftRadio.setSelected(true);
            rightRadio.setSelected(false);
        }
        else if ( whichOne.equals("right") ) {
            rightRadio.setSelected(true);
            leftRadio.setSelected(false);
        }
    }

    public void setLeftText(String text) {
        leftRadio.setText(text);
    }
    public void setRightText(String text) {
        rightRadio.setText(text);
    }


    public void addConfigurationChangeListener(java.awt.event.ActionListener listener) {
       //setPrinterButton.addActionListener(listener);
       confChangeListener = listener;
    } 

}
