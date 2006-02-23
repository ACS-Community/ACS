/*
 * ConfigureBehaviourPanel.java
 *
 * Created on February 19, 2003, 3:30 PM
 */

package cern.laser.guiplatform.windows.behaviour;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;

import org.apache.log4j.Logger;
import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.util.NbBundle;

import cern.laser.console.Behaviour;
import cern.laser.console.Configuration;
import cern.laser.console.LaserConsoleException;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windows.DisplayableColumnExplorer;

/**
 *
 * @author  pawlowsk
 */
public class ConfigureBehaviourPanel extends javax.swing.JPanel 
    implements ActionListener {

    final static Logger logger = 
        LogFactory.getLogger(ConfigureBehaviourPanel.class.getName());
        
    /** contains things like "klaxon settings */
    JPanel behaviourPanel = null;
    
    DisplayableColumnExplorer columnsExplorer   = null;
    // ----------------------- objects
    //
    AlarmDailyPrintPanel dailyPrintPanel        = null;
    SimpleChoosePanel simChoosePanel            = null;
    SimpleChoosePanel behChoosePanel            = null;
    SimpleChoosePanel klaxonChoosePanel         = null;
    ChooseKlaxonVolumePanel klaxonVoluePanel    = null;
    SimpleChoosePanel activeListFontPanel       = null;
    SimpleChoosePanel clockMenuBarPanel         = null;
    SimpleChoosePanel reducedMaskFlagPanel    = null;
       
  
    private static final String RIGHT           = "right";
    private static final String LEFT            = "left";
    private static final String FIRST           = "first";
    private static final String SECOND          = "second";
    private static final String THIRD           = "third";
    private static final String FOURTH          = "fourth";



    /** configuration */
    Configuration configuration = null;
    
    /** Creates a new instance of ConfigureBehaviourPanel */
    public ConfigureBehaviourPanel(Configuration configuration) 
        throws LaserConsoleException {
        super();
        this.configuration = configuration;
        initComponent();
    }
    
  
    private void initComponent() throws LaserConsoleException {
        setLayout(new GridLayout(0, 2));

        behaviourPanel = new JPanel();

        dailyPrintPanel = new AlarmDailyPrintPanel(
                        NbBundle.getMessage(
                            ConfigureBehaviourPanel.AlarmDailyPrintPanel.class,
                            "LBL_AlarmDailyPrintPanel_border_name"),
                        NbBundle.getMessage(
                            ConfigureBehaviourPanel.AlarmDailyPrintPanel.class,
                            "LBL_AlarmDailyPrintPanel_RadioButton_name"),
                        configuration.getBehaviour().isDailyPrinting(),
                        configuration.getBehaviour().getDailyPrinter()
                        );
        dailyPrintPanel.addConfigurationChangeListener(
            new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    cern.laser.guiplatform.util.AppRegister.getInstance().notifyConfigurationChange();
                }
            });

        behaviourPanel.add(dailyPrintPanel);

        behaviourPanel.setLayout(new GridLayout(7, 0));
       
        Behaviour behaviour = configuration.getBehaviour();
        boolean reducedMaskSet = configuration.getSelection().getReducedMaskedSelection();

        // TODO: move all name like "New Alarms Behaviour" to
        // Bundle.properties file 
        simChoosePanel = new SimpleChoosePanel(
                        "New Alarms Behaviour", "Distinguish" ,"Same", 
                        (behaviour.isAlarmDistinguished() ? LEFT : RIGHT));
        simChoosePanel.addConfigurationChangeListener(
            new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    cern.laser.guiplatform.util.AppRegister.getInstance().notifyConfigurationChange();
                }
            });
        behaviourPanel.add(simChoosePanel);
 
        behChoosePanel = new SimpleChoosePanel(
                        "Terminates Behaviour", "Automatic" ,"Manual", 
                        (behaviour.isAlarmAutoTerminated() ? LEFT : RIGHT));
        behChoosePanel.addConfigurationChangeListener(
            new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    cern.laser.guiplatform.util.AppRegister.getInstance().notifyConfigurationChange();
                }
            });
        behaviourPanel.add(behChoosePanel);
        
        klaxonChoosePanel = new SimpleChoosePanel(
                        "Klaxon Setting", "Selected FS" ,"Every FS", 
                        (behaviour.isAlarmAutoKlaxon() ? RIGHT : LEFT));
        klaxonChoosePanel.addConfigurationChangeListener(
            new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    cern.laser.guiplatform.util.AppRegister.getInstance().notifyConfigurationChange();
                }
            });
        behaviourPanel.add(klaxonChoosePanel);
        
        klaxonVoluePanel = new ChooseKlaxonVolumePanel("Klaxon Volume", "No",
                            "Bell High", "Bell Low", "Klaxon", 
                            setKlaxonVolume(behaviour.getKlaxonVolume()));
        klaxonVoluePanel.addConfigurationChangeListener(
            new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    cern.laser.guiplatform.util.AppRegister.getInstance().notifyConfigurationChange();
                }
            });
        behaviourPanel.add(klaxonVoluePanel);

        activeListFontPanel = new SimpleChoosePanel(
                        "Active List Font Behaviour", "Small font" ,"Big font",
                        (LEFT));        // this shoule be implemented,
                                        // because currently it does not exists in console API
        activeListFontPanel.addConfigurationChangeListener(
            new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    cern.laser.guiplatform.util.AppRegister.getInstance().notifyConfigurationChange();
                }
            });
        behaviourPanel.add(activeListFontPanel);
        
        clockMenuBarPanel = new SimpleChoosePanel(
                        "Show Clock in menubar", "Yes" ,"No", 
                        (LEFT));        // this shoule be implemented,                         
                                        // because currently it does not exists in console API
        clockMenuBarPanel.addConfigurationChangeListener(
            new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    cern.laser.guiplatform.util.AppRegister.getInstance().notifyConfigurationChange();
                }
            });
        behaviourPanel.add(klaxonChoosePanel);
        
        reducedMaskFlagPanel= new SimpleChoosePanel(
                        "Reduction Mask Filtering", "On" ,"Off", 
                        (reducedMaskSet ? LEFT : RIGHT));                                 
                                         
        reducedMaskFlagPanel.addConfigurationChangeListener(
            new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    cern.laser.guiplatform.util.AppRegister.getInstance().notifyConfigurationChange();
                }
            });
        behaviourPanel.add(reducedMaskFlagPanel);

        columnsExplorer = new DisplayableColumnExplorer(
                                configuration.getBehaviour().getColumnsToDisplay(),
                                false);
                            //new String [] { "faultFamily", "faultMember", });
        
        add(columnsExplorer);
        add(behaviourPanel);
    }
 
    public void actionPerformed(java.awt.event.ActionEvent actionEvent) {


    }
    
    /**
     * This method resets all component for this class
     *
     * TODO: this method shuld be in interface. And this interface should be 
     * impelmented by all tabbed panes.
     *
     */
    public void reset() {
        simChoosePanel.setSelected(LEFT);
        behChoosePanel.setSelected(RIGHT);
        klaxonChoosePanel.setSelected(RIGHT);
        activeListFontPanel.setSelected(LEFT);
        clockMenuBarPanel.setSelected(LEFT); 
        reducedMaskFlagPanel.setSelected(RIGHT); 

        klaxonVoluePanel.setDefaultChecked();
        dailyPrintPanel.reset();

        columnsExplorer.reset();
        
    }
    
    /**
     * This method returns selected behaviour as string
     * name=value;name=value;
     * 
     * In final version this should be changed
     * @deprecated not implemented yet, probably is useless
     */
    public HashMap getSelectedBehaviuor() {
        throw new UnsupportedOperationException("Method not implemented yet");
    }
   
    //
    // -- methods for setting behaviour -------------------------------
    // 
    public boolean isAlarmAutoKlaxon() {
        
        if ( klaxonChoosePanel.getChoosenButton().equals(LEFT) ) 
            return false;
        else 
            return true;

    } 
    
    public String getKlaxonVolume() {
        //if ( isAlarmAutoKlaxon() ) {
            if ( klaxonVoluePanel.getChoosenButton().equals(SECOND) )
                return Behaviour.HIGH_BELL_VOLUME;
            else if ( klaxonVoluePanel.getChoosenButton().equals(THIRD) )
                return Behaviour.LOW_BELL_VOLUME;
            else if ( klaxonVoluePanel.getChoosenButton().equals(FOURTH) ) 
                return Behaviour.KLAXON_VOLUME;
            else if ( klaxonVoluePanel.getChoosenButton().equals(FIRST) ) 
                return Behaviour.SILENT_VOLUME;
        //}
        
        return Behaviour.SILENT_VOLUME;

    } 
   
    /**
     * @param volume one of: Behaviour.HIGH_BELL_VOLUME,
     * Behaviour.KLAXON_VOLUME, Behaviour.SILENT_VOLUME,
     * Behaviour.LOW_BELL_VOLUME
     */ 
    private String setKlaxonVolume(String volume) {
        if ( volume.equals(Behaviour.SILENT_VOLUME) ) 
            return FIRST;
        else if ( volume.equals(Behaviour.HIGH_BELL_VOLUME) )
            return SECOND;
        else if ( volume.equals(Behaviour.LOW_BELL_VOLUME) )
            return THIRD;

        return FOURTH;

    }

    public boolean isDailyPrinting() {
        return dailyPrintPanel.isDailyPrinting();
    }

    public String getDailyPrinter() {
        return dailyPrintPanel.getDailyPrinterName();
    } 

    public boolean isAlarmDistinguished() {
        if ( simChoosePanel.getChoosenButton().equals(LEFT) )
            return true;

        return false;
    }

    public boolean isAlarmAutoTerminated() {
        if ( behChoosePanel.getChoosenButton().equals(LEFT) )
            return true;
        return false;
    }

    public boolean isReducedMaskedFlagOn() {
        if ( reducedMaskFlagPanel.getChoosenButton().equals(LEFT) )
            return true;
        return false;
    }

    public String [] getColumnsToDisplay() {
        List list = columnsExplorer.getEnabledColumns();
        String [] newColumns = (String [])list.toArray(new String[0]);
    
        logger.debug("getColumnsToDisplay() " + list.toString());
        return newColumns;
     
    }

    public void updateBehaviour(Configuration newConfiguration) 
        throws LaserConsoleException {

        configuration = newConfiguration;
        Behaviour behaviour = configuration.getBehaviour();
        boolean reducedMaskSet = configuration.getSelection().getReducedMaskedSelection();

        dailyPrintPanel.updatePanel(configuration.getBehaviour().isDailyPrinting(),
                        configuration.getBehaviour().getDailyPrinter());

        simChoosePanel.setSelected(behaviour.isAlarmDistinguished() ? LEFT : RIGHT);     
        behChoosePanel.setSelected(behaviour.isAlarmAutoTerminated() ? LEFT : RIGHT);
        klaxonChoosePanel.setSelected(behaviour.isAlarmAutoKlaxon() ? RIGHT : LEFT);
        klaxonVoluePanel.setSelected(setKlaxonVolume(behaviour.getKlaxonVolume()));

        activeListFontPanel.setSelected(LEFT);
        clockMenuBarPanel.setSelected(LEFT);


        reducedMaskFlagPanel.setSelected(reducedMaskSet ? LEFT : RIGHT);

        columnsExplorer.updateColumns(
            configuration.getBehaviour().getColumnsToDisplay());
    }
 
 
    /** ******************************************************************
     *       private class Klaxon Volume panel
     * *******************************************************************/
    private class ChooseKlaxonVolumePanel extends JPanel 
        implements ActionListener 
    {

        /** left rafio button */
        private JRadioButton firstRadio = null;
    
        /** rght rafio button */
        private JRadioButton secondRadio = null;
        
        /** rght rafio button */
        private JRadioButton thirdRadio = null;
       
        /** rght rafio button */
        private JRadioButton fourthRadio = null;

        /** default selected */
        private String defaultChecked = null;

        /** listener which is able to change Save button on ConfigurationPanel */
        private ActionListener confChangeListener = null;

        public ChooseKlaxonVolumePanel() {
            super();
            //initComponents();
        }

        /**
         * Create simple choose panel with 4 radio buttons 
         *
         * This method initialize all component.
         * Creates TitleBorder, radio button and all necesary components.
         * 
         * @param defaultChecked - which radio button should be default checked
         *                       FIRST for first, SECOND for second,
         *                       etc ... or null
         */
        public ChooseKlaxonVolumePanel(String borderTitle, 
                                        String firstRadioTitle, 
                                        String secondRadioTitle,
                                        String thirdRadioTitle,
                                        String fourthRadioTitle,
                                        String defaultChecked)
        {
            super();

            initComponents(borderTitle, firstRadioTitle, secondRadioTitle,
                           thirdRadioTitle, fourthRadioTitle, defaultChecked);
        }
        
        private void initComponents(String borderTitle, String firstRadioTitle, 
                                        String secondRadioTitle,
                                        String thirdRadioTitle,
                                        String fourthRadioTitle,
                                        String defaultChecked)
        {
            this.defaultChecked = defaultChecked;

            Border etched = BorderFactory.createEtchedBorder();
            TitledBorder titleBorder = BorderFactory.createTitledBorder(etched, 
                                                        borderTitle);
            //titleBorder.setTitleFont(new Font(null, Font.BOLD, 12));
            setLayout(new GridLayout(2, 2));
            setBorder(titleBorder);


            // add radio buttons
            firstRadio = new JRadioButton(firstRadioTitle);
            //firstRadio.setFont(new Font(null, Font.PLAIN, 12));
            // adding action listener
            firstRadio.addActionListener(this);

            // add radio buttons
            secondRadio = new JRadioButton(secondRadioTitle);
            //secondRadio.setFont(new Font(null, Font.PLAIN, 12));
            // adding action listener
            secondRadio.addActionListener(this);

            // add radio buttons
            thirdRadio = new JRadioButton(thirdRadioTitle);
            //thirdRadio.setFont(new Font(null, Font.PLAIN, 12));
            // adding action listener
            thirdRadio.addActionListener(this);


            // add radio buttons
            fourthRadio = new JRadioButton(fourthRadioTitle);
            //fourthRadio.setFont(new Font(null, Font.PLAIN, 12));
            // adding action listener
            fourthRadio.addActionListener(this);


            if ( defaultChecked != null ) {
                if ( defaultChecked.equals(FIRST) ) firstRadio.setSelected(true);
                else if ( defaultChecked.equals(SECOND) ) secondRadio.setSelected(true);
                else if ( defaultChecked.equals(THIRD) ) thirdRadio.setSelected(true);
                else if ( defaultChecked.equals(FOURTH) ) fourthRadio.setSelected(true);
            }

            add(firstRadio);
            add(secondRadio);
            add(thirdRadio);
            add(fourthRadio);
        }
         
        
        public void actionPerformed(java.awt.event.ActionEvent actionEvent) {
            if ( actionEvent.getSource().equals(firstRadio) ) {
                    setSelected(FIRST);
            } else if (actionEvent.getSource().equals(secondRadio) ) {
                    setSelected(SECOND);
            } else if (actionEvent.getSource().equals(thirdRadio) ) {
                    setSelected(THIRD);
            } else if (actionEvent.getSource().equals(fourthRadio) ) {
                    setSelected(FOURTH);
            } 
            if ( confChangeListener != null )
                confChangeListener.actionPerformed(actionEvent);

        }

        /** 
         * This method set checked left of right radio button.
         *
         * @param  whichOne - FIRST for first radio button, SECOND for second,
         *                      etc  
         */
        public void setSelected(String whichOne) {

            if ( whichOne.equals(FIRST) ) {
                firstRadio.setSelected(true);
                secondRadio.setSelected(false);
                thirdRadio.setSelected(false);
                fourthRadio.setSelected(false);
            } else if ( whichOne.equals(SECOND) ) {
                firstRadio.setSelected(false);
                secondRadio.setSelected(true);
                thirdRadio.setSelected(false);
                fourthRadio.setSelected(false);
            } else if ( whichOne.equals(THIRD) ) {
                firstRadio.setSelected(false);
                secondRadio.setSelected(false);
                thirdRadio.setSelected(true);
                fourthRadio.setSelected(false);
            } else if ( whichOne.equals(FOURTH) ) {
                firstRadio.setSelected(false);
                secondRadio.setSelected(false);
                thirdRadio.setSelected(false);
                fourthRadio.setSelected(true);
            }
        }

        public void setDefaultChecked() {
            if ( defaultChecked != null ) setSelected(defaultChecked);
        }

        /**
         * This method tells, which button is selected.
         * 
         * @return FIRST if first radio button is selected
         *          SECOND is second radion button is selected
         *          etc
         *
         */
        public String getChoosenButton() {
            if ( firstRadio.isSelected() )
               return FIRST;
            else if ( secondRadio.isSelected() ) 
                return SECOND;
            else if ( thirdRadio.isSelected() ) 
                return THIRD;
            else 
                return FOURTH;

        }
        
        public void addConfigurationChangeListener(java.awt.event.ActionListener listener) {
            confChangeListener = listener;
        }

    } // end private class Klaxon Volume Panel
    
 
    /**
     * This panel contains one button "daily print"
     * and one button "set daily printer"
     */ 
    private class AlarmDailyPrintPanel extends JPanel {

        final Logger logger =
            LogFactory.getLogger(AlarmDailyPrintPanel.class.getName());

        private JButton setDailyPrinterButton   = null;
        private JRadioButton radioButton        = null;
        private JButton setPrinterButton        = null;
        private String dailyPrinterName         = null;
        private JLabel printerNameLabel         = null;

        /** listener which is able to change Save button on ConfigurationPanel */
        private ActionListener confChangeListener = null;

        /** Constructor 
         * @param borderTitle
         * @param radioTitle
         * @param checked indicates whether radio should be checked
         * @param printerName printer name
         */
        public AlarmDailyPrintPanel(String borderTitle, String radioTitle,
                boolean checked, String printerName) {

            dailyPrinterName = printerName;

            Border etched = BorderFactory.createEtchedBorder();
            TitledBorder titleBorder = BorderFactory.createTitledBorder(etched, 
                                                        borderTitle);

            setLayout(new GridLayout(2, 0));
            setBorder(titleBorder);

            JPanel panel_temp = new JPanel(new BorderLayout());
            radioButton = new JRadioButton(radioTitle);
            radioButton.addActionListener(new java.awt.event.ActionListener() {
                    public void actionPerformed(java.awt.event.ActionEvent evt) {
                        radioButtonActionPerformed(evt);
                    }
            });
             if ( checked )
                radioButton.setSelected(true);
            panel_temp.add(radioButton, BorderLayout.WEST);

            setPrinterButton = new JButton("Set Daily Printer");
            setPrinterButton.addActionListener(new java.awt.event.ActionListener() {
                    public void actionPerformed(java.awt.event.ActionEvent evt) {
                        setPrinterButtonActionPerformed(evt);
                    }
            });
            panel_temp.add(setPrinterButton, BorderLayout.EAST);
            add(panel_temp);
            
            printerNameLabel = new JLabel("Printer name: " + dailyPrinterName);
            add(printerNameLabel);  
     
        }

        //
        // -- action listener method -----------------------
        //
        public void setPrinterButtonActionPerformed(java.awt.event.ActionEvent evt) {
            NotifyDescriptor.InputLine desc = new NotifyDescriptor.InputLine(
                                                    "Give printer name", 
                                                    "Printer name",
                                                    NotifyDescriptor.DEFAULT_OPTION,
                                                    NotifyDescriptor.PLAIN_MESSAGE);
            if ( DialogDisplayer.getDefault().notify(desc) == NotifyDescriptor.OK_OPTION ) {
                dailyPrinterName = desc.getInputText();
                printerNameLabel.setText("Printer name: " + dailyPrinterName);
                if ( confChangeListener != null )
                    confChangeListener.actionPerformed(evt);
            }

        }
        public void radioButtonActionPerformed(java.awt.event.ActionEvent evt) {
            if ( confChangeListener != null )
                confChangeListener.actionPerformed(evt);
         }

        /**
         * This method resets this panel
         */
        public void reset() {
            dailyPrinterName = "";
            printerNameLabel.setText("Printer name: " + dailyPrinterName);
            radioButton.setSelected(false);
        }

        public void updatePanel(boolean newDailyPrint, String newPrinterName) {
            dailyPrinterName = newPrinterName;
            printerNameLabel.setText("Printer name: " + dailyPrinterName);
            radioButton.setSelected(newDailyPrint);
        }

        /**
         * @return daily printer name
         */
        public String getDailyPrinterName() {
            return dailyPrinterName;
        }

        /**
         * @reuturn true is daily print radio button is checked
         *          false otherwise
         */
        public boolean isDailyPrinting() {
            return radioButton.isSelected();
        }
           
        public void addConfigurationChangeListener(java.awt.event.ActionListener listener) {
           //setPrinterButton.addActionListener(listener);
           confChangeListener = listener;
        } 
    } // end inner class AlarmDailyPrintPanel



    
}
