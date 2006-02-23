/*
 * AntennaMountGUI.java
 *
 * Created on January 27, 2004, 5:57 PM
 */

package antMountGUI;

/**
 * 
 * @author  acaproni
 */

import java.awt.BorderLayout;
import java.beans.PropertyVetoException;

import javax.swing.ButtonGroup;
import javax.swing.JApplet;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JRadioButtonMenuItem;

import abeans.pluggable.acs.ACSAbeansEngine;
import abeans.pluggable.acs.maci.settings.ACSPlugSettingsPlugIn;

import com.cosylab.abeans.AbeansEngine;
import com.cosylab.abeans.AbeansLaunchable;
import com.cosylab.abeans.adapters.DoublePropertyAdapter;
import com.cosylab.abeans.plugins.AbeansStandardActionsPlugIn;
import com.cosylab.abeans.plugins.AbeansSystemMenuPlugIn;
import com.cosylab.abeans.plugins.AboutPlugIn;
import com.cosylab.gui.LabelDisplayer;
import com.cosylab.gui.framework.Desktop;
import com.cosylab.gui.framework.Launcher;
import com.cosylab.gui.framework.LauncherEnvironment;
import com.cosylab.gui.plugins.CosyStandardActionsPlugIn;
import com.cosylab.gui.plugins.VitragePlugIn;

/**
 * The mount GUI for an antenna
 * 
 * @author acaproni
 * 
 * @version 1.0
 */
public class AntennaMountGUI extends AbeansLaunchable {
    
    /**
     * ACS Abeans engined used by this application.
     */
    private ACSAbeansEngine engine;
    
    /**
     * Creates a new instance of this class. The default no-arg constructor
     * may be used only by visual builders to instantiate an instance of
     * launchable panel. During run-time spcific constructor must be used.
     * @see com.cosylab.abeans.AbeansLaunchable
     */
    public AntennaMountGUI() {
        super();
    }
    
    /**
     * Creates a new instance of this class that will reside in a <code>JFrame</code> container.
     * @see com.cosylab.abeans.AbeansLaunchable
     */
    public AntennaMountGUI(
    		Launcher launcher,
			LauncherEnvironment env, 
			JFrame owner) 	{
        super(launcher, env, owner);
    }
    
    /**
     * Creates a new instance of this class that will reside inside a <code>JInternalFrame</code>.
     * @see com.cosylab.abeans.AbeansLaunchable
     */
    public AntennaMountGUI(
    		Launcher launcher,
			LauncherEnvironment env,
			Desktop desk,
			JInternalFrame owner) {
        super(launcher, env, desk, owner);
    }
    
    /**
     * Creates a new instance of this class that will reside inside an applet in a web browser.
     * @see com.cosylab.abeans.AbeansLaunchable
     */
    public AntennaMountGUI(
    		Launcher launcher,
			LauncherEnvironment env,
			JApplet owner) 
	{
        super(launcher, env, owner);
    }
    
    /**
     * @see com.cosylab.abeans.AbeansLaunchable#getAbeansEngine()
     */
    public AbeansEngine getAbeansEngine() {
        if (engine == null)
            engine = new ACSAbeansEngine(getClass().getName());
        return engine;
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
      */
    private void initComponents() {
        modeButtonGroup = new javax.swing.ButtonGroup();
        connectAntennaDlg = new javax.swing.JDialog();
        jSeparator1 = new javax.swing.JSeparator();
        jPanel1 = new javax.swing.JPanel();
        doneButton = new javax.swing.JButton();
        singleAbeanSelector1 = new com.cosylab.abeans.SingleAbeanSelector();
        mountAnt = new alma.MOUNT_ACS.abeans.Mount();
        jInternalFrame3 = new javax.swing.JPanel();
        jTabPane2 = new javax.swing.JTabbedPane();
        dartPanel2 = new javax.swing.JPanel();
        centralPanel2 = new javax.swing.JPanel();
        equatorCoordsPanel2 = new javax.swing.JPanel();
        raPanel2 = new javax.swing.JPanel();
        referenceRA = new com.cosylab.abeans.displayers.DoubleNumberField();
        
        commandRA = new com.cosylab.abeans.displayers.DoubleNumberField();
        actualRA = new com.cosylab.abeans.displayers.DoubleNumberField();
        deviationRA = new com.cosylab.abeans.displayers.DoubleNumberField();
        decPanel2 = new javax.swing.JPanel();
        referenceDec = new com.cosylab.abeans.displayers.DoubleNumberField();
        commandDec = new com.cosylab.abeans.displayers.DoubleNumberField();
        actualDec = new com.cosylab.abeans.displayers.DoubleNumberField();
        deviationDec = new com.cosylab.abeans.displayers.DoubleNumberField();
        jPanel5 = new javax.swing.JPanel();
        eqSourceB = new javax.swing.JButton();
        eqPatternB = new javax.swing.JButton();
        horizCoordsPanel2 = new javax.swing.JPanel();
        azPanel2 = new javax.swing.JPanel();
        referenceAz = new com.cosylab.abeans.displayers.DoubleNumberField();
        commandAz = new com.cosylab.abeans.displayers.DoubleNumberField();
        deviationAz = new com.cosylab.abeans.displayers.DoubleNumberField();
        actualVelAz = new com.cosylab.abeans.displayers.DoubleNumberField();
        elPanel2 = new javax.swing.JPanel();
        referenceEl = new com.cosylab.abeans.displayers.DoubleNumberField();
        commandEl = new com.cosylab.abeans.displayers.DoubleNumberField();
        deviationEl = new com.cosylab.abeans.displayers.DoubleNumberField();
        actualVelEl = new com.cosylab.abeans.displayers.DoubleNumberField();
        jPanel6 = new javax.swing.JPanel();
        hrSourceB = new javax.swing.JButton();
        hrPatternB = new javax.swing.JButton();
        pointingB = new javax.swing.JButton();
        headerPanel2 = new javax.swing.JPanel();
        jLabel3 = new javax.swing.JLabel();
        jTextField3 = new javax.swing.JTextField();
        southPanel2 = new javax.swing.JPanel();
        dartboardBean1 = new dartboard.DartboardBean();
        commandPanel2 = new javax.swing.JPanel();
        shutdownB = new javax.swing.JToggleButton();
        standbyB = new javax.swing.JToggleButton();
        trackB = new javax.swing.JToggleButton();
        inactiveB = new javax.swing.JToggleButton();
        jMenuBar3 = new javax.swing.JMenuBar();
        antennaMenu2 = new javax.swing.JMenu();
        connectMenuItem2 = new javax.swing.JMenuItem();
        presetMenu2 = new javax.swing.JMenu();
        parkMenuItem2 = new javax.swing.JMenuItem();
        zenithMenuItem2 = new javax.swing.JMenuItem();
        jSeparator3 = new javax.swing.JSeparator();
        toolsMenu2 = new javax.swing.JMenu();
        commandMenuItem2 = new javax.swing.JMenuItem();
        jLogMenuItem2 = new javax.swing.JMenuItem();
        objectMenuItem2 = new javax.swing.JMenuItem();
        optionMenu = new javax.swing.JMenu();
        degreesMI= new JRadioButtonMenuItem();
        radiansMI= new JRadioButtonMenuItem();
        unitsBG=new ButtonGroup();
        

        /** @assert connectAntennaDlg!=null */
        connectAntennaDlg.setTitle("Connect Antenna");
        connectAntennaDlg.setModal(true);
        connectAntennaDlg.setName("connectAntennaDlg");
        connectAntennaDlg.getContentPane().add(jSeparator1, java.awt.BorderLayout.CENTER);

        /** @assert doneButton!=null */
        doneButton.setText("Done");
        doneButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                onConnectDlgDoneButton();
            }
        });

        jPanel1.add(doneButton);

        connectAntennaDlg.getContentPane().add(jPanel1, java.awt.BorderLayout.SOUTH);

        singleAbeanSelector1.setAbean(mountAnt);
        connectAntennaDlg.getContentPane().add(singleAbeanSelector1, java.awt.BorderLayout.NORTH);

        /** @assert mountAnt!=null */
        mountAnt.addLinkListener(new abeans.models.LinkListener() {
            public void linkLost(abeans.models.LinkEvent evt) {
                antennaLinkLost(evt);
            }
            public void linkEstablished(abeans.models.LinkEvent evt) {
                antennaLinkEstablished(evt);
            }
            public void linkableResumed(abeans.models.LinkEvent evt) {
            }
            public void linkableSuspended(abeans.models.LinkEvent evt) {
            }
        });

        setLayout(new java.awt.BorderLayout());

        setMinimumSize(new java.awt.Dimension(800, 600));
        setPreferredSize(new java.awt.Dimension(1024, 800));
        
        /** @assert jInternalFrame3!=null */
        jInternalFrame3.setMinimumSize(new java.awt.Dimension(800, 600));
        jInternalFrame3.setPreferredSize(new java.awt.Dimension(1024, 768));
        jInternalFrame3.setVisible(true);
        
        /** @assert jTabPane2!=null */
        jTabPane2.setMinimumSize(new java.awt.Dimension(640, 480));
        jTabPane2.setName("");
        
        /** @assert dartPanel2!=null */
        dartPanel2.setLayout(new java.awt.BorderLayout());

        /** @assert centralPanel2!=null */ 
        centralPanel2.setLayout(new java.awt.GridLayout(1, 2, 10, 0));

        centralPanel2.setMinimumSize(new java.awt.Dimension(320, 200));
        
        /** @assert equatorCoordsPanel2!=null */
        equatorCoordsPanel2.setLayout(new java.awt.BorderLayout());

        equatorCoordsPanel2.setBorder(new javax.swing.border.TitledBorder("Equatorial"));
        
        /** @assert raPanel2!=null */
        raPanel2.setLayout(new javax.swing.BoxLayout(raPanel2, javax.swing.BoxLayout.Y_AXIS));

        raPanel2.setBorder(new javax.swing.border.TitledBorder("RA"));
        
        /** @assert referenceRA!=null */
        referenceRA.setTitle("Reference");
        referenceRA.setUnits("hour");
        raPanel2.add(referenceRA);

        /** @assert commandRA!=null */
        commandRA.setTitle("Command");
        commandRA.setUnits("hour");
        raPanel2.add(commandRA);

        /** @assert actualRA!=null */
        actualRA.setTitle("Actual");
        actualRA.setUnits("hour");
        raPanel2.add(actualRA);

        /** @assert deviationRA!=null */
        deviationRA.setTitle("Deviation");
        deviationRA.setUnits("hour");
        raPanel2.add(deviationRA);

        /** @assert equatorCoordsPanel2!=null */
        equatorCoordsPanel2.add(raPanel2, java.awt.BorderLayout.WEST);

        /** @assert decPanel2!=null */
        decPanel2.setLayout(new javax.swing.BoxLayout(decPanel2, javax.swing.BoxLayout.Y_AXIS));

        decPanel2.setBorder(new javax.swing.border.TitledBorder("Dec"));
        
        /** @assert referenceDec!=null */
        referenceDec.setTitle("Reference");
        referenceDec.setUnits("deg");
        decPanel2.add(referenceDec);

        /** @assert commandDec!=null */
        commandDec.setTitle("Command");
        commandDec.setUnits("deg");
        decPanel2.add(commandDec);

        /** @assert actualDec!=null */
        actualDec.setTitle("Actual");
        actualDec.setUnits("deg");
        decPanel2.add(actualDec);

        /** @assert deviationDec!=null */
        deviationDec.setTitle("Deviation");
        deviationDec.setUnits("deg");
        decPanel2.add(deviationDec);

        equatorCoordsPanel2.add(decPanel2, java.awt.BorderLayout.EAST);

        /** @assert eqSourceB!=null */
        eqSourceB.setText("Source");
        eqSourceB.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                onEqSourcePressed();
            }
        });

        /** @assert jPanel5!=null */
        jPanel5.add(eqSourceB);

        /** @assert eqPatternB!=null */
        eqPatternB.setText("Pattern");
        eqPatternB.setToolTipText("");
        eqPatternB.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                onEqPatternPressed();
            }
        });

        jPanel5.add(eqPatternB);

        equatorCoordsPanel2.add(jPanel5, java.awt.BorderLayout.SOUTH);

        centralPanel2.add(equatorCoordsPanel2);

        /** @assert horizCoordsPanel2!=null */
        horizCoordsPanel2.setLayout(new java.awt.BorderLayout());

        horizCoordsPanel2.setBorder(new javax.swing.border.TitledBorder("Horizontal"));
        
        /** @assert azPanel2!=null */
        azPanel2.setLayout(new javax.swing.BoxLayout(azPanel2, javax.swing.BoxLayout.Y_AXIS));

        azPanel2.setBorder(new javax.swing.border.TitledBorder("Az"));
        referenceAz.setTitle("Reference");
        referenceAz.setUnits("deg");
        azPanel2.add(referenceAz);

        /** @assert commandAz!=null */
        commandAz.setTitle("Command");
        commandAz.setUnits("deg");
        azPanel2.add(commandAz);

        /** @assert azimuthAdapter!=null */
        azimuthAdapter = new DoublePropertyAdapter();
        azimuthAdapter.setDoubleProperty(mountAnt.getActAz());
        
        /** @assert azimuthLabel!=null */
        azimuthLabel = new LabelDisplayer();
        azimuthLabel.setValue("N/A");
        azimuthLabel.setTitle("Actual azimuth");
        
        /** @assert southPanel2!=null */
        southPanel2.add(BorderLayout.WEST, azimuthLabel);
        
        try {
        	azimuthAdapter.addConsumer(new ValueConverter(new ValuesShow(azimuthLabel), this));
        } catch (PropertyVetoException e) {
        	System.err.println("Exception assigning the adapter "+e.toString());
        }
        
        
        azPanel2.add(azimuthLabel);

        /** @assert deviationAz!=null */
        deviationAz.setTitle("Deviation");
        deviationAz.setUnits("deg");
        azPanel2.add(deviationAz);

        /** @assert actualVelAz!=null */
        actualVelAz.setTitle("Actual vel.");
        actualVelAz.setUnits("deg/s");
        azPanel2.add(actualVelAz);

        horizCoordsPanel2.add(azPanel2, java.awt.BorderLayout.WEST);

        /** @assert elPanel2!=null */
        elPanel2.setLayout(new javax.swing.BoxLayout(elPanel2, javax.swing.BoxLayout.Y_AXIS));

        elPanel2.setBorder(new javax.swing.border.TitledBorder("El"));
        
        /** @assert referenceEl!=null */
        referenceEl.setTitle("Reference");
        referenceEl.setUnits("deg");
        elPanel2.add(referenceEl);

        /** @assert commandEl!=null */
        commandEl.setTitle("Command");
        commandEl.setUnits("deg");
        elPanel2.add(commandEl);

        /** @assert elevationAdapter!=null */
        elevationAdapter = new DoublePropertyAdapter();
        elevationAdapter.setDoubleProperty(mountAnt.getActEl());
        
        /** @assert elevationLabel!=null */
        elevationLabel = new LabelDisplayer();
        elevationLabel.setValue("N/A");
        elevationLabel.setTitle("Actual elevation");
        southPanel2.add(BorderLayout.WEST, elevationLabel);
        
        try {
        	elevationAdapter.addConsumer(new ValueConverter(new ValuesShow(elevationLabel), this));
        } catch (PropertyVetoException e) {
        	System.err.println("Exception assigning the adapter "+e.toString());
        }
        elPanel2.add(elevationLabel);

        /** @assert deviationEl!=null */
        deviationEl.setTitle("Deviation");
        deviationEl.setUnits("deg");
        elPanel2.add(deviationEl);

        /** @assert actualVelEl!=null */
        actualVelEl.setTitle("Actual Vel.");
        actualVelEl.setUnits("deg/s");
        elPanel2.add(actualVelEl);

        horizCoordsPanel2.add(elPanel2, java.awt.BorderLayout.EAST);

        /** @assert hrSourceB!=null */
        hrSourceB.setText("Source");
        hrSourceB.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                onHrSourcePressed();
            }
        });

        /** @assert jPanel6!=null */
        jPanel6.add(hrSourceB);

        /** @assert hrPatternB!=null */
        hrPatternB.setText("Pattern");
        hrPatternB.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                onHrPatternPressed();
            }
        });

        jPanel6.add(hrPatternB);

        /** @assert pointingB!=null */
        pointingB.setText("Pointing");
        pointingB.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                onPointingButtonPressed();
            }
        });

        jPanel6.add(pointingB);

        horizCoordsPanel2.add(jPanel6, java.awt.BorderLayout.SOUTH);

        centralPanel2.add(horizCoordsPanel2);

        /** @assert dartPanel2!=null */
        dartPanel2.add(centralPanel2, java.awt.BorderLayout.CENTER);

        /** @assert headerPanel2!=null */
        headerPanel2.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 10, 5));

        /** @assert jLabel3!=null */
        jLabel3.setText("Sidereal time:");
        headerPanel2.add(jLabel3);

        /** @assert jTextField3!=null */
        jTextField3.setEditable(false);
        jTextField3.setText("10:12:20.02");
        jTextField3.setBorder(null);
        headerPanel2.add(jTextField3);

        dartPanel2.add(headerPanel2, java.awt.BorderLayout.NORTH);

        southPanel2.setMinimumSize(new java.awt.Dimension(300, 150));
        southPanel2.add(dartboardBean1);

        /** @assert commandPanel2!=null */
        commandPanel2.setLayout(new java.awt.GridLayout(4, 1));

        commandPanel2.setBorder(
        		new javax.swing.border.TitledBorder(null,
        				"Axis modes",
						javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION,
						javax.swing.border.TitledBorder.DEFAULT_POSITION,
						new java.awt.Font("Dialog", 0, 12)
					)
				);
        
        commandPanel2.setName("cmdPanel");
        
        /** @assert shutdownB!=null */
        shutdownB.setText("Shutdown");
        
        /** @assert modeButtonGroup!=null */
        modeButtonGroup.add(shutdownB);
        commandPanel2.add(shutdownB);

        /** @assert standbyB!=null */
        standbyB.setText("Standby");
        modeButtonGroup.add(standbyB);
        commandPanel2.add(standbyB);

        /** @assert trackB!=null */
        trackB.setText("Track");
        modeButtonGroup.add(trackB);
        trackB.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        commandPanel2.add(trackB);

        /** @assert inactiveB!=null */
        inactiveB.setText("Inactive");
        modeButtonGroup.add(inactiveB);
        commandPanel2.add(inactiveB);

        southPanel2.add(commandPanel2);

        dartPanel2.add(southPanel2, java.awt.BorderLayout.SOUTH);

        /** @assert jTabPane2!=null */
        jTabPane2.addTab("General", dartPanel2);

        jInternalFrame3.add(jTabPane2, java.awt.BorderLayout.CENTER);

        buildMenu();
        

        add(jInternalFrame3, java.awt.BorderLayout.CENTER);
        
        commandAz.setDoubleProperty(mountAnt.getCmdAz());
        commandEl.setDoubleProperty(mountAnt.getCmdEl());

        /** @assert dartboardBean1!=null */
        dartboardBean1.setTelescopeAzimuth(mountAnt.getActAz());
        dartboardBean1.setTelescopeElevation(mountAnt.getActEl());
        dartboardBean1.setTelescopeDestinationAzimuth(mountAnt.getCmdAz());
        dartboardBean1.setTelescopeDestinationElevation(mountAnt.getCmdEl());
    }
    
    /**
     * Build the menu bar
     *
     * @pre this.getJMenuBar()!=null
     */
    private void buildMenu() {
    	/** @assert antennaMenu2!=null */
    	antennaMenu2.setText("Antenna");
    	/** @assert connectMenuItem2!=null */
    	connectMenuItem2.setText("Connect");
    	connectMenuItem2.addActionListener(new java.awt.event.ActionListener() {
    		public void actionPerformed(java.awt.event.ActionEvent evt) {
    			onConnectAntennaMenu();
    		}
    	});

    	antennaMenu2.add(connectMenuItem2);

    	this.getJMenuBar().add(antennaMenu2);

    	/** @assert presetMenu2!=null */
    	presetMenu2.setText("Preset");
    	
    	/** @assert parkMenuItem2!=null */
    	parkMenuItem2.setText("Park");
    	presetMenu2.add(parkMenuItem2);

    	/** @assert zenithMenuItem2!=null */
    	zenithMenuItem2.setText("Zenith");
    	presetMenu2.add(zenithMenuItem2);

    	presetMenu2.add(jSeparator3);

    	this.getJMenuBar().add(presetMenu2);
    	
    	/** @assert optionMenu!=null */
    	optionMenu.setText("Options");
    	this.getJMenuBar().add(optionMenu);
    	
    	/** @assert degreesMI!=null */
    	degreesMI.setText("Degrees");
    	
    	/** @assert radiansMI!=null */
    	radiansMI.setText("Radiants");
    	
    	/** @assert unitsBG!=null */
    	unitsBG.add(degreesMI);
    	unitsBG.add(radiansMI);
    	degreesMI.setSelected(true);
    	radiansMI.setSelected(false);
    	optionMenu.add(degreesMI);
    	optionMenu.add(radiansMI);

    	/** @assert toolsMenu2!=null */
    	toolsMenu2.setText("Tools");
    	
    	/** @assert commandMenuItem2!=null */
    	commandMenuItem2.setText("Command Center");
    	toolsMenu2.add(commandMenuItem2);

    	/** @assert jLogMenuItem2!=null */
    	jLogMenuItem2.setText("jLog");
    	toolsMenu2.add(jLogMenuItem2);

    	/** @assert objectMenuItem2!=null */
    	objectMenuItem2.setText("Object Explorer");
    	toolsMenu2.add(objectMenuItem2);

    	this.getJMenuBar().add(toolsMenu2);
    	
    }

    /** 
     * The link with the component is lost
     *  
     * @param evt The event
     */
    private void antennaLinkLost(abeans.models.LinkEvent evt) {
        // Add your handling code here:
        enableWidgets(false);
    }

    /**
     * A connection with an antenna has been established
     * 
     * @param evt The event
     */
    private void antennaLinkEstablished(abeans.models.LinkEvent evt) {
        // Add your handling code here:
        enableWidgets(true);
    }

    /**
     * The user pressed the done button on the connect dialog box
     * 
     * @param evt
     */
    private void onConnectDlgDoneButton() {
        // Add your handling code here:
    	/** @assert connectAntennaDlg!=null */
        connectAntennaDlg.setVisible(false);
    }

    /**
     * The user select the connect menu item 

     */
    private void onConnectAntennaMenu() {
    	/** @assert connectAntennaDlg!=null */
        connectAntennaDlg.setModal(true);
        connectAntennaDlg.setBounds(10,50,10,10);
        connectAntennaDlg.pack();
        connectAntennaDlg.setVisible(true);
    }

    /**
     * The user pressed the button to define an hr patter
     */
    private void onHrPatternPressed() {
        // Add your handling code here:
        PatternDlg dlg = new PatternDlg((java.awt.Frame)null,true);
        /** @assert dlg!=null */
        dlg.setVisible(true);
    }

    /**
     * The user pressed the button to define a eq pattern
     *
     */
    private void onEqPatternPressed() {
        // Add your handling code here:
        PatternDlg dlg = new PatternDlg((java.awt.Frame)null,true);
        /** @assert dlg!=null */
        dlg.setVisible(true);
    }

    /**
     * The user press the button to define a new hr source
     *
     */
    private void onHrSourcePressed() {
        // Add your handling code here:
        hrSourceDlg dlg = new hrSourceDlg((java.awt.Frame)null,true,mountAnt);
        /** @assert dlg!=null */
        dlg.setVisible(true);
    }

    /**
     * The user press the button to define a new eq source
     *
     */
    private void onEqSourcePressed() {
        // Add your handling code here:
        eqSourceDlg dlg = new eqSourceDlg((java.awt.Frame)null,true);
        /** @assert dlg!=null */
        dlg.setVisible(true);
    }

    /**
     * The user press the button to select the pointing mode
     *
     */
    private void onPointingButtonPressed() {
         // Add your handling code here:
        PointingAzElDlg dlg = new PointingAzElDlg((java.awt.Frame)null,true);
        /** @assert dlg!=null */
        dlg.setVisible(true);
    }

    
    /**
     * This method is called by the Abeans framework at initialization.
     */
    public void userInitializeGUI() {
        initComponents();
        getSystemPane().setPreferredSize(getPreferredSize());
        enableWidgets(false);
    }
    
    /**
     * The main entry point for every Java application.
     */
    public static void main(String args[]) {
        AbeansLaunchable.launch(antMountGUI.AntennaMountGUI.class, args);
    }
    
    /**
     * This method initializes the standard Abeans plugins.
     */
    public void userInitializePlugIns()	{
        try {
     installPlugIn(AbeansSystemMenuPlugIn.class);
     installPlugIn(CosyStandardActionsPlugIn.class);
     installPlugIn(AboutPlugIn.class);
     installPlugIn(AbeansStandardActionsPlugIn.class);
     installPlugIn(VitragePlugIn.class);
     installPlugIn(ACSPlugSettingsPlugIn.class);
     
     // uncomment following line(s) to get plugin(s) installed at startup
     // installPlugIn(AbeansExceptionPanelPlugIn.class);
     // installPlugIn(TreeBrowserPlugIn.class);
     // installPlugIn(LoggingPlugIn.class);
     // installPlugIn(ReportAreaPlugIn.class);
        } catch (Exception e) {
     e.printStackTrace();
        }
    }
    
    /** Enable or disable the widgets */
    private void enableWidgets(boolean b) {
        referenceAz.setEnabled(b);
        commandAz.setEnabled(b);
        azimuthLabel.setEnabled(b);
        deviationAz.setEnabled(b);
        actualVelAz.setEnabled(b);
        referenceEl.setEnabled(b);
        commandEl.setEnabled(b);
        elevationLabel.setEnabled(b);
        deviationEl.setEnabled(b);
        actualVelEl.setEnabled(b);
        referenceRA.setEnabled(b);
        commandRA.setEnabled(b);
        actualRA.setEnabled(b);
        deviationRA.setEnabled(b);
        referenceDec.setEnabled(b);
        commandDec.setEnabled(b);
        actualDec.setEnabled(b);
        deviationDec.setEnabled(b);
        shutdownB.setEnabled(b);
        standbyB.setEnabled(b);
        trackB.setEnabled(b);
        inactiveB.setEnabled(b);
        eqSourceB.setEnabled(b);
        eqPatternB.setEnabled(b);
        hrSourceB.setEnabled(b);
        hrPatternB.setEnabled(b);
        pointingB.setEnabled(b);
    }

    /**
     * Return the way the user prefers to see the values on the GUI
     * 
     * @return True means that the user wants degrees
     * 
     * @pre degreesMI!=null
     */
    public boolean showDegrees() {
    	return degreesMI.isSelected();
    }
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private com.cosylab.abeans.displayers.DoubleNumberField actualDec;
    private com.cosylab.abeans.displayers.DoubleNumberField actualRA;
    private com.cosylab.abeans.displayers.DoubleNumberField actualVelAz;
    private com.cosylab.abeans.displayers.DoubleNumberField actualVelEl;
    private javax.swing.JMenu antennaMenu2;
    private javax.swing.JPanel azPanel2;
    private javax.swing.JPanel centralPanel2;
    private com.cosylab.abeans.displayers.DoubleNumberField commandAz;
    private com.cosylab.abeans.displayers.DoubleNumberField commandDec;
    private com.cosylab.abeans.displayers.DoubleNumberField commandEl;
    private javax.swing.JMenuItem commandMenuItem2;
    private javax.swing.JPanel commandPanel2;
    private com.cosylab.abeans.displayers.DoubleNumberField commandRA;
    private javax.swing.JDialog connectAntennaDlg;
    private javax.swing.JMenuItem connectMenuItem2;
    private javax.swing.JPanel dartPanel2;
    private dartboard.DartboardBean dartboardBean1;
    private javax.swing.JPanel decPanel2;
    private com.cosylab.abeans.displayers.DoubleNumberField deviationAz;
    private com.cosylab.abeans.displayers.DoubleNumberField deviationDec;
    private com.cosylab.abeans.displayers.DoubleNumberField deviationEl;
    private com.cosylab.abeans.displayers.DoubleNumberField deviationRA;
    private javax.swing.JButton doneButton;
    private javax.swing.JPanel elPanel2;
    private javax.swing.JButton eqPatternB;
    private javax.swing.JButton eqSourceB;
    private javax.swing.JPanel equatorCoordsPanel2;
    private javax.swing.JPanel headerPanel2;
    private javax.swing.JPanel horizCoordsPanel2;
    private javax.swing.JButton hrPatternB;
    private javax.swing.JButton hrSourceB;
    private javax.swing.JToggleButton inactiveB;
    private javax.swing.JPanel jInternalFrame3;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JMenuItem jLogMenuItem2;
    private javax.swing.JMenuBar jMenuBar3;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSeparator jSeparator3;
    private javax.swing.JTabbedPane jTabPane2;
    private javax.swing.JTextField jTextField3;
    private javax.swing.ButtonGroup modeButtonGroup;
    private alma.MOUNT_ACS.abeans.Mount mountAnt;
    private javax.swing.JMenuItem objectMenuItem2;
    private javax.swing.JMenuItem parkMenuItem2;
    private javax.swing.JButton pointingB;
    private javax.swing.JMenu presetMenu2;
    private javax.swing.JPanel raPanel2;
    private com.cosylab.abeans.displayers.DoubleNumberField referenceAz;
    private com.cosylab.abeans.displayers.DoubleNumberField referenceDec;
    private com.cosylab.abeans.displayers.DoubleNumberField referenceEl;
    private com.cosylab.abeans.displayers.DoubleNumberField referenceRA;
    private javax.swing.JToggleButton shutdownB;
    private com.cosylab.abeans.SingleAbeanSelector singleAbeanSelector1;
    private javax.swing.JPanel southPanel2;
    private javax.swing.JToggleButton standbyB;
    private javax.swing.JMenu toolsMenu2;
    private javax.swing.JToggleButton trackB;
    private javax.swing.JMenuItem zenithMenuItem2;
    private javax.swing.JMenu optionMenu;
    private JRadioButtonMenuItem degreesMI;
    private JRadioButtonMenuItem radiansMI;
    private javax.swing.ButtonGroup unitsBG;
    
    
    private DoublePropertyAdapter azimuthAdapter;
    private DoublePropertyAdapter elevationAdapter;
    
    private LabelDisplayer azimuthLabel;
    private LabelDisplayer elevationLabel;
}
