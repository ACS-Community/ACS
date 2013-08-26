/*
 * Created on Apr 6, 2004 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.awt.BorderLayout;

import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.SpringLayout;
import javax.swing.border.EmptyBorder;

import alma.acs.commandcenter.gui.thirdparty.SpringUtilities;



abstract class ManagerLocationPanel extends JPanel {

	protected JRadioButton btnLatest, btnCustom;
	protected JTextField defaultHostF, defaultPortF;
	protected JTextField customHostF, customPortF;
	protected ButtonGroup buttonGroup;
	protected JLabel north;
	protected JPanel center; 

	
	protected ManagerLocationPanel(String explanationText) {
		this.setLayout(new BorderLayout());

		north = new JLabel(explanationText);
		this.add(north, BorderLayout.NORTH);
		
		center = new JPanel();
		center.setBorder(new EmptyBorder(10, 10, 10, 10));
		center.setLayout(new SpringLayout());
		
		center.add(btnLatest = new JRadioButton("Use Common settings"));
		center.add(new JPanel()); // placeholder
		center.add(new JLabel("Manager Host"));
		center.add(defaultHostF = new JTextField());
		center.add(new JLabel("Manager Port"));
		center.add(defaultPortF = new JTextField());
		
		defaultHostF.setEditable(false);
		defaultPortF.setEditable(false);
		
		center.add(btnCustom = new JRadioButton("Use Custom settings"));
		center.add(new JPanel()); // placeholder
		center.add(new JLabel("Manager Host"));
		center.add(customHostF = new JTextField());
		center.add(new JLabel("Manager Port"));
		center.add(customPortF = new JTextField());

		SpringUtilities.makeCompactGrid(center, 0, 2);
		
		buttonGroup = new ButtonGroup();
		buttonGroup.add(btnLatest);
		buttonGroup.add(btnCustom);
		btnLatest.setSelected(true);
		
		this.add(center, BorderLayout.CENTER);
		
		
		btnLatest.setName("btn_Latest");
		btnCustom.setName("btn_Custom");
		defaultHostF.setName("txt_DefaultHost");
		defaultPortF.setName("txt_DefaultPort");
		customHostF.setName("txt_CustomHost");
		customPortF.setName("txt_CustomPort");
	}


	static class ForContainers extends ManagerLocationPanel {

		protected JTextField defaultCdbF, defaultIntRepF;
		protected JTextField customCdbF, customIntRepF;

		protected ForContainers() {
			super("Choose manager and services to run the containers against:                       ");

			int insert = 6;
			center.add(new JLabel("CDB (cpp only)"), insert++);
			center.add(defaultCdbF = new JTextField(), insert++);
			
			center.add(new JLabel("Int.Rep. (cpp only)"), insert++);
			center.add(defaultIntRepF = new JTextField(""), insert++);

			defaultCdbF.setEditable(false);
			defaultIntRepF.setEditable(false);
			
			
			center.add(new JLabel("CDB (cpp only)"));
			center.add(customCdbF = new JTextField());
			
			center.add(new JLabel("Int.Rep. (cpp only)"));
			center.add(customIntRepF = new JTextField(""));
			
			SpringUtilities.makeCompactGrid(center, 0, 2);
			
			
			defaultCdbF.setName("txt_DefaultCdb");
			defaultIntRepF.setName("txt_DefaultIntRep");
			customCdbF.setName("txt_CustomCdb");
			customIntRepF.setName("txt_CustomIntRep");
		}
	}

	static class ForTools extends ManagerLocationPanel {

		protected JTextField defaultIntRepF, defaultNameServiceF;
		protected JTextField customIntRepF, customNameServiceF;
		
		protected ForTools() {
			super("Choose manager and services to run the tools against:                          ");

			int insert = 6;
			center.add(new JLabel("Interface Repository"), insert++);
			center.add(defaultIntRepF = new JTextField(), insert++);
			
			center.add(new JLabel("Name Service"), insert++);
			center.add(defaultNameServiceF = new JTextField(), insert++);

			defaultNameServiceF.setEditable(false);
			defaultIntRepF.setEditable(false);
			
			
			center.add(new JLabel("Interface Repository corbaloc"));
			center.add(customIntRepF = new JTextField());
			
			center.add(new JLabel("Name Service corbaloc"));
			center.add(customNameServiceF = new JTextField());

			SpringUtilities.makeCompactGrid(center, 0, 2);
			
			
			defaultIntRepF.setName("txt_DefaultIntRep");
			defaultNameServiceF.setName("txt_DefaultNameService");
			customIntRepF.setName("txt_CustomIntRep");
			customNameServiceF.setName("txt_CustomNameService");
		}
	}

}
////////////////////////////////////////////////////////
/// ------------------- API ------------------------ ///
////////////////////////////////////////////////////////

////////////////////////////////////////////////////////
/// ----------------- Internal --------------------- ///
////////////////////////////////////////////////////////

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