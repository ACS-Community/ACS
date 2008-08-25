/**
 * @author Alejandro Baltra <abaltra@alumnos.inf.utfsm.cl>
 * @author Rodrigo Tobar <rtobar@inf.utfsm.cl>
 * @author Jorge Avarias <javarias@inf.utfsm.cl>
 */

package cl.utfsm.samplingSystemUI;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Iterator;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;


/**
 * Class that works as a displayable container of sampling items. It has an internal state to check
 * if it's selected to be sampled (a checkbox) and N samples, wich will start parallel to one another 
 * once the "Start Sample(s)" button from the GUI is pressed 
 */
public class BeanGrouper extends JPanel {

	private static final long serialVersionUID = 1L;
	private JLabel jCheckBox = null;
	private JLabel jLabel = null;
	private JPanel jPanel = null;
	private JButton jStopButton = null;
	private JButton jCloseButton = null;
	private JLabel frecuencyLabel = null;
	private JTextField freqTextField = null;
	private JLabel timeSampLabel = null;
	private JTextField timeSampTextField = null;
	private ArrayList<DataPrinter> samplers = null;
	private boolean ready2samp = false;
	private JButton startSampleButton = null;
	private FileHelper toFile;
	private boolean isStopped=true;
	private SamplingSystemGUI ssg = null;
	
	/**
	 * This is the default constructor
	 */
	public BeanGrouper(SamplingSystemGUI ssg) {
		super();
		this.ssg = ssg;
		toFile=new FileHelper();
		initialize();
	}

	/**
	 * This method initializes this
	 * 
	 * @return void
	 */
	private void initialize() {
		this.setBorder(javax.swing.border.LineBorder.createBlackLineBorder());
		this.setLayout(new GridBagLayout());
		
		GridBagConstraints gridBagConstraints = new GridBagConstraints();
		Insets insets = new Insets(5,5,5,5);
		
		gridBagConstraints.gridy = 0;
		gridBagConstraints.gridx = 0;
		gridBagConstraints.weightx = 1;
		gridBagConstraints.anchor = GridBagConstraints.WEST;
		this.add(getJCheckBox(), gridBagConstraints);
		
		gridBagConstraints.anchor = GridBagConstraints.EAST;
		gridBagConstraints.gridx = 1;
		gridBagConstraints.insets = insets;
		this.add(getStartSampleButton(), gridBagConstraints);
		gridBagConstraints.gridx = 2;
		this.add(getStopButton(), gridBagConstraints);
		gridBagConstraints.gridx = 3;
		this.add(getCloseSamplingButton(), gridBagConstraints);
		gridBagConstraints.gridy=1;
		gridBagConstraints.gridx=0;
		this.add(getFrequencyLabel(),gridBagConstraints);
		gridBagConstraints.gridx=1;
		gridBagConstraints.anchor = GridBagConstraints.WEST;
		this.add(getFreqTextField(),gridBagConstraints);
		gridBagConstraints.gridx=2;
		this.add(getTimeSampLabel(),gridBagConstraints);
		gridBagConstraints.gridx=3;
		this.add(getTimeSampTextField(), gridBagConstraints);
		gridBagConstraints.gridy=4;
		gridBagConstraints.gridx=0;
		gridBagConstraints.gridwidth=3;
		this.add(getjLabel(),gridBagConstraints);
		gridBagConstraints.anchor = GridBagConstraints.EAST;
		gridBagConstraints.gridy = 2;
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridwidth = 3;
		gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		this.add(getJPanel(), gridBagConstraints);

		this.getStopButton().setEnabled(false);
		
		samplers = new ArrayList<DataPrinter>();
	}

	private JButton getCloseSamplingButton(){
		if(jCloseButton==null){
			jCloseButton=new JButton();
			jCloseButton.setText("X");
			jCloseButton.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					if(!isStopped)
						stopSample();
					ssg.deleteFromStatus(samplers,getCheckName());
					Container dad = BeanGrouper.this.getParent();
					dad.remove(BeanGrouper.this);
					dad.repaint();
					dad.validate();
				}
			});
		}
		return jCloseButton;
	}
	
	private JTextField getTimeSampTextField() {
		if(timeSampTextField == null){
			timeSampTextField = new JTextField();
			timeSampTextField.setText("0");
			timeSampTextField.setPreferredSize(new Dimension(50,19));
		}
		return timeSampTextField;
	}

	private JLabel getTimeSampLabel() {
		if(timeSampLabel==null){
			timeSampLabel=new JLabel();
			timeSampLabel.setText("Sampling time (mins):");
		}
		return timeSampLabel;
	}

	private JLabel getjLabel(){
		if(jLabel == null){
			jLabel = new JLabel();
			jLabel.setText("Sampling to:");
			//jLabel.setText(toFile.getFileName());
		}
		return jLabel;
	}
	
	private void setjLabel(String text){
		jLabel.setText(text);
	}
	
	private JTextField getFreqTextField() {
		if(freqTextField==null){
			freqTextField=new JTextField();
			freqTextField.setText("100");
			freqTextField.setPreferredSize(new Dimension(50, 19));
		}
		return freqTextField;
	}

	private JLabel getFrequencyLabel() {
		if(frecuencyLabel==null){
			frecuencyLabel=new JLabel();
			frecuencyLabel.setText("Frequency (Hz): ");
		}
		return frecuencyLabel;
	}

	/**
	 * This method initializes jCheckBox	
	 * 	
	 * @return javax.swing.JCheckBox	
	 */
	private JLabel getJCheckBox() {
		if (jCheckBox == null) {
			jCheckBox = new JLabel();
			jCheckBox.setText("  SampGroup");
			/*jCheckBox.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					if(ready2samp)ready2samp = false;
					else ready2samp = true;
				}
			});*/
		}
		return jCheckBox;
	}

	private JButton getStopButton() {
		
		if (jStopButton == null) {
			jStopButton = new JButton("Stop sampling");
			jStopButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
					jCloseButton.setEnabled(true);
					stopSample();
					jStopButton.setEnabled(false);
				}
			});
		}
		
		return jStopButton;
	}
	
	/**
	 * This method initializes jPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel() {
		if (jPanel == null) {
			jPanel = new JPanel();
			jPanel.setLayout(new GridBagLayout());
		}
		return jPanel;
	}

	/**
	 * This method adds a new sample (represented as a BeanListe) into this class. This class should be able to hold
	 * N samples inside itself.
	 * @param w
	 */

	public void addSamp(DataPrinter w){
		GridBagConstraints tmp = new GridBagConstraints();
		tmp.anchor = GridBagConstraints.NORTHWEST;
		tmp.fill = GridBagConstraints.HORIZONTAL;
		tmp.gridy = jPanel.getComponentCount();
		tmp.weightx = 1;
		jPanel.add((JPanel)w.getWidget(),tmp);
		updateLabel();
		jPanel.validate();
		samplers.add(w);
	}
	
	public void updateLabel(){
	}
	
	/**
	 * Method to begin the sampling of all the samples contained in the class.
	 *
	 */
	private void startSample(){

		int freq=100;
		setjLabel("Sampling to file: " + toFile.getFileName());
		try{
			freq = Integer.parseInt(getFreqTextField().getText());
		}catch(NumberFormatException ex){
			getFreqTextField().setText("100");
		}
		for(DataPrinter wp : samplers){
			wp.setFrecuency(freq);
			try {
				wp.startSample();
				isStopped = false;
			} catch(alma.ACSErrTypeCommon.CouldntAccessComponentEx e) {
				wp.setComponentAvailable(false,"Cannot access component");
			} catch(alma.ACSErrTypeCommon.TypeNotSupportedEx e) {
				wp.setComponentAvailable(false,"Type not supported");
			}
		}

		if( isStopped ) {
			jCloseButton.setEnabled(true);
			jStopButton.setEnabled(false);
			startSampleButton.setEnabled(true);
			return;
		}
		
		int mins=0;
		try{
			mins = Integer.parseInt(getTimeSampTextField().getText());
		}catch(NumberFormatException ex){
			getTimeSampTextField().setText("0");
		}
		
		if(mins!=0)
			new Watchdog(mins).start();
	}

	/**
	 * Getter for a boolean status check.
	 * @return True if the Group is selected for sampling, false in the other case
	 */
	public boolean Ready2Samp(){
		return ready2samp;
	}
	
	/**
	 * Generic setter for the Group name
	 * @param name Name of the group
	 */
	public void setCheckName(String name){
		jCheckBox.setText(name);
	}
	
	/**
	 * Generic getter for the group name
	 * @return The name of group.
	 */
	public String getCheckName(){
		return jCheckBox.getText();
	}

	/**
	 * This method initializes startSampleButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getStartSampleButton() {
		if (startSampleButton == null) {
			startSampleButton = new JButton();
			startSampleButton.setText("Start sampling");
			startSampleButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					jCloseButton.setEnabled(false);
					jStopButton.setEnabled(true);
					startSampleButton.setEnabled(false);
					getFreqTextField().setEnabled(false);
					getTimeSampTextField().setEnabled(false);
					toFile.initialize(Integer.parseInt(getFreqTextField().getText()));
					startSample();
				}
			});
		}
		return startSampleButton;
	}

	public void addSamp(String component, String property) {
		MemoryPrinter w = new MemoryPrinter(ssg);
		w.setComponent(component);
		w.setProperty(property);
		addSamp(w);
	}
	
	public boolean checkIfExists(String component, String property) {
		
		for (Iterator iter = samplers.iterator(); iter.hasNext();) {
			DataPrinter element = (DataPrinter) iter.next();
			if( element.component == component &&
				element.property  == property )
				return true;
		}
		return false;
	}

	private void stopSample(){
		isStopped=true;
		jStopButton.setEnabled(false);
		startSampleButton.setEnabled(true);
		String header ="Time";
		for(DataPrinter i : samplers) {
			i.stopSampling();
			if( i.isComponentAvailable() == true )
				toFile.addSamplingSet(i.getSamples());
			header+=";"+i.getComponent()+"."+i.getProperty();
		}
		toFile.setHeaderFile(header);
		toFile.dumpToFile(Integer.parseInt(getFreqTextField().getText()));
	}

	class Watchdog extends Thread{
		private long sleepTime;
		
		public Watchdog(int mins){
			this.setPriority(Thread.MAX_PRIORITY);
			sleepTime=mins*60*1000;
		}
		
		public void run() {
			try {
				Thread.sleep(sleepTime);
				if(!isStopped)
					stopSample();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}
}
