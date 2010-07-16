package cl.utfsm.samplingSystemUI;

import javax.swing.JLabel;

import java.awt.Color;
import java.awt.Font;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

public class BeanMemoryWidget extends SamplingWidget {

	private static final long serialVersionUID = 4823621192367385632L;
	private JLabel componentLabel = null;
	private JLabel propertyLabel = null;
	private JLabel SamplingSizeLabel = null;
	
	private long samples=0;

	public BeanMemoryWidget() {
		super();
		initialize();
	}

	/**
	 * This method initializes this
	 * 
	 */
	private void initialize() {
        SamplingSizeLabel = new JLabel();
        SamplingSizeLabel.setFont(new Font("Dialog", Font.PLAIN, 10));
        SamplingSizeLabel.setText("Sampling size: 0");
        SamplingSizeLabel.setPreferredSize(new Dimension(180, 20));
        propertyLabel = new JLabel();
        propertyLabel.setFont(new Font("Dialog", Font.PLAIN, 10));
        propertyLabel.setText("JLabel");
        propertyLabel.setPreferredSize(new Dimension(70, 20));
        componentLabel = new JLabel();
        componentLabel.setFont(new Font("Dialog", Font.PLAIN, 10));
        componentLabel.setText("JLabel");
        componentLabel.setPreferredSize(new Dimension(90, 20));
		this.setLayout(new GridBagLayout());
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.anchor = GridBagConstraints.WEST;
		gbc.ipadx = 20;
		gbc.weightx = 1;
        this.add(componentLabel, gbc);
        this.add(propertyLabel, gbc);
        this.add(SamplingSizeLabel, gbc);
			
	}

	public void updateValues(long time, double value, int position) {
		samples++;
		if(samples%100==0)
			SamplingSizeLabel.setText("Sampling size: "+ samples+"+");

	}

	public void setValues(String component, String property, int position) {
		componentLabel.setText(component);
		propertyLabel.setText(property);
	}

	public void setComponentAvailable(boolean tmp, String reason, int position) {
		if( tmp == true ) {
			componentLabel.setForeground(Color.BLACK);
			propertyLabel.setForeground(Color.BLACK);
			SamplingSizeLabel.setForeground(Color.BLACK);
			SamplingSizeLabel.setText("Sampling size:");
		}
		else {
			componentLabel.setForeground(Color.RED);
			propertyLabel.setForeground(Color.RED);
			SamplingSizeLabel.setForeground(Color.RED);
			SamplingSizeLabel.setText("(" + reason + ")");
		}
		
		componentLabel.repaint();
	}
	
	public void resetSampleCount() {
		samples = 0;
		SamplingSizeLabel.setText("Sampling size: "+ samples);
	}


	public void setTimeWindow(double frequency, int time) {
		System.out.println("NOT IMPLEMENTED");
		// TODO Auto-generated method stub
		
	}
}