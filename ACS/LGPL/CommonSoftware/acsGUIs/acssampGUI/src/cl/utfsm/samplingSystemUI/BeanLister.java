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
package cl.utfsm.samplingSystemUI;

import javax.swing.JLabel;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

public class BeanLister extends SamplingWidget{

	private static final long serialVersionUID = 1L;
	private JLabel jLabel1 = null;
	private JLabel jLabel2 = null;
	private JLabel timeLabel = null;
	private JLabel jLabel4 = null;
	/**
	 * This is the default constructor
	 */
	public BeanLister() {
		super();
		initialize();
	}

	/**
	 * This method initializes this
	 * 
	 * @return void
	 */
	private void initialize() {
		jLabel4 = new JLabel();
		jLabel4.setText("JLabel");
		jLabel4.setFont(new Font("Dialog", Font.PLAIN, 10));
		jLabel4.setPreferredSize(new Dimension(75, 20));
		timeLabel = new JLabel();
		timeLabel.setText("JLabel");
		timeLabel.setFont(new Font("Dialog", Font.PLAIN, 10));
		timeLabel.setPreferredSize(new Dimension(125, 20));
		jLabel2 = new JLabel();
		jLabel2.setFont(new Font("Dialog", Font.PLAIN, 10));
		jLabel2.setText("JLabel");
		jLabel2.setPreferredSize(new Dimension(75, 20));
		jLabel1 = new JLabel();
		jLabel1.setFont(new Font("Dialog", Font.PLAIN, 10));
		jLabel1.setText("JLabel");
		jLabel1.setPreferredSize(new Dimension(50, 20));
		this.setLayout(new GridBagLayout());
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.anchor = GridBagConstraints.WEST;
		gbc.ipadx = 20;
		gbc.weightx = 1;
		this.add(jLabel1,gbc);
		this.add(jLabel2,gbc);
		this.add(timeLabel,gbc);
		this.add(jLabel4,gbc);
	}

	public void updateValues(long time, double value, int position) {
		timeLabel.setText(Long.toString(time));
		jLabel4.setText(Double.toString(value));
	}
	
	public void setValues(String component, String property, int position){
		jLabel1.setText(component);
		jLabel2.setText(property);
		timeLabel.setText("0"); 
		jLabel4.setText("0");
	}

	
	public void setComponentAvailable(boolean tmp, String reason, int position) {
		if( tmp == false ) {
			jLabel1.setForeground(Color.RED);
			jLabel2.setForeground(Color.RED);
			timeLabel.setForeground(Color.RED);
			timeLabel.setText("(" + reason + ")");
			jLabel4.setForeground(Color.RED);
			jLabel4.setText("");
		}
		else {
			jLabel1.setForeground(Color.BLACK);
			jLabel2.setForeground(Color.BLACK);
			jLabel4.setForeground(Color.BLACK);
			timeLabel.setForeground(Color.BLACK);
		}	
	}

	public void resetSampleCount() {
		
	}

	public void setTimeWindow(double frequency, int time) {
		
	}
}
