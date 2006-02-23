/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs.maci.settings;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.URL;

import javax.swing.ImageIcon;
import javax.swing.JApplet;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

import abeans.pluggable.acs.maci.ACSPlug;

import com.cosylab.abeans.AbeansEngine;
import com.cosylab.abeans.AbeansLaunchable;
import com.cosylab.gui.framework.Desktop;
import com.cosylab.gui.framework.Launcher;
import com.cosylab.gui.framework.LauncherEnvironment;

/**
 * ACS Plug Settings application.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
// TODO currently multiple instantiations are allowed
public class ACSPlugSettings extends AbeansLaunchable {

	/**
	 * Application engine.
	 */
	private ACSPlugSettingsEngine engine;

	/**
	 * ACS Plug to be managed.
	 */
	private ACSPlug plug;
	
	/**
	 * Status label.
	 */
	private JLabel statusLabel;
	
	/**
	 * Connect button.
	 */
	private JButton connectButton;
	
	/**
	 * Disconnect button.
	 */
	private JButton disconnectButton;

	/**
	 * Updater remote directory button.
	 */
	private JButton updateRemoteDirectoryButton;

	/**
	 * Manager reference label.
	 */
	private JLabel label;

	/**
	 * Manager reference text field.
	 */
	private JTextField textField;
	
	/**
	 * 
	 */
	public ACSPlugSettings() {
		super();
	}

	/**
	 * @param arg0
	 * @param arg1
	 * @param arg2
	 */
	public ACSPlugSettings(
		Launcher arg0,
		LauncherEnvironment arg1,
		JFrame arg2) {
		super(arg0, arg1, arg2);
	}

	/**
	 * @param arg0
	 * @param arg1
	 * @param arg2
	 * @param arg3
	 */
	public ACSPlugSettings(
		Launcher arg0,
		LauncherEnvironment arg1,
		Desktop arg2,
		JInternalFrame arg3) {
		super(arg0, arg1, arg2, arg3);
	}

	/**
	 * @param arg0
	 * @param arg1
	 * @param arg2
	 */
	public ACSPlugSettings(
		Launcher arg0,
		LauncherEnvironment arg1,
		JApplet arg2) {
		super(arg0, arg1, arg2);
	}

	/**
	 * @see com.cosylab.abeans.AbeansLaunchable#getAbeansEngine()
	 */
	public AbeansEngine getAbeansEngine()
	{
		if (engine == null)
			engine = new ACSPlugSettingsEngine();
		return engine;
	}

	/**
	 * @see com.cosylab.gui.core.CosyPanel#userInitializeGUI()
	 */
	public void userInitializeGUI() {
		setTitle("ACS Plug Settings");

		plug = ((ACSPlugSettingsEngine)getAbeansEngine()).getPlug();
		if (plug == null)
		{
			setLayout(new BorderLayout());
			setSize(300, 100);

			JLabel label = new JLabel("No ACS Plug installed.", SwingConstants.CENTER);
			add(label); 
		}
		else
		{
			setLayout(new BorderLayout());
			setSize(500, 200);

			add(createTabbedPane());
		}
	}


	/**
	 * Get tabbed panel.
	 * @return tabbed panel.
	 */
	private JTabbedPane createTabbedPane()
	{
		JTabbedPane tabbedPane = new JTabbedPane();
		URL preferencesIcon = this.getClass().getClassLoader().getResource("Resources/icons/general/Preferences16.gif");
		tabbedPane.addTab("Managers settings", new ImageIcon(preferencesIcon), createManagerPanel());
		return tabbedPane;
	}

	/**
	 * Update status label.
	 */
	private void updateStatus()
	{
		boolean isConnected = plug.isConnected();
		if (isConnected)
		{
			// update only when connected
			textField.setText(plug.getDefaultManagerReference());

			statusLabel.setForeground(Color.BLACK);
			statusLabel.setText("Connected to '" + plug.getDefaultManagerReference() + "'.");
			
			connectButton.setEnabled(false);
			disconnectButton.setEnabled(true);
			updateRemoteDirectoryButton.setEnabled(true);
			label.setEnabled(false);
			textField.setEnabled(false);
		}
		else
		{
			statusLabel.setForeground(Color.RED);
			statusLabel.setText("Disconnected.");
			
			connectButton.setEnabled(true);
			disconnectButton.setEnabled(false);
			updateRemoteDirectoryButton.setEnabled(false);
			label.setEnabled(true);
			textField.setEnabled(true);
		}
	}

	/**
	 * Manager panel.
	 * @return Manager panel.
	 */
	private JPanel createManagerPanel()
	{
		JPanel panel = new JPanel();
		panel.setLayout(new GridBagLayout());

		// add status label
		GridBagConstraints statusLabelConstraints = new GridBagConstraints();
		statusLabelConstraints.gridx = 0;
		statusLabelConstraints.gridy = 0;
		statusLabelConstraints.gridwidth = 2;
		statusLabelConstraints.insets = new Insets(12, 24, 4, 12);
		statusLabelConstraints.fill = GridBagConstraints.HORIZONTAL;
		statusLabelConstraints.weightx = 1.0;
		statusLabelConstraints.weighty = 0.0;
		statusLabel = new JLabel("<unknown status>", SwingConstants.CENTER);
		panel.add(statusLabel, statusLabelConstraints);

		// add separator
		GridBagConstraints separatorConstraints = new GridBagConstraints();
		separatorConstraints.gridx = 0;
		separatorConstraints.gridy = 1;
		separatorConstraints.gridwidth = 2;
		separatorConstraints.insets = new Insets(12, 24, 12, 24);
		separatorConstraints.fill = GridBagConstraints.HORIZONTAL;
		separatorConstraints.weightx = 1.0;
		separatorConstraints.weighty = 0.0;
		JSeparator separator = new JSeparator(SwingConstants.HORIZONTAL);
		panel.add(separator, separatorConstraints);

		// add manager reference label
		GridBagConstraints labelConstraints = new GridBagConstraints();
		labelConstraints.gridx = 0;
		labelConstraints.gridy = 2;
		labelConstraints.insets = new Insets(4, 24, 4, 8);
		labelConstraints.fill = GridBagConstraints.HORIZONTAL;
		labelConstraints.weightx = 0.0;
		labelConstraints.weighty = 0.0;
		label = new JLabel("Manager reference:", SwingConstants.RIGHT);
		panel.add(label, labelConstraints);

		// add manager reference text field
		GridBagConstraints textFieldConstraints = new GridBagConstraints();
		textFieldConstraints.gridx = 1;
		textFieldConstraints.gridy = 2;
		textFieldConstraints.insets = new Insets(4, 4, 4, 24);
		textFieldConstraints.fill = GridBagConstraints.HORIZONTAL;
		textFieldConstraints.weightx = 1.0;
		textFieldConstraints.weighty = 0.0;
		textField = new JTextField();
		textField.setText(plug.getDefaultManagerReference());
		panel.add(textField, textFieldConstraints);

		// add button panel
		GridBagConstraints buttonPanelConstraints = new GridBagConstraints();
		buttonPanelConstraints.gridx = 0;
		buttonPanelConstraints.gridy = 3;
		buttonPanelConstraints.gridwidth = 3;
		buttonPanelConstraints.insets = new Insets(12, 24, 12, 24);
		buttonPanelConstraints.fill = GridBagConstraints.NONE;
		buttonPanelConstraints.weightx = 1.0;
		buttonPanelConstraints.weighty = 0.0;
		JPanel buttonPanel = new JPanel();
		panel.add(buttonPanel, buttonPanelConstraints);

		// add connect button
		connectButton = new JButton("Connect");
		connectButton.addActionListener(
			new ActionListener()
			{
				public void actionPerformed(ActionEvent e)
				{
					plug.managerConnect(textField.getText());
				}
			}
		);
		buttonPanel.add(connectButton);
		
		// add disconnect button
		disconnectButton = new JButton("Disconnect");
		disconnectButton.addActionListener(
			new ActionListener()
			{
				public void actionPerformed(ActionEvent e)
				{
					plug.managerDisconnect();
				}
			}
		);
		buttonPanel.add(disconnectButton);
		
		// add update remote directory button
		updateRemoteDirectoryButton = new JButton("Update Remote Directory");
		updateRemoteDirectoryButton.addActionListener(
			new ActionListener()
			{
				public void actionPerformed(ActionEvent e)
				{
					plug.updateRemoteDirectory();
				}
			}
		);
		buttonPanel.add(updateRemoteDirectoryButton);

		// update status label
		new Thread(
			new Runnable()
			{
				public void run()
				{
					while (!isDestroyed())
					{
						updateStatus();
						try
						{
							Thread.sleep(1000);
						}
						catch (InterruptedException ie) {}
					}
				}
			}, "ACS Plug Settings Update Status Thread"
		).start();

		return panel;
	}
}
