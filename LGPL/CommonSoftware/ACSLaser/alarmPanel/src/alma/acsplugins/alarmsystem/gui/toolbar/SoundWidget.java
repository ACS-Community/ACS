/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acsplugins.alarmsystem.gui.toolbar;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JToggleButton;
import javax.swing.SwingUtilities;

import alma.acsplugins.alarmsystem.gui.sound.AlarmSound;
import alma.acsplugins.alarmsystem.gui.sound.AlarmSoundListener;

/**
 * This class encapsulates the sound button of the toolbar
 * 
 * @author acaproni
 * @since ACS 8.1.0
 */
public class SoundWidget extends JToggleButton 
implements ActionListener, AlarmSoundListener {
	/**
	 * The object to play audibles for alarms
	 * <P>
	 * This class checks the sound level to enable/disable
	 * the popup menu item to inhibit sounds
	 */
	private final AlarmSound alarmSound;
	
	/**
	 * The icon shown in the button when the sound is selected
	 */
	private static final ImageIcon soundSelIcon = new ImageIcon(SoundWidget.class.getResource("/alma/acsplugins/alarmsystem/gui/resources/sound.png"));
	
	/**
	 * The icon shown in the button when the sound is selected
	 */
	private static final ImageIcon soundUnselIcon = new ImageIcon(SoundWidget.class.getResource("/alma/acsplugins/alarmsystem/gui/resources/sound_mute.png"));
	
	/**
	 * The icon shown in the button when a sound is palying
	 */
	private static final ImageIcon soundPlayIcon = new ImageIcon(SoundWidget.class.getResource("/alma/acsplugins/alarmsystem/gui/resources/sound_playing.png"));
	
	public SoundWidget(AlarmSound alarmSound) {
		super(soundSelIcon,false);
		if (alarmSound==null) {
			throw new IllegalArgumentException("The AlarmSound can't be null");
		}
		this.alarmSound=alarmSound;
		this.alarmSound.addSoundListener(this);
		addActionListener(this);
		// NOTE: the folowing lines disable the 
		//		 the sound button in order to inhibit
		//		 the panel to sound for low priority alarms (priorities 2-3)
		//
		// TODO: re-enable the sound for lowest priority alarms when
		//		 the alarm system is better configured and tuned
		setSelected(true);
		setEnabled(false);
	}
	
	/**
	 * @see AlarmSoundListener
	 */
	@Override
	public void played() {
		SwingUtilities.invokeLater(new Runnable(){
			public void run() {
				setEnabled(true);
				toggleSoundButton();
			}
		});
	}

	/**
	 * @see AlarmSoundListener
	 */
	@Override
	public void playing(int priority) {
		SwingUtilities.invokeLater(new Runnable(){
			public void run() {
				setIcon(soundPlayIcon);
				setToolTipText("Playing...");
				setEnabled(false);
			}
		});
	}

	/**
	 * @see AlarmSoundListener
	 */
	@Override
	public void reset() {
		SwingUtilities.invokeLater(new Runnable(){
			public void run() {
				setSelected(false);
				toggleSoundButton();
			}
		});
	}
	
	/**
	 * set the icon of the sound button depending on the state
	 * of the button
	 */
	private void toggleSoundButton() {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				setToolTipText("Enable/disable audibles");
				if (isSelected()) {
					// Disable the sound
					alarmSound.inhibit(1);
					setIcon(soundUnselIcon);
				} else {
					// Enable the sound
					alarmSound.inhibit(3);
					setIcon(soundSelIcon);
				}
			}
		});
	}

	/**
	 * @see ActionListener
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==this) {
			toggleSoundButton();
		} else {
			System.err.println("Unknown source of events "+e.getSource());
		}
	}
}
