/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
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
package alma.acsplugins.alarmsystem.gui.sound;

import java.io.IOException;
import java.net.URL;
import java.util.Map;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;

import javax.sound.sampled.AudioFileFormat;
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.Mixer;
import javax.sound.sampled.SourceDataLine;
import javax.sound.sampled.UnsupportedAudioFileException;

import alma.acsplugins.alarmsystem.gui.table.AlarmTableModel;

/**
 * A class to play sounds for alarms.
 * <P>
 * The class emits a sound every <code>TIME_INTERVAL</code> seconds
 * if the table contains alarms not seen by the user.
 * The sound depends on the highest priority of the alarms 
 * not seen i.e. every priority has its own specific sound.
 * <P>
 * It is possible to inhibit the sound for a given period of
 * time, <code>INHIBIT_TIME</code>.
 * After <code>INHIBIT_TIME</code> seconds, the sounds will be
 * re-enabled.
 * <P>
 * In case of error playing a sound, objects of this class do
 * nothing because we do not want to trigger a failure in the 
 * application neither to present a dialog to the operator. 
 * Apart of that, if the audio system is in use by another 
 * application then it might happen that it is not possible
 * to reserve a new line and play the sound.;
 * <P>
 * <I>Note</I>:
 * after testing in tf-ohg I  have found that the STE sound environment 
 * is quite problematic. for example in my account there were 4 mixers 
 * but I have tried another one that add only one and in that case it was 
 * not possible to find a line to play the sound.
 * With my account, even with 4 mixers available the process was able
 * to play the sound but not in the speakers.
 * <BR>
 * To limit the risk that the sound is played in the wrong device or
 * that the line obtained by default is not available because locked
 * by another application and so n, I decided to play the sound 
 * in all available devices with the risk to play it twice.
 *  
 * @author acaproni
 * @since ACS 8.1.0
 */
public class AlarmSound extends TimerTask {
	
	/**
	 * The table model
	 */
	private final AlarmTableModel tableModel;
	
	/**
	 * The size of the buffer
	 */
	private final int EXTERNAL_BUFFER_SIZE = 524288; // 128Kb
	
	/**
	 * A sound is emitted every TIME_INTERVAL seconds
	 */
	private static final int TIME_INTERVAL=60;
	
	/**
	 * Inhibit the sound for the given amount of seconds.
	 * <P>
	 * The <code>INHIBIT_TIME</code> is a multiple of 
	 * <code>TIME_INTERVAL</code>.
	 */
	private static final int INHIBIT_TIME=15;
	
	/**
	 * Objects of this class plays a sound when the priority of
	 * an unseen alarm is equal or less then soundLevel appears in 
	 * the table.
	 * <P>
	 * It is not possible to inhibit alarms having priority 0 or 1
	 * therefore this property can only have values in range [1-3]
	 */
	private int soundLevel=3;
	
	/**
	 * The counter to re-enable the level
	 * <P>
	 * It is reset to 0 when a new level is selected or after
	 * <code>INHIBIT_CONTER</code> seconds.
	 */
	private int inhibitCounter=0;
	
	/**
	 * The timer to emit the sound
	 */
	private final Timer timer = new Timer("AlarmSound",true);
	
	/**
	 * <code>true</code> if the object has been close.
	 */
	private boolean closed=false;
	
	/**
	 * A listener for sound events.
	 * <P>
	 * This class supports only one listener; if more then
	 * one listener is added then only the last one will be notified.
	 */
	private volatile AlarmSoundListener listener=null;
	
	/**
	 * The URLs of the sounds to play.
	 * <P>
	 * The index of the array correspond to the level of the
	 * alarm i.e. for alarms with level=0 the sound to
	 * play is <code>soundURLs[0]</code>.
	 * 
	 */
	private static final URL[] soundURLs = {
		AlarmSound.class.getResource("/alma/acsplugins/alarmsystem/gui/sound/resources/level0.wav"),
		AlarmSound.class.getResource("/alma/acsplugins/alarmsystem/gui/sound/resources/level1.wav"),
		AlarmSound.class.getResource("/alma/acsplugins/alarmsystem/gui/sound/resources/level2.wav"),
		AlarmSound.class.getResource("/alma/acsplugins/alarmsystem/gui/sound/resources/level3.wav")
	};
	
	/**
	 * Constructor
	 * 
	 * @param model The table model 
	 */
	public AlarmSound(AlarmTableModel model) {
		if (model==null) {
			throw new IllegalArgumentException("The AlarmTableModel can't be null");
		}
		tableModel=model;
		// Start the timer
		timer.schedule(this, TIME_INTERVAL*1000, TIME_INTERVAL*1000);
		//dumpAudioInformation();
	}
	
	/**
	 * Play the sound for the given priority
	 * 
	 * @param priority The priority of the alarm
	 */
	private void play(int priority) throws Exception {
		
		if (priority<0 || priority>3) {
			throw new IllegalStateException("Invalid alarm priority "+priority);
		}
		URL url=soundURLs[priority];
	
		AudioInputStream audioInputStream=null;
		try {
			audioInputStream=AudioSystem.getAudioInputStream(url);
		} catch (Throwable t) {
			// If there is an error then the panel does nothing
			// It might happen for example if another application
			// is locking the audio.
			System.err.println(t.getMessage());
			t.printStackTrace();
			return;
		}
		
		// Obtain the information about the AudioInputStream
		AudioFormat audioFormat = audioInputStream.getFormat();
		SourceDataLine line=null;
		DataLine.Info info = new DataLine.Info(SourceDataLine.class, audioFormat);
		
		// Get the list of available mixers
		Mixer.Info[] mixersInfo = AudioSystem.getMixerInfo();
		// Try to get and open a line in one of the mixers until 
		// one is available is found
		for (int i=0; i<mixersInfo.length && line==null; i++) {
			Mixer.Info mi=mixersInfo[i];
			try {
				Mixer mixer = AudioSystem.getMixer(mi);
				line=(SourceDataLine)mixer.getLine(info);
			} catch (LineUnavailableException lue) {
				System.err.println("Line unavailable "+lue.getMessage());
				line=null;
				continue;
			} catch (Throwable t) {
				System.err.println("Exception getting the line "+t.getMessage());
				line=null;
				continue;
			}
			try {
				line.open(audioFormat, EXTERNAL_BUFFER_SIZE);
			} catch (Throwable t) {
				System.err.println("Error opeining the line: "+t.getMessage());
				line=null;
				continue;
			}
			try {
				line.start();
			} catch (Throwable t) {
				System.err.println("Error starting the line: "+t.getMessage());
				line=null;
				continue;
			}
			try {
				playOnLine(line,audioInputStream);
			} catch (Throwable t) {
				System.err.println("Error playing: "+t.getMessage());
				line=null;
				continue;
			}
			// plays what's left and and closes the audioChannel
		    line.drain();
		    line.close();
		}
	}
	
	/**
	 * Play the sound using the passed line.
	 * 
	 * @param line
	 * @param audioIStream
	 */
	private void playOnLine(SourceDataLine line,AudioInputStream audioIStream) {
		if (line==null) {
			return;
		}
		int readBytes = 0;
		byte[] audioBuffer = new byte[EXTERNAL_BUFFER_SIZE];
	 
		try {
		    while (readBytes != -1) {
		    	readBytes = audioIStream.read(audioBuffer, 0,audioBuffer.length);
		    	if (readBytes >= 0){
		    		int ret=line.write(audioBuffer, 0, readBytes);
		    	}
		    }
		} catch (IOException e1) {
		    System.err.println("Exception caught: "+e1.getMessage());
		}
		
	}
	
	/**
	 * Set the level of the alarm to which a sound has to be played.
	 * 
	 * @param level The new level of the sound to  play;
	 * 				level must be in the same range of {@link AlarmSound#soundLevel}.
	 * @see {@link AlarmSound#soundLevel}
	 */
	public void inhibit(int level) {
		if (level<1 || level>3) {
			throw new IllegalArgumentException("Level "+level+" not in [1,3]");
		}
		soundLevel=level;
		inhibitCounter=0;
	}
	
	/**
	 * Return the inhibit sound level
	 * 
	 * @return thethe inhibit sound level
	 * @see {@link AlarmSound#soundLevel}
	 */
	public int getSoundLevel() {
		return soundLevel;
	}

	/**
	 * Close the timer.
	 */
	public void close() {
		listener=null;
		closed=true;
		timer.cancel();
	}

	/**
	 * The task to emit the sound.
	 */
	@Override
	public void run() {
		// Should the inhibit level be cleared?
		if (++inhibitCounter>=INHIBIT_TIME) {
			inhibitCounter=0;
			soundLevel=3;
			if (listener!=null) {
				listener.reset();
			}
		}
		final int pri= tableModel.hasNotAckAlarms();
		
		// Audibles inhibited for the lowest 2 priorities till
		// the alarm system is better is better configured
		//
		// TODO: remove the following link to re-enable sounds for
		//		 lowest priorities
		if (pri>1) {
			return;
		}
		
		if (pri<0 || pri>soundLevel) {
			// No unseen alarms in table
			// or alarm inhibited
			return;
		}
		if (listener!=null) {
			listener.playing(pri);
		}
		// Emit the sound in a separate thread
		Thread soundPlayerThread = new Thread(new Runnable() {
			public void run() {
				try {
					play(pri);
				} catch (Throwable t) {
					// Write a message in stderr
					System.err.println("Error emitting sound for priority "+pri);
					t.printStackTrace(System.err);
				} finally {
					if (listener!=null) {
						listener.played();
					}
				}
			}
		}, "SoundPlayer");
		soundPlayerThread.setDaemon(true);
		soundPlayerThread.start();
	}
	
	/**
	 * Add or remove a listener
	 *  
	 * @param listener If not <code>null</code> add the listener;
	 * 					otherwise remove the listener
	 */
	public void addSoundListener(AlarmSoundListener listener) {
		this.listener=listener;
	}
	
	/**
	 * Dump info about supported audio, file types and so on...
	 * <P>
	 * This method is useful while updating the audio files.
	 */
	private void dumpAudioInformation() {
		// Java supported file types
		 AudioFileFormat.Type[] fileTypes= AudioSystem.getAudioFileTypes();
		 if (fileTypes==null || fileTypes.length==0) {
			 System.out.println("No audio file types supported.");
		 } else {
			 for (AudioFileFormat.Type type: fileTypes) {
				 System.out.println(type.toString()+", extension "+type.getExtension());
			 }
		 }
		 Mixer.Info[] mixerInfos=AudioSystem.getMixerInfo();
		 System.out.println("Mixers found: "+mixerInfos.length);
		 for (Mixer.Info mi: mixerInfos) {
			 System.out.println("\tMixer "+mi.getName()+": "+mi.getVendor()+", "+mi.getDescription());
		 }
		 // Dump info about the alarm files
		 for (URL url: soundURLs) {
			AudioFileFormat format=null;
			try {
				format=AudioSystem.getAudioFileFormat(url);
			} catch (IOException ioe) {
				System.err.println("Error "+ioe.getMessage()+" accessing URL "+url.toString());
				continue;
			} catch (UnsupportedAudioFileException ue) {
				System.err.println("Unsupported audio format for "+url+" ("+ue.getMessage()+")");
			}
			System.out.println("Properties of "+url);
			System.out.println("\tAudio file type "+format.getType().toString());
			System.out.println("\tIs file type supported: "+AudioSystem.isFileTypeSupported(format.getType()));
			System.out.println("\tLength in byes "+format.getByteLength());
			Map<String,Object> props=format.properties();
			Set<String> keys=props.keySet();
			for (String str: keys) {
				System.out.println("\t["+str+", "+props.get(str).toString()+"]");
			}
			AudioFormat aFormat=format.getFormat();
			System.out.println("\tEncoding "+aFormat.getEncoding().toString());
			System.out.print("\tByte order ");
			if (aFormat.isBigEndian()) {
				System.out.println("big endian");
			} else {
				System.out.println("little endian");
			}
			System.out.println("\tSample rate: "+aFormat.getSampleRate());
			System.out.println("\tNum. of bits of a sample: "+aFormat.getSampleSizeInBits());
			System.out.println("\tNum. of channels: "+aFormat.getChannels());
		 }
	}
}
