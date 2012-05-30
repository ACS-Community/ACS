package com.cosylab.acs.maci.manager.app;

import java.io.File;
import java.io.IOException;
import java.util.Timer;
import java.util.TimerTask;

import org.prevayler.implementation.SnapshotPrevayler;

import com.cosylab.util.FileHelper;

/**
 * @author dragan
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
public class RecoverySnapshotTask extends TimerTask {

	private Timer t = new Timer(true);
	private SnapshotPrevayler prevayler;
	private String recoveryLocation;

	public RecoverySnapshotTask( SnapshotPrevayler prevayler, long period, String recoveryLocation ) {
		super();
		this.prevayler = prevayler;
		this.recoveryLocation = recoveryLocation;
		t.schedule(this, period, period);
	}

	/**
	 * @see Runnable#run()
	 */
	public void run() {
		try {
			// make new snapshot
			synchronized (prevayler) {
				prevayler.takeSnapshot();
			}
			//add rights to group in order to be able to start with '-n'
			FileHelper.setFileAttributes( "g+w", recoveryLocation );
			
			// clean up all old logs
			RecoveryFilesRemover.removeRecoveryFiles(new File(recoveryLocation));

		}
		catch (IOException e) {
			/// What to do with this exception ?
			e.printStackTrace();
		}
	}
}
