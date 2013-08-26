/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.manager.app;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.Arrays;

/**
 *  Removed old (obsolete) recovery files.
 * @author msekoranja
 */
public class RecoveryFilesRemover {

	public static void removeRecoveryFiles(File recoveryDirectory)
	{
		try {
			long limit = Long.MAX_VALUE;
			File lastSnapshot = removeSnapshots(recoveryDirectory, true, "snapshot", limit);
			if (lastSnapshot != null)
				limit = number(lastSnapshot, "snapshot"); // since last will not be deleted
			else
				// preserve all command logs if no snapshot is present
				limit = -1;
			
			// delete only commandLog older than the last snapshot (lower or equal number)
			removeSnapshots(recoveryDirectory, false, "commandLog", limit);
		} catch (Throwable th) {
			th.printStackTrace();
		}
	}

	public static File removeSnapshots(File recoveryDirectory, boolean leaveLast, String suffix, long limit) throws IOException {
			if (!recoveryDirectory.exists())
				return null;
		
			File[] snapshots = recoveryDirectory.listFiles(new SnapshotFilter(suffix, limit));
			if (snapshots == null)
				throw new IOException("Error reading file list from directory " + recoveryDirectory);

			Arrays.sort(snapshots);

			// leave last
			int lastNotToDelete = snapshots.length;
			if (leaveLast)
				lastNotToDelete--;
			for (int i = 0; i < lastNotToDelete; i++)
				snapshots[i].delete();
			
			// return last snapshot (that one that was not deleted)
			if (snapshots.length > 0 && lastNotToDelete != snapshots.length)
				return snapshots[lastNotToDelete];
			else
				return null;
	}

	private static long number(File snapshot, String suffix) throws NumberFormatException { 
		String name = snapshot.getName();
		if (!name.endsWith("." + suffix))
			throw new NumberFormatException();
		
		return Long.parseLong(name.substring(0, name.indexOf('.'))); // "00000.snapshot" becomes "00000".
	}

	private static class SnapshotFilter implements FileFilter {

		private String suffix;
		private long limit;
		
		public SnapshotFilter(String suffix, long limit) {
			this.suffix = suffix;
			this.limit = limit;
		}

		public boolean accept(File file) {
			try {
				long n = number(file, suffix);
				if (n > limit)
					return false;
			} catch (Throwable th) {
				return false;
			}
			return true;
		}
	}
}