/*
 * Created on Oct 4, 2006 by mschilli
 */
package alma.acs.jhelpgen;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import alma.acs.jhelpgen.Gen.AnchorNode;




public class Util {


	static List<File> findFiles (List<File> ret, File dir, final String... extensions) {
		/* we're processing the real files in every dir first */
		List<File> subdirs = new LinkedList<File>();
		File[] ff = dir.listFiles();
		Arrays.sort(ff, alphabeticSort);
		for (File f : ff) {
			if (f.isDirectory())
				subdirs.add(f);
			else 
				for (String ext : extensions) {
					if (f.getName().endsWith(ext))
						ret.add(f);
				}
		}
		for (File f : subdirs)
			findFiles (ret, f, extensions);
		
		return ret;
	}

	private static Comparator<File> alphabeticSort = new Comparator<File>() {
		public int compare (File o1, File o2) {
			return o1.getName().compareTo(o2.getName());
		}
	};

	/**
	 * Reads in the file's contents, skipping all
	 * line terminators (newlines or carriage returns).
	 * 
	 * @param f the file
	 * @return the file contents (without line terminators)
	 */
	static String readFile (File f) {
		BufferedInputStream bis = null;
		try {
			bis = new BufferedInputStream(new FileInputStream(f));
			StringBuilder buf = new StringBuilder((int) f.length());
			int c;
			while (true) {
				c = bis.read();
				if (c == -1)
					break;
				if (c == '\r' || c == '\n')
					continue;
				buf.append((char) c);
			}

			return buf.toString();

		} catch (IOException e) {
			throw new RuntimeException("workdir is " + System.getProperty("user.dir") + ", couldn't read file contents: " + e);

		} finally {
			try {
				bis.close();
			} catch (Exception e1) {}
		}
	}

	static void writeFile (String contents, File f) {
		FileWriter fw = null;
		try {
			fw = new FileWriter(f);
			fw.write(contents);
			
		} catch (IOException e) {
			throw new RuntimeException("workdir is "+System.getProperty("user.dir")+", couldn't write contents to file: "+e);
	
		} finally {
			try {
				fw.close();
			} catch (Exception exc) {}
		}
	}
		
	
}


