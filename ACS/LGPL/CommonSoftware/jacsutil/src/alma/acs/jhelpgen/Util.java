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
/*
 * Created on Oct 4, 2006 by mschilli
 */
package alma.acs.jhelpgen;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;




public class Util {


	/**
	 * Returns all files and directories underneath the specified <code>dir</code>,
	 * recursively. Files can be filtered by extensions.
	 * 
	 * @param ret results will be added to this list
	 * @param dir the directory to search (recursively)
	 * @param extensions only include files with these extensions 
	 * @return
	 */
	public static List<File> findFiles (List<File> ret, File dir, final String... extensions) {
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
	 * @throws RuntimeException if something goes wrong
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

	/**
	 * Writes a string to a file.
	 * 
	 * @param contents the string to write
	 * @param f the file to write
	 * @throws RuntimeException if something goes wrong
	 */
	public static void writeFile (String contents, File f) {
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
	
	
	/**
	 * Reads a resource into a string.
	 * 
	 * @param name the resource's name
	 * @return the resource's content
	 * @throws RuntimeException if something goes wrong
	 */
	public static String readResource (String name) {
		InputStream is = Util.class.getResourceAsStream(name);
		if (is == null)
			throw new RuntimeException("resource not found: "+name);
		
		BufferedInputStream bis = null;
		try {
			bis = new BufferedInputStream(is);
			StringBuilder buf = new StringBuilder(4096);
			int c;
			while (true) {
				c = bis.read();
				if (c == -1)
					break;
				buf.append((char) c);
			}

			return buf.toString();

		} catch (IOException e) {
			throw new RuntimeException("couldn't read from resource: " + e);

		} finally {
			try {
				bis.close();
			} catch (Exception e1) {}
		}
	}
		
	
}


