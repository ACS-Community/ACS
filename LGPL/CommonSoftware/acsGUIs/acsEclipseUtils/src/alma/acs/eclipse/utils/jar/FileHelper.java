/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.eclipse.utils.jar;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

/**
 * An helper class performing usefule file oeprations.
 * 
 * @author acaproni
 *
 */
public class FileHelper {

	/**
	 * Copy the source file in the destination.
	 * 
	 * @param src The file to copy
	 * @param dest The copied file
	 * @throws IOException In case of I/O error while copying
	 */
	public static void copy(File src, File dest) throws IOException {
		if (src==null || dest==null) {
			throw new IllegalArgumentException("The src/dest file cant be null");
		}
		if (!src.canRead()) {
			throw new IllegalArgumentException("Source file unreadable");
		}
		FileInputStream inF= new FileInputStream(src);
		FileOutputStream outF = new FileOutputStream(dest);
		byte[] buffer = new byte[1024];
		int charsRead;
	    while ((charsRead=inF.read(buffer)) != -1) {
	    	outF.write(buffer,0, charsRead);
	    }
	    inF.close();
	    outF.close();
	}
	
	/**
	 * Copy the source file in the destination.
	 * 
	 * @param src The path of the source file
	 * @param dest The path of the dest file
	 * @throws IOException In case of I/O error while copying
	 */
	public static void copy (String src, String dest) throws IOException {
		if (src==null || src.isEmpty()) {
			throw new IllegalArgumentException("Invalid source name");
		}
		if (src==null || src.isEmpty()) {
			throw new IllegalArgumentException("Invalid destination name");
		}
		copy(new File(src), new File(dest)); 
	}
	
	/**
	 * Link the source file in the destination.
	 * <P>
	 * Java does not support file links and this method should be implemented with the 
	 * help of JNI.
	 * This problems should be fixed in the incoming JDK7.
	 * <P>
	 * In this version the link is replaced by a copy.
	 * 
	 * TODO: Implement this method with JNI and use JDK7 method when available
	 * 
	 * @param src The path of the source file
	 * @param dest The path of the dest file
	 * @throws IOException In case of I/O error while linking
	 */
	public static void link(String src, String dest) throws IOException {
		copy(src,dest);
	}
	
	/**
	 * Link the source file in the destination.
	 * <P>
	 * Java does not support file links and this method should be implemented with the 
	 * help of JNI.
	 * This problems should be fixed in the incoming JDK7.
	 * <P>
	 * In this version the link is replaced by a copy.
	 * 
	 * TODO: Implement this method with JNI and use JDK7 method when available
	 * 
	 * @param src The file to copy
	 * @param dest The copied file
	 * @throws IOException In case of I/O error while copying
	 */
	public static void link(File src, File dest) throws IOException {
		copy(src,dest);
	}
}
