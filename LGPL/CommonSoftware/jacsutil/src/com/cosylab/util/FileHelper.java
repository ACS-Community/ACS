/*
 * Created on May 12, 2003
 *
 * To change this generated comment go to 
 * Window>Preferences>Java>Code Generation>Code Template
 */
package com.cosylab.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;

/**
 * @author dvitas
 */
public class FileHelper {

	/**
	 * Method getTempFileName.
	 * This is a copy from C++ acsutilTempFile
	 *
	 * @param fileNameEnvVar
	 * @param fileName
	 * @return String
	 */
	static public String getTempFileName(String fileNameProperty, String fileName) {

		final String ACS_TEMP_DIR = "ACS.tmp";
		final String TEMP_DIR = "tmp";

		String propertyVal;
		String filePath = null;

		// check for <fileNameEnvVal> env. var
		if (fileNameProperty != null) {
			propertyVal = System.getProperty(fileNameProperty);
			if (propertyVal != null)
				return propertyVal;
		}

		// fileName is needed, check if defined
		if (fileName == null)
			return new String();

		// check if another temp. dir. is specified
		propertyVal = System.getProperty(ACS_TEMP_DIR);
		if (propertyVal != null)
			filePath = propertyVal;
		// use default
		else {
			filePath = "";
			filePath += File.separatorChar;
			filePath += TEMP_DIR;
		}

		filePath += File.separatorChar;
		filePath += fileName;

		return filePath;
	}

	/**
	 * Tries to set attributes given by attribs on given file
	 * by executing "chmod -R attribs filePath" as external
	 * process.
	 * If it fails or there is no such command on current OS 
	 * it does nothing naither printouts the error.
	 * 
	 * @param attribs
	 * @param filePath
	 */
	static public void setFileAttributes(String attribs, String filePath) {
		File file = new File(filePath);
		if( !file.exists() ) 
			return;
		String command = "chmod -R " +  attribs + " " + filePath;
		try {
			Runtime.getRuntime().exec(command).waitFor();
		} catch (Exception e) {
			// nop
		}
	}
	
	/**
	 * Copies file <code>source</code> to location <code>dest</code>.
	 * Necessary directories are created automatically.
	 * The modification time is preserved if <code>preserveTime</code> is <code>true</code>.
	 */
	public static void copy(File source, File dest, boolean preserveTime) throws IOException {
		FileChannel in = null, out = null;
		try {
			dest.getParentFile().mkdirs();
			
			in = new FileInputStream(source).getChannel();
			out = new FileOutputStream(dest).getChannel();

			long size = in.size();
			MappedByteBuffer buf = in.map(FileChannel.MapMode.READ_ONLY, 0, size);

			out.write(buf);

			if (preserveTime) {
				dest.setLastModified(source.lastModified());
			}
		} finally {
			if (in != null) {
				in.close();
			}
			if (out != null) {
				out.close();
			}
		}
	}

}
