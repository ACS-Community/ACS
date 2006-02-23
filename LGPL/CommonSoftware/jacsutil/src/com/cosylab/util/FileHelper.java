/*
 * Created on May 12, 2003
 *
 * To change this generated comment go to 
 * Window>Preferences>Java>Code Generation>Code Template
 */
package com.cosylab.util;

import java.io.File;

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

}
