/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2015 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.acs.logging.io;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;


/**
 * <code>UrlDownloader</code> downloads the passed URL into a temporary file.
 * 
 * Objects of this class download the passed URL into a temporary file
 * that will be removed when jlog terminates.
 * 
 * @author  acaproni
 * @since 2015.8
 */
public class UrlDownloader {
	
	/**
	 * The URL of the resource to download
	 */
	private final URL url;
	
	/**
	 * The name of the temporary file extracted from the URL
	 */
	private final String fileName;
	
	/**
	 * 
	 * @param url The URL of the resource to download
	 */
	public UrlDownloader(URL url) {
		if (url==null) {
			throw new IllegalArgumentException("Invalid null URL to download");
		}
		this.url=url;
		this.fileName=extractFileName();
	}
	
	/**
	 * Extract the file name form the {@link #url}
	 * 
	 * @return The name of the file to save in the temporary folder
	 */
	private String extractFileName() {
		int pos=url.toString().lastIndexOf("/");
		return (pos==-1)? url.toString(): url.toString().substring(pos);
	}
	
	/**
	 * Get the name of the temporary folder in this order:
	 * <OL>
	 * 	<LI>ACS temporary folder
	 * 	<LI>system temporary folder
	 * 	<LI>current directory
	 * </OL>
	 * Return the temporary folder to store the file into
	 */
	private String getTempFolder() {
		// Try to get ACS temporary folder
		String acsTtempFolder=System.getProperty("ACS.tmp", ".");
		if (acsTtempFolder.equals(".")) {
			// Try with the system temporary folder 
			acsTtempFolder= System.getProperty("java.io.tmpdir",".");
		}
		return acsTtempFolder;
	}
	
	/**
	 * Download the url in the thread.
	 * 
	 * @return The name of the downloaded file
	 */
	public String download() throws IOException {
		String outFolder=getTempFolder();
		String outFName = outFolder+File.separator+fileName;
		
		System.out.println("UrlDownloader: Downloading "+url+" as "+outFName);
		
		ReadableByteChannel rbc = Channels.newChannel(url.openStream());
		FileOutputStream fos = null;
		try {
			fos = new FileOutputStream(outFName);
			fos.getChannel().transferFrom(rbc, 0, Long.MAX_VALUE);
		} finally {
			if (fos!=null) {
				fos.close();
			}
		}
		System.out.println("UrlDownloader: "+url+" downloaded as "+outFName);
		return outFName;
		
	}
}
