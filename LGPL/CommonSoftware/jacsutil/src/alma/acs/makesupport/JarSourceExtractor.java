/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
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
package alma.acs.makesupport;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.JarOutputStream;
import java.util.zip.ZipException;

/**
 * @author hsommer
 * created Sep 16, 2003 4:24:41 PM
 */
public class JarSourceExtractor extends AbstractJarEntryExtractor
{

	/**
	 * 
	 */
	public JarSourceExtractor()
	{
		super(".java");
	}


	/**
	 * Extracts Java source files from a JAR file and puts them as individual files
	 * under a given directory.
	 * 
	 * @param jarfile  jar file from which Java source will be extracted
	 * @param outDir  root dir under which the extracted java files will be placed
	 * @throws IOException
	 */
	public void extractJavaSourcesToFiles(JarFile jarfile, File outDir) throws IOException
	{
		if (!outDir.exists() || !outDir.isDirectory() || !outDir.canWrite())
		{
			throw new IOException("specified directory " + outDir.getAbsolutePath() + 
						" is not a valid writeable directory.");
		}
		
		JarEntry[] javaEntries = getJavaEntries(jarfile);
		
		for (int i = 0; i < javaEntries.length; i++)
		{
			JarEntry javaEntry = javaEntries[i];
			
			String className = getClassName(javaEntry);			
			FileOutputStream out = null;
			try
			{
				File outFile = new File(outDir.getAbsolutePath() + File.separator + className);
//				System.out.println("outfile name = " + outFile.getAbsolutePath());
				outFile.getParentFile().mkdirs();
				out = new FileOutputStream(outFile);
				extract(jarfile, javaEntry, out);
			}
			finally
			{
				if (out != null)
				{
					out.close();
				}
			}
		}
	}


	/**
	 * Extracts Java source files from a JAR file and adds them to another JAR file.
	 * 
	 * @param jarfile  jar file from which Java source will be extracted
	 * @param jarOut  JAR output stream to which the extracted java files will be written;
	 * 					<code>jarOut</code> is left open by this method, so that the client
	 * 					can either call it again, or call <code>jarOut.close()</code> when it's done.
	 * 					(there are problems with re-opening and adding entries to a Jar file.) 
	 * @throws IOException
	 */
	public void extractJavaSourcesToJar(JarFile jarfile, JarOutputStream jarOut) throws IOException
	{
		JarEntry[] javaEntries = getJavaEntries(jarfile);
		if (javaEntries.length > 0)
		{
			System.out.println("extracting .java from " + jarfile.getName());
			for (int i = 0; i < javaEntries.length; i++)
			{
				JarEntry javaEntry = javaEntries[i];
				
				String className = getClassName(javaEntry);
				JarEntry outEntry = new JarEntry(className); 
				outEntry.setTime(javaEntry.getTime());
				// write the JarEntry meta-data
				try {
					jarOut.putNextEntry(outEntry);
					// write the entry data
					extract(jarfile, javaEntry, jarOut);
					jarOut.closeEntry();				
				} catch (ZipException e) {
					System.err.println("failed to add JarEntry for class '" + className + "' from file '" 
							+ jarfile.getName() + "': " + e.toString());
				}				
			}
			jarfile.close();
			jarOut.flush();
		}
		else
		{
			System.out.println("no .java found in " + jarfile.getName());
		}
	}


	/**
	 * Extracts a file from a JAR file and writes it to an output stream.
	 * Does not close the output stream.
	 * 
	 * @param jarfile  the jarfile from which to extract a file
	 * @param entry  the entry (file) to be extracted from the jar file
	 * @param out  the stream to write the entry to
	 * @throws IOException  if the Jar file can't be read or the out stream can't be written to
	 */
	void extract(JarFile jarfile, JarEntry entry, OutputStream out) throws IOException
	{
		if (entry != null)
		{
			InputStream entryStream = null;
			try
			{
				entryStream = jarfile.getInputStream(entry);
	
				byte[] buffer = new byte[1024];
				int bytesRead;
				while ((bytesRead = entryStream.read(buffer)) != -1)
				{
					out.write(buffer, 0, bytesRead);
				}
			}
			finally
			{
				if (entryStream != null)
				{
					entryStream.close();
				}
			}
		}
	}


}
