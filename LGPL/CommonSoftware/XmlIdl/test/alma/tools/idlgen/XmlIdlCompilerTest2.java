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
package alma.tools.idlgen;

import java.io.File;

import junit.framework.TestCase;

/**
 * @author hsommer Apr 4, 2003 5:11:01 PM
 */
public class XmlIdlCompilerTest2 extends TestCase
{
	public void testMain()
	{
		String acsroot = System.getProperty("ACSROOT");
		assertNotNull("Java property 'ACSROOT' must be defined.", acsroot);
		assertTrue("ACSROOT/idl/acscomponent.idl must exist", new File(acsroot + "/idl/acscomponent.idl").exists());
		
		String jacorbroot = System.getProperty("JACORB_HOME");
		assertNotNull("Java property 'JACORB_HOME' must be defined.", jacorbroot);

		// check if IDL file is available (current directory must be xmlidl/test)
		String idlFileName = "../idl/acscourseMount.idl";
		File idlFile = new File(idlFileName);
		assertTrue("test IDL file " + idlFile.getAbsolutePath() + " present", idlFile.exists());
		
//		String idl2jbind = "SchedBlock=dummyentities.SchedBlock;" +
//							"ObsProject=dummyentities.ObsProject;" +
//							"ObsProposal=dummyentities.ObsProposal";
							
//		System.setProperty("ACS.idl2jbind", idl2jbind);
		
		System.setProperty(XmlIdlCompiler.PROP_DO_GENERATE_COMP_HELPERS, "true");
		System.setProperty(XmlIdlCompiler.PROP_COMP_HELPERS_OUTDIR, "../src_generated");
					
		String[] args = {
				"-d", "../src_generated", // output dir
				"-I../idl",
				"-I" + acsroot + "/idl", 
				"-I" + jacorbroot + "/idl/jacorb", 
				"-I" + jacorbroot + "/idl/omg",
//				"-verbose",
				idlFileName
		};
		XmlIdlCompiler.main(args);
	}

}
