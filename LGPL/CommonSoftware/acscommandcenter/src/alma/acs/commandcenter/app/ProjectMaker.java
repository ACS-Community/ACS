/*
 * Created on Oct 26, 2005 by mschilli
 */
package alma.acs.commandcenter.app;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import org.exolab.castor.xml.MarshalException;
import org.exolab.castor.xml.ValidationException;

import alma.entity.xmlbinding.acscommandcenterproject.AcsCommandCenterProject;
import alma.entity.xmlbinding.acscommandcenterproject.ContainerT;
import alma.entity.xmlbinding.acscommandcenterproject.ContainersT;
import alma.entity.xmlbinding.acscommandcenterproject.types.ModeType;



/**
 * 
 * @author mschilli
 */
public class ProjectMaker {
	
	protected String projectCreatorId;
	public ProjectMaker(String projectCreatorId) {
		this.projectCreatorId = projectCreatorId;
	}

	
	// CREATE PROJECTS
	// =============================================================
	
	public AcsCommandCenterProject createProject () {
		AcsCommandCenterProject ret = new AcsCommandCenterProject();


		/* 3 env.variables are of interest: ACS_CDB, ACSROOT, ACSDATA.
		 * The first two transformed to Java system properties by
		 * the acscommandcenter script, not acsStartJava.
		 * We try to locate the cdb root directory and provide a fallback if
		 * we can't.  
		 * We want to find additional schemas stored under ACSROOT of the
		 * acs installation. The additional schema path is stored in the sysprop
		 * "ACS.cdbpath" which we do not want to touch if it has been set by the
		 * user. Otherwise, we'll infer it from ACSROOT.
		 */ 

		String cdbroot = null; 
		{
			// 1. $ACS_CDB
			cdbroot = System.getProperty("ACS.cdbroot");
				
			// 2. $ACSDATA/config/defaultCDB
			if (cdbroot == null || cdbroot.equals("")) {
				String acsdata = System.getProperty("ACS.data");
				if (acsdata != null) {
					acsdata += (acsdata.endsWith("/")) ? "" : "/";
					cdbroot = acsdata + "config/defaultCDB";
				}
			}
			
			// 3. fallback
			if (cdbroot == null || cdbroot.equals("")) {
				cdbroot = "/alma/ACS-8.0/acsdata/config/defaultCDB";	
			}
		}

		ret.setMode(ModeType.LOCAL);
		ret.setScriptBase(System.getProperty("ACS.baseport", "9"));
		ret.setServicesLocalJavaRoot(cdbroot);
		
		ret.setRemoteHost("");

		ret.setToolAgainstManagerHost("");
		ret.setToolAgainstManagerPort("");
		ret.setToolAgainstNameService("");
		ret.setToolAgainstInterfaceRepository("");
		
		ret.setContainers(new ContainersT());
		ret.getContainers().setAgainstManagerHost("");
		ret.getContainers().setAgainstManagerPort("");
		ret.getContainers().setAgainstCDB("");
		ret.getContainers().setAgainstInterfaceRepository("");
		ret.getContainers().addContainer(createContainer());
		
		ret.setCreator(projectCreatorId);
		
		return ret;
	}

	
	
	protected ContainerT createContainer () {
		ContainerT ret = new ContainerT();
		ret.setType("java");
		return ret;
	}

	
	
	// WRITE PROJECTS
	// =============================================================
	

	/**
	 * Purges passwords from the project, sets the version/creator info,
	 * then writes the project to disk.
	 */
	public void writeProject (AcsCommandCenterProject p, File f) throws IOException, MarshalException, ValidationException {

		// clear all passwords from the model
		p.setRemotePassword("");
		for (int i = 0; i < p.getContainers().getContainerCount(); i++) {
			p.getContainers().getContainer(i).setRemotePassword(null);
		}

		// write to disk
		BufferedWriter w = new BufferedWriter(new FileWriter(f));
		p.marshal(w);
		w.flush();
		w.close();
	}


	
	// READ PROJECTS
	// =============================================================
	
	
	public AcsCommandCenterProject loadProject (File f) throws MarshalException, ValidationException, FileNotFoundException, IOException {
		AcsCommandCenterProject project = readProject(f);
		sanitizeProject(project); 
		return project;
	}

	
	protected AcsCommandCenterProject readProject (File f) throws FileNotFoundException, MarshalException, ValidationException, IOException {
		BufferedReader r = new BufferedReader(new FileReader(f));
		AcsCommandCenterProject p = AcsCommandCenterProject.unmarshalAcsCommandCenterProject(r);
		r.close();
		return p;
	}


	protected void sanitizeProject (AcsCommandCenterProject p) {
		boolean sanitized = false;

		// catch ill cases caused by manual editing
		// ----------------------------
		if (p.getContainers() == null) {
			p.setContainers(new ContainersT());
			sanitized = true;
		}

		// upgrade projects to 4.1
		// ----------------------------
		if (p.getMode() == null) {
			p.setMode(ModeType.LOCAL);
			sanitized = true;
		}

		if (! p.hasToolRunAgainstDedicatedSettings()) {
			p.setToolRunAgainstDedicatedSettings(false);
			sanitized = true;
		}

		// upgrade projects to 8.0
		// ----------------------------
		for (ContainerT c : p.getContainers().getContainer()) {
			if (c.getType().equals("archive")) {
				c.setType("java");
				c.addTypeModifier("archiveContainer");
				sanitized = true;
			}
		}

		if (sanitized)
			// now you're up-to-date
			p.setCreator(projectCreatorId);
	}


}

