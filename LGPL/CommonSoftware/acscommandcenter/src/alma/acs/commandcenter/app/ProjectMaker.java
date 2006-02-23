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

import alma.acs.commandcenter.CommandCenter;
import alma.entity.xmlbinding.acscommandcenterproject.AcsCommandCenterProject;
import alma.entity.xmlbinding.acscommandcenterproject.ContainerT;
import alma.entity.xmlbinding.acscommandcenterproject.ContainersT;
import alma.entity.xmlbinding.acscommandcenterproject.types.ModeType;



/**
 * 
 * @author mschilli
 */
public class ProjectMaker {
	
	protected CommandCenterLogic ctrl;
	public ProjectMaker(CommandCenterLogic ctrl) {
		this.ctrl = ctrl;
	}

	
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
				cdbroot = "/alma/ACS-4.0/acsdata/config/defaultCDB";	
			}
		}
		
		String cdbpath = null;
		{
			// 1. ACS.cdbpath
			cdbpath = System.getProperty("ACS.cdbpath");
			
			// 2. $ACSROOT/config/CDB/schemas
			if (cdbpath == null || cdbpath.equals("")) {
				String acsroot = System.getProperty("ACS.root");
				if (acsroot != null) {
					acsroot += (acsroot.endsWith("/")) ? "" : "/";
					cdbpath = acsroot + "config/CDB/schemas";

					//	store, so cdb-jDAL can find it
					if (cdbpath != null) 
						System.setProperty("ACS.cdbpath", cdbpath);
					// TODO(msc): instead setting sysprop ACS.cdbpath: store in project, add field to gui
				}
			}
			
			// 3. fallback to what?
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
		
		ret.setCreator(ctrl.projectCreatorId());
		
		return ret;
	}

	
	public AcsCommandCenterProject loadProject (File f) throws MarshalException, ValidationException, FileNotFoundException {
		AcsCommandCenterProject project = readProject(f);
		
		if (isFromAnOutdatedVersion(project)) {
			// could ask here whether upgrade is wanted
		}
		
		// checks and upgrades the project
		sanitizeProject(project); 
		
		return project;
	}


	/**
	 * Purges passwords from the project, sets the version/creator info,
	 * then writes the project to disk.
	 */
	public void writeProject (AcsCommandCenterProject p, File f) throws IOException, MarshalException, ValidationException {

		// --- clear all passwords from the model
		p.setRemotePassword("");
		for (int i = 0; i < p.getContainers().getContainerCount(); i++) {
			p.getContainers().getContainer(i).setRemotePassword("");
		}

		// --- write to disk
		BufferedWriter w = new BufferedWriter(new FileWriter(f));
		p.marshal(w);
	}

	
	protected ContainerT createContainer () {
		ContainerT ret = new ContainerT();
		ret.setType("java");
		return ret;
	}

	/**
	 * Used to upgrade a project after it was read in.
	 */
	protected void sanitizeProject(AcsCommandCenterProject p) {
		
		// --- upgrading projects to 4.1
		
		if (p.getMode() == null) {
			p.setMode(ModeType.LOCAL);
		}
		if (! p.hasToolRunAgainstDedicatedSettings()) {
			p.setToolRunAgainstDedicatedSettings(false);
		}

		// --- upgrading projects to xyz

		
		// --- store the fact that the project was sanitized 
		//     by this current command center version
		p.setCreator(ctrl.projectCreatorId());
		
	}
	
	
	protected AcsCommandCenterProject readProject (File f) throws FileNotFoundException, MarshalException, ValidationException {
		BufferedReader r = new BufferedReader(new FileReader(f));
		AcsCommandCenterProject p = AcsCommandCenterProject.unmarshalAcsCommandCenterProject(r);
		return p;
	}


	/**
	 * @since v4.1
	 */
	protected boolean isFromAnOutdatedVersion(AcsCommandCenterProject p) {
		String v = p.getCreator();
		if (v == null) {
			return true;
		}
		// PENDING(msc): more sophisticated version checking
		boolean a = v.equals("acc-v4.1");
		boolean b = v.startsWith("acc-v4.1");
		
		return !a; // sufficient in current version
	}	
	
}



	// ================================================
	// API
	// ================================================

	// ================================================
	// Internal
	// ================================================

	// ================================================
	// Inner Types
	// ================================================

