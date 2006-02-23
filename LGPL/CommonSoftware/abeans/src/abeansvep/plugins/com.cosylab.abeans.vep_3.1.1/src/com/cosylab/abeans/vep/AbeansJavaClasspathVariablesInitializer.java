/*
 * Created on Feb 27, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package com.cosylab.abeans.vep;

import java.io.File;
import java.net.URL;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.ClasspathVariableInitializer;
import org.eclipse.jdt.core.JavaCore;
import org.osgi.framework.Bundle;

/**
 * @author jkamenik
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class AbeansJavaClasspathVariablesInitializer extends ClasspathVariableInitializer {

	
	private IProgressMonitor fMonitor;
	private static final String ABEANS_LIB = "Abeans VE Plug-in";
	
	

	/* (non-Javadoc)
	 * @see org.eclipse.jdt.core.ClasspathVariableInitializer#initialize(java.lang.String)
	 */
	public void initialize(String variable) {
		if (variable.equals(ABEANS_LIB))
			try {
				Bundle bundle = Platform.getBundle("com.cosylab.abeans.vep"); //$NON-NLS-1$

				URL installLocation= bundle.getEntry("abeansplugin.jar"); //$NON-NLS-1$
				URL local= Platform.asLocalURL(installLocation);

				String fullPath= new File(local.getPath()).getAbsolutePath();
				JavaCore.setClasspathVariable(ABEANS_LIB, new Path(fullPath), getMonitor());
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				JavaCore.removeClasspathVariable(ABEANS_LIB, getMonitor());
				return;
			}	
	}

	protected IProgressMonitor getMonitor() {
		if (fMonitor == null) {
			return new NullProgressMonitor();
		}
		return fMonitor;
	}
}
