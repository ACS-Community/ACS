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
package alma.acs.eventbrowser;

import java.util.concurrent.ArrayBlockingQueue;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.equinox.app.IApplication;
import org.eclipse.equinox.app.IApplicationContext;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import alma.acs.eventbrowser.model.EventModel;
import alma.acs.eventbrowser.preferences.MonitoringPreferencePage;
import alma.acs.eventbrowser.views.ArchiveEventData;
import alma.acs.eventbrowser.views.EventData;

/**
 * This class controls all aspects of the application's execution
 * 
 * $Id: Application.java,v 1.9 2011/10/28 11:56:21 jschwarz Exp $
 * 
 */
public class Application implements IApplication, IStartup {
	
	public static final String PLUGIN_ID = "alma.acs.eventbrowser";
	
	public static ArrayBlockingQueue<EventData> equeue = new ArrayBlockingQueue<EventData>(50000);
	public static ArrayBlockingQueue<ArchiveEventData> archQueue = new ArrayBlockingQueue<ArchiveEventData>(100000);
	
	private static boolean monitoring = false;
	
	private EventModel em;

	/* (non-Javadoc)
	 * @see org.eclipse.equinox.app.IApplication#start(org.eclipse.equinox.app.IApplicationContext)
	 */
	public Object start(IApplicationContext context) throws Exception {
		Display display = PlatformUI.createDisplay();
		Runtime.getRuntime().addShutdownHook(new ShutdownHook());
		try {
			setMonitoring(getMonitoringPreference());
			try {
				em = EventModel.getInstance();
			} catch (Exception e) {
				System.err.println("Can't create EventModel...exiting.");
				return IApplication.EXIT_OK;
			}
			
			int returnCode = PlatformUI.createAndRunWorkbench(display, new ApplicationWorkbenchAdvisor());
			em.tearDown();
			if (returnCode == PlatformUI.RETURN_RESTART)
				return IApplication.EXIT_RESTART;
			else
				return IApplication.EXIT_OK;
		} finally {
			if (!display.isDisposed()) display.dispose();
		}
		
	}

	/* (non-Javadoc)
	 * @see org.eclipse.equinox.app.IApplication#stop()
	 */
	public void stop() {
		System.out.println("Application.stop() called");
		final IWorkbench workbench = PlatformUI.getWorkbench();
		if (workbench == null)
			return;
		em.tearDown();
		System.out.println("cs.tearDown called from Application.stop()");
		final Display display = workbench.getDisplay();
		display.syncExec(new Runnable() {
			public void run() {
				if (!display.isDisposed())
					workbench.close();
			}
		});
	}

	public static boolean isMonitoring() {
		return monitoring;
	}

	public static void setMonitoring(boolean monitoring) {
		Application.monitoring = monitoring;
	}
	
	private boolean getMonitoringPreference() {
        IPreferencesService service = Platform.getPreferencesService();
        return service.getBoolean(Application.PLUGIN_ID,
                        MonitoringPreferencePage.AUTO_MONITOR, false, null);

	}

	@Override
	public void earlyStartup() {
		// TODO Auto-generated method stub
		
	}
	
	private class ShutdownHook extends Thread {
		@Override
		public void run() {
			try {
				final IWorkbench workbench = PlatformUI.getWorkbench();
				final Display display = PlatformUI.getWorkbench()
				.getDisplay();
				if (workbench != null && !workbench.isClosing()) {
					display.syncExec(new Runnable() {
						public void run() {
							IWorkbenchWindow [] workbenchWindows = 
								workbench.getWorkbenchWindows();
							for(int i = 0;i < workbenchWindows.length;i++) {
								IWorkbenchWindow workbenchWindow =
									workbenchWindows[i];
								if (workbenchWindow == null) {
									// SIGTERM shutdown code must access
									// workbench using UI thread!!
								} else {
									em.tearDown();
									System.out.println("cs.tearDown called from ShutdownHook");
									IWorkbenchPage[] pages = workbenchWindow
									.getPages();
									for (int j = 0; j < pages.length; j++) {
										IEditorPart[] dirtyEditors = pages[j]
										                                   .getDirtyEditors();
										for (int k = 0; k < dirtyEditors.length; k++) {
											dirtyEditors[k]
											             .doSave(new NullProgressMonitor());
										}
									}
								}
							}
						}
					});
					display.syncExec(new Runnable() {
						public void run() {
							workbench.close();
						}
					});
				}
			} catch (IllegalStateException e) {
				// ignore
			}
		}
	}

}
