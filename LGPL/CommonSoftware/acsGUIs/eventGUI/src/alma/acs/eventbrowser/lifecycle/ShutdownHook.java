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
package alma.acs.eventbrowser.lifecycle;


/**
 * Took the ShutdownHook from alma.acs.eventbrowser.Application, but it is not
 * yet used.
 * 
 * TODO: Decide whether we need the ShutdownHook with e4.
 */
public class ShutdownHook extends Thread {
//	@Override
//	public void run() {
//		try {
//			final IWorkbench workbench = PlatformUI.getWorkbench();
//			final Display display = PlatformUI.getWorkbench().getDisplay();
//			if (workbench != null && !workbench.isClosing()) {
//				display.syncExec(new Runnable() { // todo http://www.vogella.com/articles/EclipseJobs/article.html
//					public void run() {
//						IWorkbenchWindow[] workbenchWindows = workbench.getWorkbenchWindows();
//						for (int i = 0; i < workbenchWindows.length; i++) {
//							IWorkbenchWindow workbenchWindow = workbenchWindows[i];
//							if (workbenchWindow == null) {
//								// SIGTERM shutdown code must access
//								// workbench using UI thread!!
//							} else {
//								em.tearDown();
//								System.out.println("cs.tearDown called from ShutdownHook");
//								IWorkbenchPage[] pages = workbenchWindow.getPages();
//								for (int j = 0; j < pages.length; j++) {
//									IEditorPart[] dirtyEditors = pages[j]
//											.getDirtyEditors();
//									for (int k = 0; k < dirtyEditors.length; k++) {
//										dirtyEditors[k]
//												.doSave(new NullProgressMonitor());
//									}
//								}
//							}
//						}
//					}
//				});
//				display.syncExec(new Runnable() {
//					public void run() {
//						workbench.close();
//					}
//				});
//			}
//		} catch (IllegalStateException e) {
//			// ignore
//		}
//	}
}
