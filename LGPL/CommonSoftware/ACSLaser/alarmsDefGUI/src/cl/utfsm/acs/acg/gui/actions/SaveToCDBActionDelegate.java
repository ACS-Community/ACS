/*
 *    ALMA - Atacama Large Millimeter Array
 *    (c) Universidad Tecnica Federico Santa Maria, 2009
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
package cl.utfsm.acs.acg.gui.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

import cl.utfsm.acs.acg.core.AlarmSystemManager;
import cl.utfsm.acs.acg.gui.IMyViewPart;

/**
 * @author 
 *
 */
public class SaveToCDBActionDelegate implements IWorkbenchWindowActionDelegate {

	private IWorkbenchWindow _window;

	@Override
	public void dispose() {
		
	}

	@Override
	public void init(IWorkbenchWindow window) {
		_window = window;
	}

	@Override
	public void run(IAction action) {

		if( _window == null )
			return;

		boolean confirmSave;
		confirmSave = MessageDialog.openQuestion(_window.getShell(), "Save to CDB",
				"Save the new contents to the CDB?");

		if( !confirmSave )
			return;

		final Display display = _window.getShell().getDisplay();

		new Thread(new Runnable(){

			public void run() {

				display.syncExec(new Runnable() {
					public void run() {
						// Disable all views
						IViewReference[] views = _window.getActivePage().getViewReferences();
						for (int i = 0; i < views.length; i++) {
							if( views[i].getView(false) instanceof IMyViewPart )
								((IMyViewPart)views[i].getView(false)).setEnabled(false);
						}
					}
				});

				// Save and reload information from the CDB
				AlarmSystemManager.getInstance().saveToCDB();

				display.asyncExec(new Runnable() {
					public void run() {
						// Enable all views, and reload their contents
						IViewReference[] views = _window.getActivePage().getViewReferences();
						for (int i = 0; i < views.length; i++) {
							if( views[i].getView(false) instanceof IMyViewPart ) {
								IMyViewPart view = ((IMyViewPart)views[i].getView(false));
								view.setEnabled(true);
								view.refreshContents();
							}
						}						
					}
				});

			}

		}).start();

	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		
	}

}