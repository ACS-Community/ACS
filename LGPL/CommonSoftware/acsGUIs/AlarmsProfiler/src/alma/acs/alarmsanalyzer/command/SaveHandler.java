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
package alma.acs.alarmsanalyzer.command;

import java.io.File;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.commands.IHandlerListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;

import alma.acs.alarmsanalyzer.save.SaveData;

public class SaveHandler implements IHandler {

	@Override
	public void addHandlerListener(IHandlerListener handlerListener) {
	}

	@Override
	public void dispose() {
	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		System.out.println("FileHandler.execute");
		Display display = Display.getCurrent();
		if (display!=null) {
			Shell shell = display.getActiveShell();
			if (shell!=null) {
				System.out.println("Saving");
				FileDialog fileChooser = new FileDialog(shell, SWT.SAVE);
				String fileName= fileChooser.open();
				if (fileName==null) {
					System.out.println("No file to save");
				} else {
					System.out.println("File to save: "+fileName);
					File fileToSave = new File(fileName);
					try {
						SaveData saveHelper = new SaveData(SaveData.FileContentType.WIKI, fileToSave);
					} catch (Throwable t) {
						System.err. println(t.getMessage());
						t.printStackTrace(System.err);
					}
				}
			}
		}
		return null;
	}

	@Override
	public boolean isEnabled() {
		return true;
	}

	@Override
	public boolean isHandled() {
		return true;
	}

	@Override
	public void removeHandlerListener(IHandlerListener handlerListener) {
	}

}
