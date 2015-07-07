/*******************************************************************************
 * Copyright (c) 2010 - 2013 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Lars Vogel <lars.Vogel@gmail.com> - Bug 419770
 *******************************************************************************/
package alma.acs.alarmsystemprofiler.handlers;

import java.io.File;

import org.eclipse.e4.core.di.annotations.Execute;
import org.eclipse.swt.widgets.Shell;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.SWT;
import alma.acs.alarmsystemprofiler.save.SaveData;

public class SaveHandler {

	@Execute
	public void execute(Shell shell) {
		System.out.println("Saving...");
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