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
package alma.acs.eventbrowser.handlers;

import javax.inject.Named;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.e4.core.di.annotations.CanExecute;
import org.eclipse.e4.core.di.annotations.Execute;
import org.eclipse.e4.core.di.annotations.Optional;
import org.eclipse.e4.ui.services.IServiceConstants;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.widgets.Display;

import alma.acs.eventbrowser.parts.ParsedAnyData;



public class CopyDetailsToClipboardHandler {

	@CanExecute
	public boolean canExecute(@Optional @Named(IServiceConstants.ACTIVE_SELECTION) ParsedAnyData[] parsedAnyData) throws ExecutionException {

		return ( parsedAnyData != null && parsedAnyData.length > 0 );
	}
	
	
	/**
	 * Receives the selection from the event detail part.
	 * <p>
	 * It may be my lack of understanding of good Eclipse 4 RCP practices that I wish
	 * I could handle the mouse menu action locally in the event detail part,
	 * instead of sending the data via selection service to separate handler.
	 * Perhaps to be refactored once this becomes clearer. 
	 */
	@Execute
	public void execute(@Named(IServiceConstants.ACTIVE_SELECTION) ParsedAnyData[] parsedAnyData) throws ExecutionException {

		if (parsedAnyData == null || parsedAnyData.length == 0) {
			return;
		}

		System.out.println("Will copy event details to the clipboard...");
		
		// Convert selected table rows into a multi-line String
		StringBuilder sb = new StringBuilder();
		for (ParsedAnyData parsedAny : parsedAnyData) {
			sb.append(parsedAnyDataToString(parsedAny));
		}
		
		// Write that data to the system clipboard
		Clipboard cb = new Clipboard(Display.getCurrent());
		TextTransfer textTransfer = TextTransfer.getInstance();
		cb.setContents(new Object[] { sb.toString() }, new Transfer[] { textTransfer });
		cb.dispose();
		
	}

	private String parsedAnyDataToString(ParsedAnyData parsedAny) {
		return parsedAny.getName() + "\t" + parsedAny.getType() + "\t"
				+ parsedAny.getValue() + System.getProperty("line.separator");
	}

}
