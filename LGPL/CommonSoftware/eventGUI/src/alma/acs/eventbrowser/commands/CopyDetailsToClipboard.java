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
package alma.acs.eventbrowser.commands;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.handlers.HandlerUtil;

import alma.acs.eventbrowser.views.EventDetailView;
import alma.acs.eventbrowser.views.ParsedAnyData;

public class CopyDetailsToClipboard extends AbstractHandler {

	@SuppressWarnings("unchecked")
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		IWorkbenchPage page = HandlerUtil.getActiveWorkbenchWindow(event).getActivePage();

		Clipboard cb = new Clipboard(Display.getDefault());

		ISelection selection = page.getSelection(EventDetailView.ID);
		List<ParsedAnyData> parsedAnyList = new ArrayList<ParsedAnyData>();
		if (selection != null && selection instanceof IStructuredSelection) {
			IStructuredSelection sel = (IStructuredSelection) selection;
			for (Iterator iterator = sel.iterator(); iterator.hasNext();) {
				ParsedAnyData parsedAny = (ParsedAnyData)iterator.next();
				parsedAnyList.add(parsedAny);
			}
		} else
			return null;
		StringBuilder sb = new StringBuilder();
		for (ParsedAnyData parsedAny : parsedAnyList) {
			sb.append(parsedAnyDataToString(parsedAny));
		}
		if (parsedAnyList.size() <= 0) return null;
		TextTransfer textTransfer = TextTransfer.getInstance();
		cb.setContents(new Object[] { sb.toString() },
				new Transfer[] { textTransfer });

		return null;
	}

	private String parsedAnyDataToString(ParsedAnyData parsedAny) {
		return parsedAny.getName() + "\t" + parsedAny.getType() + "\t"
				+ parsedAny.getValue() + System.getProperty("line.separator");
	}

}
