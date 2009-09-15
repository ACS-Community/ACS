package alma.acs.eventbrowser.views;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

public class CopyDetailsToClipboard extends AbstractHandler {

	@SuppressWarnings("unchecked")
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		IWorkbenchWindow window = PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow();
		IWorkbenchPage page = window.getActivePage();
		IViewPart view = page.findView(EventDetailView.ID);
		Clipboard cb = new Clipboard(Display.getDefault());
		ISelection selection = view.getSite().getSelectionProvider()
				.getSelection();
		List<ParsedAnyData> personList = new ArrayList<ParsedAnyData>();
		if (selection != null && selection instanceof IStructuredSelection) {
			IStructuredSelection sel = (IStructuredSelection) selection;
			for (Iterator<ParsedAnyData> iterator = ((ArrayList<ParsedAnyData>) sel)
					.iterator(); iterator.hasNext();) {
				ParsedAnyData person = iterator.next();
				personList.add(person);
			}
		}
		StringBuilder sb = new StringBuilder();
		for (ParsedAnyData person : personList) {
			sb.append(parsedAnyDataToString(person));
		}
		TextTransfer textTransfer = TextTransfer.getInstance();
		cb.setContents(new Object[] { sb.toString() },
				new Transfer[] { textTransfer });

		return null;
	}

	private String parsedAnyDataToString(ParsedAnyData person) {
		return person.getName() + "\t" + person.getType() + "\t"
				+ person.getValue() + System.getProperty("line.separator");
	}

}
