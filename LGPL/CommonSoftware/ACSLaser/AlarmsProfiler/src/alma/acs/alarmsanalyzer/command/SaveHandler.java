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
