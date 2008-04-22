package alma.acs.eventbrowser;

import java.util.concurrent.ArrayBlockingQueue;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.equinox.app.IApplication;
import org.eclipse.equinox.app.IApplicationContext;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;

import alma.acs.eventbrowser.preferences.MonitoringPreferencePage;
import alma.acs.eventbrowser.views.EventData;

/**
 * This class controls all aspects of the application's execution
 */
public class Application implements IApplication {
	
	public static final String PLUGIN_ID = "alma.acs.eventbrowser";
	
	public static ArrayBlockingQueue<EventData> equeue = new ArrayBlockingQueue<EventData>(50000);
	
	private static boolean monitoring = false;

	/* (non-Javadoc)
	 * @see org.eclipse.equinox.app.IApplication#start(org.eclipse.equinox.app.IApplicationContext)
	 */
	public Object start(IApplicationContext context) throws Exception {
		Display display = PlatformUI.createDisplay();
		try {
			setMonitoring(getMonitoringPreference());
			int returnCode = PlatformUI.createAndRunWorkbench(display, new ApplicationWorkbenchAdvisor());
			if (returnCode == PlatformUI.RETURN_RESTART)
				return IApplication.EXIT_RESTART;
			else
				return IApplication.EXIT_OK;
		} finally {
			display.dispose();
		}
		
	}

	/* (non-Javadoc)
	 * @see org.eclipse.equinox.app.IApplication#stop()
	 */
	public void stop() {
		final IWorkbench workbench = PlatformUI.getWorkbench();
		if (workbench == null)
			return;
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
}
