/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2013
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

package alma.acs.eventbrowser.status;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.inject.Inject;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.e4.core.di.annotations.Optional;
import org.eclipse.e4.ui.di.UIEventTopic;
import org.eclipse.e4.ui.di.UISynchronize;
import org.eclipse.e4.ui.model.application.ui.menu.MToolControl;
import org.eclipse.jface.action.StatusLineManager;
import org.eclipse.swt.widgets.Composite;

/**
 * As of Eclipse 4.2, it is not well documented how to implement a status bar.
 * There seems to be an improvement over E3 though, that the status bar is no longer tied to a view
 * and thus can display messages from async jobs etc.
 * http://www.eclipse.org/forums/index.php/m/962170/ (StatusLineManager
 * http://www.vogella.com/articles/EclipseJobs/article.html
 * http://www.eclipse.org/forums/index.php/m/753670/
 * <p>
 * Note that "@Inject IStatusLineManager" (as described in http://wiki.eclipse.org/Eclipse4/RCP/FAQ#Accessing_the_status_line and elsewhere)
 * does not work, see also https://bugs.eclipse.org/bugs/show_bug.cgi?id=332499
 * 
 * @author hsommer
 *
 */
public class MyStatusBar {
	
	public static final String STATUS_BAR_TOPIC_ID = "statusbar";
	
	/**
	 * @see StatusLineWriter#flashMessage(String, int)
	 */
	public static class MessageWithTime {
		String msg;
		int timeSeconds;
		public MessageWithTime(String msg, int timeSeconds) {
			this.msg = msg;
			this.timeSeconds = timeSeconds;
		}
	}
	
	@Inject 
	private UISynchronize uiSync;
	
	/**
	 * The SWT or jface control. Alternatively use a Label.
	 */
	private StatusLineManager slm;
	
	/**
	 * 
	 */
	private String permanentMessage = "";

	/**
	 * Is null whenever we do not display a flash message.
	 */
	private Job msgRestoreJob;
	
	@PostConstruct
	void  createControls(Composite parent, MToolControl toolControl) {
		if (!toolControl.isVisible()) {
			System.out.println("Skipping creation of status bar as it is marked invisible in the model.");
			return;
		}
		slm = new StatusLineManager();
		slm.createControl(parent);
		slm.setMessage("Refresh service data to get correct supplier/consumer info.");
	}

	/**
	 * See http://www.vogella.com/articles/Eclipse4EventSystem/article.html 
	 * Alternatively an object can also register an instance of the EventHandler directly with the IEventBroker 
	 * via the subscribe() method. 
	 * Using dependency injection for subscribing should be preferred compared to the direct subscription. 
	 * @param s
	 */
	@Inject
	@Optional
	private void getNotified(@UIEventTopic(STATUS_BAR_TOPIC_ID) String s) {
		if (slm != null) {
			slm.setMessage(s);
			permanentMessage = s;
			clearFlashJob();
		}
	}

	@Inject
	@Optional
	private void getNotified(@UIEventTopic(STATUS_BAR_TOPIC_ID) MessageWithTime msgWithTime) {
		if (slm != null) {
			String flashMsg = msgWithTime.msg;
			int timeSeconds = msgWithTime.timeSeconds;
			slm.setMessage(flashMsg);
			clearFlashJob();
			msgRestoreJob = new Job(MyStatusBar.class.getSimpleName() + "RemoveFlashMessage") {
				@Override
				protected IStatus run(IProgressMonitor monitor) {
					// restore the previous message
					uiSync.syncExec(new Runnable() {
						@Override
						public void run() {
							slm.setMessage(permanentMessage);
						}
					});
					return Status.OK_STATUS;
				}
			};
			msgRestoreJob.schedule(timeSeconds*1000);
		}
	}

	private synchronized void clearFlashJob() {
		if (msgRestoreJob != null) {
			msgRestoreJob.cancel();
			msgRestoreJob = null;
		}
	}
	
	
	@PreDestroy
	public void preDestroy() {
		slm.removeAll();
		slm.dispose();
	}
}
