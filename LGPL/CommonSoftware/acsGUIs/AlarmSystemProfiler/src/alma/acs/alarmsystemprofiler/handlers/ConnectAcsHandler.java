/*******************************************************************************
 * Copyright (c) 2010 IBM Corporation and others.
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

import org.eclipse.e4.core.di.annotations.Execute;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;

import alma.acs.alarmsystemprofiler.document.AnnunciatedContainer;
import alma.acs.alarmsystemprofiler.document.ChatteringAlarmsContainer;
import alma.acs.alarmsystemprofiler.document.LostSourcesContainer;
import alma.acs.alarmsystemprofiler.document.MostFrequantAlarmsContainer;
import alma.acs.alarmsystemprofiler.document.StaleAlarmsContainer;
import alma.acs.alarmsystemprofiler.document.StatisticsContainer;
import alma.acs.alarmsystemprofiler.document.SuppressedContainer;
import alma.acs.alarmsystemprofiler.document.TenMinutesContainer;
import alma.acs.alarmsystemprofiler.document.flood.FloodContainer;
import alma.acs.alarmsystemprofiler.engine.AcsSourceClient;
public class ConnectAcsHandler {

	@Execute
	public void execute(Shell shell){
		Thread t = new Thread(new Runnable() {
			public void run() {
				AcsSourceClient client;
				try {
					client=AcsSourceClient.getInstance();
				} catch (Throwable t) {
					t.printStackTrace();
					return;
				}
				client.addAlarmSourceListener(MostFrequantAlarmsContainer.getInstance());
				client.addAlarmSourceListener(StaleAlarmsContainer.getInstance());
				client.addAlarmSourceListener(ChatteringAlarmsContainer.getInstance());
				client.addAlarmSourceListener(TenMinutesContainer.getInstance());
				client.addAlarmSourceListener(StatisticsContainer.getInstance());
				client.addAlarmSourceListener(LostSourcesContainer.getInstance());
				client.addListener(StatisticsContainer.getInstance());
				client.addListener(SuppressedContainer.getInstance());
				client.addListener(AnnunciatedContainer.getInstance());
				client.addListener(LostSourcesContainer.getInstance());
				client.addListener(FloodContainer.getInstance());
				System.out.println("SourceListener added");
				try {
					client.connect();
					System.out.println("SourceClient connected");
				} catch (Throwable t) {
					t.printStackTrace();
				}
			}
		},"ConnectAcsThread");
		t.setDaemon(true);
		t.start();
	}
}
