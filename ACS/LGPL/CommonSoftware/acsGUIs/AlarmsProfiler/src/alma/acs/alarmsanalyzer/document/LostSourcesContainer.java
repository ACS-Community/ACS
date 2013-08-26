/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.alarmsanalyzer.document;

import java.util.Collections;
import java.util.Vector;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Display;

import cern.laser.client.data.Alarm;
import cern.laser.source.alarmsysteminterface.FaultState;
import alma.acs.alarmsanalyzer.engine.AlarmCategoryListener;
import alma.acs.alarmsanalyzer.engine.AlarmUtils;
import alma.acs.alarmsanalyzer.save.TableData;
import alma.alarmsystem.clients.source.SourceListener;

/**
 * The container for lost alarms i.e. all the alarms sent by sources and never
 * annunciated to the operator.
 * <P> 
 * These are the reason while an alarm is not shown to the operator:
 * <UL>
 * 	<LI>the client is not registered to the category where the alarm is published
 * 	<LI>the mapping between the source and the alarm is not in the CDB and the alarm service discards the source
 * </UL>
 * <P>
 * Lost alarms are the source IDs that do not compare as alarm IDs.
 * 
 * @author acaproni
 *
 */
public class LostSourcesContainer implements AlarmCategoryListener, SourceListener, IStructuredContentProvider  {
	
	/**
	 * The singleton
	 */
	private static LostSourcesContainer singleton=null;
	
	public static LostSourcesContainer getInstance() {
		if (singleton==null) {
			singleton = new LostSourcesContainer();
		}
		return singleton;
	}
	
	/**
	 * Stop updating if the viewer has been disposed
	 */
	private boolean disposed=false;
	
	/**
	 * All the IDs of source events received
	 */
	private final Vector<String>sourceIDs = new Vector<String>();
	
	/**
	 * All the IDs of the alarms received
	 */
	private final Vector<String>alarmsIDs = new Vector<String>();
	
	/**
	 * The list viewer
	 */
	private ListViewer viewer=null;
	
	/**
	 * Constructor
	 */
	private LostSourcesContainer() {}

	@Override
	public synchronized void faultStateReceived(FaultState faultState) {
		if (disposed) {
			return;
		}
		String ID=AlarmUtils.getID(faultState);
		System.out.print("Received "+ID);
		if (!sourceIDs.contains(ID)) {
			sourceIDs.add(ID);
		}
		if (viewer!=null) {
			refresh();
		}
	}

	@Override
	public void sourceXMLMsgReceived(String asiMessage) {}

	@Override
	public synchronized void onAlarm(Alarm alarm) {
		if (disposed) {
			return;
		}
		String ID=alarm.getAlarmId();
		if (!alarmsIDs.contains(ID)) {
			alarmsIDs.add(ID);
		}
		if (viewer!=null) {
			refresh();
		}
	}
	
	@Override
	public void dispose() {
		disposed=true;
		alarmsIDs.clear();
		sourceIDs.clear();
	}
	
	public void setViewer(ListViewer viewer) {
		this.viewer=viewer;
	}

	@Override
	public synchronized void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
	}

	@Override
	public Object[] getElements(Object inputElement) {
		Vector<String> ret = new Vector<String>();
		for (String str: sourceIDs) {
			if (!alarmsIDs.contains(str)) {
				ret.add(str);
			}
		}
		return ret.toArray();
	}
	
	/**
	 * Refresh the content of the list
	 */
	private void refresh() {
		if (!disposed) {
			Display display=Display.getDefault();
			display.syncExec(
				  new Runnable() {
				    public void run(){
				    	if (!disposed) {
				    		viewer.refresh();
				    	}
				    }
			  });
		}
	}
	
	/**
	 * @return The data to be saved
	 */
	public TableData getDataToSave() {
		String[] title = {
				"Source ID"
		};
		TableData tData = new TableData("Lost sources", title);
		Vector<String> ids = new Vector<String>(sourceIDs);
		Collections.sort(ids);
		for (String id: ids) {
			String[] row = new String[1];
			row[0]="="+id+"=";
			tData.addRowData(row);
		}
		return tData;
	}
}
