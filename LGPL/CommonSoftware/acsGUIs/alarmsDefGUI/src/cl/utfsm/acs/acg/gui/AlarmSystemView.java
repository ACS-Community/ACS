/*
 *    ALMA - Atacama Large Millimeter Array
 *    (c) Universidad Tecnica Federico Santa Maria, 2009
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package cl.utfsm.acs.acg.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.part.ViewPart;

import cl.utfsm.acs.acg.core.AlarmSystemManager;

public class AlarmSystemView extends ViewPart implements IMyViewPart {

	public static String ID = "cl.utfsm.acs.acg.gui.alarmsystemview";
	
	private AlarmSystemManager _alarmSystemManager;

	/* Widget objects */
	private Group _comp;
	private Button _acsAS_radio;
	private Button _cernAS_radio;

	@Override
	public void createPartControl(Composite parent) {
		setTitleImage(Activator.getDefault().getImageRegistry().get(Activator.IMG_ALARM_SYSTEM));
		Listener selectAlarmSystem = new Listener(){
			public void handleEvent(Event event) {
				if(((Button)event.widget).getSelection()){
					if(_acsAS_radio.getSelection()){
						_alarmSystemManager.setConfigurationProperty("Implementation", "ACS");
						enableViews(false);
					}
					if(_cernAS_radio.getSelection()){
						_alarmSystemManager.setConfigurationProperty("Implementation", "CERN");
						enableViews(true);
					}
				}
			};
		};

		GridLayout layout = new GridLayout();
		layout.numColumns = 1;
		layout.makeColumnsEqualWidth = true;
		layout.marginTop = 20;
		layout.marginLeft = 20;
		_comp = new Group(parent, SWT.SHADOW_ETCHED_IN);
		_comp.setLayout(layout);
		_comp.setText("Alarm System Configuration");

		/* buttons that represent AS */ 
		_acsAS_radio = new Button (_comp, SWT.RADIO);
		_acsAS_radio.setText("ACS Alarm System");
		_acsAS_radio.addListener(SWT.Selection, selectAlarmSystem);
		_cernAS_radio = new Button (_comp, SWT.RADIO);
		_cernAS_radio.setText("CERN Alarm System");
		_cernAS_radio.addListener(SWT.Selection, selectAlarmSystem);
	}

	@Override
	public void setFocus() {
	}

	/* (non-Javadoc)
	 * @see cl.utfsm.acs.acg.gui.IMyViewPart#setEnabled(boolean)
	 */
	public void setEnabled(boolean v) {
		_comp.setEnabled(v);
		_acsAS_radio.setEnabled(v);
		_cernAS_radio.setEnabled(v);
		refreshContents();
	}

	/* (non-Javadoc)
	 * @see cl.utfsm.acs.acg.gui.IMyViewPart#refreshContents()
	 */
	public void refreshContents() {
		try{
			_alarmSystemManager = AlarmSystemManager.getInstance();
			String at = _alarmSystemManager.getConfigurationPrperty("Implementation");
			if(at != null){
				if(at.compareTo("ACS") == 0){
					_acsAS_radio.setSelection(true);
					enableViews(false);
				}
				else if(at.compareTo("CERN") == 0){
					_cernAS_radio.setSelection(true);
					enableViews(true);
				}
			}
		}catch(IllegalStateException e){			
		}
	}
	private void enableViews(boolean enable){
		IViewReference[] views = getSite().getWorkbenchWindow().getActivePage().getViewReferences();
		for (int i = 0; i < views.length; i++) {
			if( views[i].getView(false) instanceof IMyViewPart && !views[i].getView(false).equals(this))
				((IMyViewPart)views[i].getView(false)).setEnabled(enable);
			//getSite().getWorkbenchWindow().getActivePage().hideView(views[i]);
		}
	}

	@Override
	public void fillWidgets() {
		// TODO Auto-generated method stub
		
	}
	
	public void setReadOnly(boolean v){
		_acsAS_radio.setEnabled(!v);
		_cernAS_radio.setEnabled(!v);
	}
}
