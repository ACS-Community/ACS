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

import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.part.ViewPart;

import cern.laser.business.data.Alarm;
import cern.laser.business.data.Triplet;
import alma.acs.alarmsystem.generated.FaultCode;
import alma.acs.alarmsystem.generated.FaultFamily;
import alma.acs.alarmsystem.generated.FaultMember;

import cl.utfsm.acs.acg.core.AlarmSystemManager;
import cl.utfsm.acs.acg.core.AlarmManager;
import cl.utfsm.acs.acg.core.ReductionManager;
import cl.utfsm.acs.acg.core.ReductionRule;
public class ReductionsView extends ViewPart implements IMyViewPart {
	
	public static String ID = "cl.utfsm.acs.acg.gui.reductionsview";

	private enum NodeType {
		NODE_REDUCTION,
		NODE_REDUCTION_PARENT_DATA,
		NODE_REDUCTION_CHILDREN_DATA,
		MULTIPLICITY_REDUCTION,
		MULTIPLICITY_REDUCTION_PARENT_DATA,
		MULTIPLICITY_REDUCTION_CHILDREN_DATA
	}

	private ReductionManager _reductionManager;
	private AlarmManager _alarmManager;

	/* High level widgets */
	private SashForm _sash;
	private Tree _tree;
	private Composite _compInitial;

	/* NR Parent Widgets */
	private Group _NRParentGroup;
	private Group _NRParentFtGroup;
	private Label _NRParentFFLabel;
	private Combo _NRParentFFCombo;
	private Label _NRParentFMLabel;
	private Combo _NRParentFMCombo;
	private Label _NRParentFCLabel;
	private Combo _NRParentFCCombo;
	//
	private Group _NRParentChGroup;
	private Group _NRParentChFilterGroup;
	private Label _NRParentChFFLabel;
	private Combo _NRParentChFFCombo;
	private Label _NRParentChFMLabel;
	private Combo _NRParentChFMCombo;
	private Label _NRParentChFCLabel;
	private Combo _NRParentChFCCombo;
	private Label  _NRParentChFilterLabel;
	private Text  _NRParentChFilterText;
	private Table _NRParentChAlarmList;
	
	/* NR Children Widgets */
	private Group _NRChildrenGroup;
	private Label _NRChildrenFFLabel;
	private Label _NRChildrenFFName;
	private Label _NRChildrenFMLabel;
	private Label _NRChildrenFMName;
	private Label _NRChildrenFCLabel;
	private Label _NRChildrenFCName;

	/* MR Parent Widgets */
	private Group _MRParentGroup;
	private Group _MRParentFtGroup;
	private Label _MRParentFFLabel;
	private Combo _MRParentFFCombo;
	private Label _MRParentFMLabel;
	private Combo _MRParentFMCombo;
	private Label _MRParentFCLabel;
	private Combo _MRParentFCCombo;
	//
	private Group _MRParentChGroup;
	private Group _MRParentChFilterGroup;
	private Label _MRParentChFFLabel;
	private Combo _MRParentChFFCombo;
	private Label _MRParentChFMLabel;
	private Combo _MRParentChFMCombo;
	private Label _MRParentChFCLabel;
	private Combo _MRParentChFCCombo;
	private Table _MRParentChAlarmList;

	/* MR Children Widgets */
	private Group _MRChildrenGroup;
	private Label _MRChildrenFFLabel;
	private Label _MRChildrenFFName;
	private Label _MRChildrenFMLabel;
	private Label _MRChildrenFMName;
	private Label _MRChildrenFCLabel;
	private Label _MRChildrenFCName;
	
	/* Listeners */
	private Listener _addElement;
	private Listener _addRule;
	private Listener _removeElement;
	private Listener _updateNRParentFF;
	private Listener _updateNRParent;
	private Listener _updateNRParentChFF;
	private Listener _updateNRParentCh;
	private Listener _updateMRParentFF;
	private Listener _updateMRParent;
	private Listener _updateMRParentChFF;
	private Listener _updateMRParentCh;

	@Override
	public void createPartControl(Composite parent) {
		setTitleToolTip("Configuration of Reduction Rules.\nReduction Rules link two or more alarms in a father/son relationship.");
		setTitleImage(Activator.getDefault().getImageRegistry().get(Activator.IMG_REDUCTIONS));
		createViewWidgets(parent);
		refreshContents();

	}
	
	private void createViewWidgets(Composite parent) {
		_addElement = new Listener() {
			public void handleEvent(Event event) {
			}
		};
		
		_addRule = new Listener() {
			public void handleEvent(Event event) {
				if(event.type == SWT.KeyUp)
					if(!(event.keyCode == SWT.CR || event.keyCode == ' '))
						return;
				
				if(event.type == SWT.MouseDoubleClick){
					Point pt = new Point(event.x,event.y);
					if(_NRParentChAlarmList.getItem(pt) == null)
						return;
				}
				TreeItem[] tmp1 = _tree.getSelection();
				if(tmp1 == null || tmp1.length == 0)
					return;
				String[] tr = getTriplet(tmp1[0].getText());
				ReductionRule parent = _reductionManager.getNRParentByTriplet(tr[0], tr[1], Integer.parseInt(tr[2]));
				
				TableItem[] tmp2 = _NRParentChAlarmList.getSelection();
				if(tmp2 == null || tmp2.length == 0)
					return;
				TableItem item = tmp2[0];
				Alarm p = parent.getParent();
				Alarm c = parent.getChild(item.getText());
				if(c == null){
					//Add child
					c = _alarmManager.getAlarm(item.getText());
					if(c == null)
						return;
					_reductionManager.addNodeReductionRule(p, c);
					item.setImage(Activator.getDefault().getImageRegistry().get(Activator.IMG_TICKET));
				}
				else{
					//Remove child
					_reductionManager.deleteNodeReductionRule(p, c);
					item.setImage((org.eclipse.swt.graphics.Image)null);
				}
			}
		};
		
		_removeElement = new Listener() {
			public void handleEvent(Event event) {
			    boolean choice = MessageDialog.openQuestion( 
		        		  ReductionsView.this.getViewSite().getShell(),
		            "Confirmation",
		            "Are you sure you want to delete this element");
			      if (choice == true ){ 
				
				TreeItem sel = null;
				if(_tree.getSelection() == null || _tree.getSelection().length == 0)
					return;
				NodeType type = (NodeType)_tree.getSelection()[0].getData();
				if(type == NodeType.NODE_REDUCTION || type == NodeType.MULTIPLICITY_REDUCTION)
					return;
				TreeItem item = _tree.getSelection()[0];
				String[] tr = getTriplet(item.getText());
				try {
					if(type == NodeType.NODE_REDUCTION_PARENT_DATA){
						//Remove all the NODE REDUCTION Rules in which this node is a parent.
						ReductionRule rr = _reductionManager.getNRParentByTriplet(tr[0], tr[1], Integer.parseInt(tr[2]));
						Alarm p = rr.getParent();
						List<Alarm> chL = rr.getChildren();
						for (Alarm c : chL) {
							_reductionManager.deleteNodeReductionRule(p, c);
						}
					}
					else if(type == NodeType.NODE_REDUCTION_CHILDREN_DATA){
						//Remove the selected NODE REDUCTION Rule.
						String[] tr2 = getTriplet(item.getParentItem().getText());
						ReductionRule rr = _reductionManager.getNRParentByTriplet(tr2[0], tr2[1], Integer.parseInt(tr2[2]));
						Alarm p = rr.getParent();
						Alarm c = rr.getChild(tr[0], tr[1], Integer.parseInt(tr[2]));
						if(c == null)
							return;
						_reductionManager.deleteNodeReductionRule(p, c);
					}
					else if(type == NodeType.MULTIPLICITY_REDUCTION_PARENT_DATA){
						//Remove all the MULTIPLICITY REDUCTION Rules in which this node is a parent.
						ReductionRule rr = _reductionManager.getMRParentByTriplet(tr[0], tr[1], Integer.parseInt(tr[2]));
						Alarm p = rr.getParent();
						List<Alarm> chL = rr.getChildren();
						for (Alarm c : chL) {
							_reductionManager.deleteNodeReductionRule(p, c);
						}
					}
					else if(type == NodeType.MULTIPLICITY_REDUCTION_CHILDREN_DATA){
						//Remove the selected MULTIPLICITY REDUCTION Rule.
						String[] tr2 = getTriplet(item.getParentItem().getText());
						ReductionRule rr = _reductionManager.getMRParentByTriplet(tr2[0], tr2[1], Integer.parseInt(tr2[2]));
						Alarm p = rr.getParent();
						Alarm c = rr.getChild(tr[0], tr[1], Integer.parseInt(tr[2]));
						if(c == null)
							return;
						_reductionManager.deleteMultiReductionRule(p, c);
					}
				} catch (NullPointerException e){
					ErrorDialog error = new ErrorDialog(ReductionsView.this.getViewSite().getShell(),
							"Cannot delete the item",
							e.getMessage(),
							new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.toString()),
							IStatus.ERROR);
					error.setBlockOnOpen(true);
					error.open();
				}
				sel = _tree.getSelection()[0].getParentItem();
				_tree.getSelection()[0].dispose();
				if(type == NodeType.NODE_REDUCTION_CHILDREN_DATA || type == NodeType.MULTIPLICITY_REDUCTION_CHILDREN_DATA){
					if(sel.getItemCount() == 0){
						TreeItem del = sel;
						sel = sel.getParentItem();
						del.dispose();
					}
				}
				_tree.setSelection(sel);
				Event e = new Event();
				_tree.notifyListeners(SWT.Selection, e);
			}
		}
			
		};
		_sash = new SashForm(parent,SWT.NONE);

		_tree = new Tree(_sash,SWT.VIRTUAL | SWT.BORDER);
		Menu treePopUp = new Menu(parent);
		MenuItem treePopUpDelete = new MenuItem(treePopUp,SWT.PUSH);
		treePopUpDelete.setText("Delete");
		treePopUpDelete.addListener(SWT.Selection, _removeElement);
		MenuItem treePopUpAddRule = new MenuItem(treePopUp,SWT.PUSH);
		treePopUpAddRule.setText("Add Rule");
		//treePopUpAddRule.addListener(SWT.Selection, _addRule);
		MenuItem treePopUpAddChildren = new MenuItem(treePopUp,SWT.PUSH);
		treePopUpAddChildren.setText("Add Children");
		treePopUpAddChildren.addListener(SWT.Selection, _addElement);
		_tree.setMenu(treePopUp);
		_tree.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}

			public void widgetSelected(SelectionEvent e) {
				TreeItem []tmp = ((Tree)e.widget).getSelection();
				if( tmp == null || tmp.length == 0 )
					return;

				TreeItem item = tmp[0];
				NodeType type = (NodeType)item.getData();
				Control c = _compInitial.getChildren()[0];

				if( c instanceof Label ) {
					c.dispose();
					_compInitial.layout();
					c = _compInitial.getChildren()[0];
				}

				if( type == NodeType.NODE_REDUCTION ) {
					_NRParentGroup.setVisible(false);
					_NRChildrenGroup.setVisible(false);
					_MRParentGroup.setVisible(false);
					_MRChildrenGroup.setVisible(false);					
				}
				else if( type == NodeType.NODE_REDUCTION_PARENT_DATA ) {
					_NRParentGroup.setVisible(true);
					_NRChildrenGroup.setVisible(false);
					_MRParentGroup.setVisible(false);
					_MRChildrenGroup.setVisible(false);
					
					String[] triplet = getTriplet(item.getText());
					fillNRParentWidgets(triplet[0],triplet[1],Integer.parseInt(triplet[2]));
					_NRParentGroup.moveAbove(c);
					_compInitial.layout();
				}
				else if( type == NodeType.NODE_REDUCTION_CHILDREN_DATA ) {
					_NRParentGroup.setVisible(false);
					_NRChildrenGroup.setVisible(true);
					_MRParentGroup.setVisible(false);
					_MRChildrenGroup.setVisible(false);
					
					String[] triplet = getTriplet(item.getParentItem().getText());
					String[] triplet2 = getTriplet(item.getText());
					fillNRChildrenWidgets(triplet[0],triplet[1],Integer.parseInt(triplet[2]),triplet2[0],triplet2[1],Integer.parseInt(triplet2[2]));
					_NRChildrenGroup.moveAbove(c);
					_compInitial.layout();
				}
				else if( type == NodeType.MULTIPLICITY_REDUCTION ){
					_NRParentGroup.setVisible(false);
					_NRChildrenGroup.setVisible(false);
					_MRParentGroup.setVisible(false);
					_MRChildrenGroup.setVisible(false);
				}
				else if( type == NodeType.MULTIPLICITY_REDUCTION_PARENT_DATA ) {
					_NRParentGroup.setVisible(false);
					_NRChildrenGroup.setVisible(false);
					_MRParentGroup.setVisible(true);
					_MRChildrenGroup.setVisible(false);

					String[] triplet = getTriplet(item.getText());
					fillMRParentWidgets(triplet[0],triplet[1],Integer.parseInt(triplet[2]));

					_MRParentGroup.moveAbove(c);
					_compInitial.layout();
				}
				else if( type == NodeType.MULTIPLICITY_REDUCTION_CHILDREN_DATA){
					_NRParentGroup.setVisible(false);
					_NRChildrenGroup.setVisible(false);
					_MRParentGroup.setVisible(false);
					_MRChildrenGroup.setVisible(true);

					String[] triplet = getTriplet(item.getParentItem().getText());
					String[] triplet2 = getTriplet(item.getText());
					fillMRChildrenWidgets(triplet[0],triplet[1],Integer.parseInt(triplet[2]),triplet2[0],triplet2[1],Integer.parseInt(triplet2[2]));
					_MRChildrenGroup.moveAbove(c);
					_compInitial.layout();
				}
			}
		});

		/* Top widget of the right side */
		_compInitial = new Composite(_sash,SWT.NONE);
		_compInitial.setLayout(new GridLayout());

		new Label(_compInitial,SWT.NONE).setText("Select a reduction rule");

		/* NR/MR Details */
		createNRParentWidgets();
		createNRChildrenWidgets();
		createMRParentWidgets();
		createMRChildrenWidgets();

		_NRParentGroup.setVisible(false);
		_NRChildrenGroup.setVisible(false);
		_MRParentGroup.setVisible(false);
		_MRChildrenGroup.setVisible(false);
		_sash.setWeights(new int[] {3, 5});
	}

	private void createNRParentWidgets() {
		_updateNRParentFF = new Listener() {
			public void handleEvent(Event event){
				_NRParentFMCombo.removeAll();
				FaultMember[] fml = _alarmManager.getFaultFamily(_NRParentFFCombo.getText()).getFaultMember();
				for (int j = 0; j < fml.length; j++) {
					_NRParentFMCombo.add(fml[j].getName());
				}
				_NRParentFCCombo.removeAll();
				FaultCode[] fcl = _alarmManager.getFaultFamily(_NRParentFFCombo.getText()).getFaultCode();
				for (int j = 0; j < fcl.length; j++) {
					_NRParentFCCombo.add(String.valueOf(fcl[j].getValue()));
				}
				System.out.println("Widget: " + event.widget.toString());
			}
		};
		_updateNRParent = new Listener() {
			public void handleEvent(Event event) {
				if(_tree.getSelection() == null || _tree.getSelection().length == 0)
					return;
				NodeType type = (NodeType)_tree.getSelection()[0].getData();
				if(type != NodeType.NODE_REDUCTION_PARENT_DATA)
					return;
				if(_NRParentFMCombo.getText().compareTo("") == 0 || _NRParentFCCombo.getText().compareTo("") == 0)
					return;
				String[] tr = getTriplet(_tree.getSelection()[0].getText());
				ReductionRule nrr = _reductionManager.getNRParentByTriplet(tr[0], tr[1], Integer.parseInt(tr[2]));
				ReductionRule nrr2 = _reductionManager.getNRParentByTriplet(_NRParentFFCombo.getText(), _NRParentFMCombo.getText(), Integer.parseInt(_NRParentFCCombo.getText()));
				if(nrr == null || nrr2 == null)
					return;
				//If nrr2 already exist, ask if it should merge both and dispose this one if affirmative.
				//TODO
				Alarm parent = nrr.getParent();
				Alarm nParent = nrr2.getParent();
				String[] ch = parent.getNodeChildren();
				//Remove all Children from nrr and add them to nrr2.
				for (int i = 0; i < ch.length; i++) {
					Alarm aCh = _alarmManager.getAlarm(ch[i]);
					if(aCh != null){
						parent.removeNodeChild(aCh);
						nParent.addNodeChild(aCh);
					}
				}
			}
		};
		_updateNRParentChFF = new Listener() {
			public void handleEvent(Event event){
				_NRParentChFMCombo.removeAll();
				_NRParentChFMCombo.add("Any");
				if(_NRParentChFFCombo.getText().compareTo("Any")!=0){
					FaultMember[] fml = _alarmManager.getFaultFamily(_NRParentChFFCombo.getText()).getFaultMember();
					for (int j = 0; j < fml.length; j++) {
						_NRParentChFMCombo.add(fml[j].getName());
					}
				}
				_NRParentChFMCombo.select(0);
				
				_NRParentChFCCombo.removeAll();
				_NRParentChFCCombo.add("Any");
				if(_NRParentChFFCombo.getText().compareTo("Any")!=0){
					FaultCode[] fcl = _alarmManager.getFaultFamily(_NRParentChFFCombo.getText()).getFaultCode();
					for (int j = 0; j < fcl.length; j++) {
						_NRParentChFCCombo.add(String.valueOf(fcl[j].getValue()));
					}
				}
				_NRParentChFCCombo.select(0);
				
				TreeItem []tmp = _tree.getSelection();
				if( tmp == null || tmp.length == 0 )
					return;

				TreeItem item = tmp[0];
				String[] triplet = getTriplet(item.getText());
				fillNRParentChAlarmList(triplet[0],triplet[1],Integer.parseInt(triplet[2]));
			}
		};
		_updateNRParentCh = new Listener() {
			public void handleEvent(Event event){
				TreeItem []tmp = _tree.getSelection();
				if( tmp == null || tmp.length == 0 )
					return;

				TreeItem item = tmp[0];
				String[] triplet = getTriplet(item.getText());
				fillNRParentChAlarmList(triplet[0],triplet[1],Integer.parseInt(triplet[2]));
			}
		};
		_NRParentGroup = new Group(_compInitial, SWT.SHADOW_ETCHED_IN);
		GridLayout gl = new GridLayout();
		gl.numColumns = 1;
		GridData gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.grabExcessVerticalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		gd.verticalAlignment = SWT.FILL;
		_NRParentGroup.setLayout(gl);
		_NRParentGroup.setLayoutData(gd);

		_NRParentFtGroup = new Group(_NRParentGroup, SWT.SHADOW_ETCHED_IN);
		gl = new GridLayout();
		gl.numColumns = 2;
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_NRParentFtGroup.setLayout(gl);
		_NRParentFtGroup.setLayoutData(gd);
		_NRParentFtGroup.setText("Primary Alarm");
		
		_NRParentFFLabel = new Label(_NRParentFtGroup,SWT.NONE);
		_NRParentFFLabel.setText("Fault Family:");
		_NRParentFFCombo = new Combo(_NRParentFtGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_NRParentFFCombo.setLayoutData(gd);
		_NRParentFFCombo.addListener(SWT.Selection, _updateNRParentFF);

		_NRParentFMLabel = new Label(_NRParentFtGroup,SWT.NONE);
		_NRParentFMLabel.setText("Fault Member:");
		_NRParentFMCombo = new Combo(_NRParentFtGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_NRParentFMCombo.setLayoutData(gd);
		_NRParentFMCombo.addListener(SWT.Selection, _updateNRParent);

		_NRParentFCLabel = new Label(_NRParentFtGroup,SWT.NONE);
		_NRParentFCLabel.setText("Fault Code:");
		_NRParentFCCombo = new Combo(_NRParentFtGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_NRParentFCCombo.setLayoutData(gd);
		_NRParentFCCombo.addListener(SWT.Selection, _updateNRParent);
//
		_NRParentChGroup = new Group(_NRParentGroup, SWT.SHADOW_ETCHED_IN);
		gl = new GridLayout();
		gl.numColumns = 1;
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.grabExcessVerticalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		gd.verticalAlignment = SWT.FILL;
		_NRParentChGroup.setLayout(gl);
		_NRParentChGroup.setLayoutData(gd);
		_NRParentChGroup.setText("Alarms to Ignore");
		
		_NRParentChFilterGroup = new Group(_NRParentChGroup, SWT.SHADOW_ETCHED_IN);
		gl = new GridLayout();
		gl.numColumns = 2;
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_NRParentChFilterGroup.setLayout(gl);
		_NRParentChFilterGroup.setLayoutData(gd);
		_NRParentChFilterGroup.setText("Filter Options");
		
		_NRParentChFFLabel = new Label(_NRParentChFilterGroup,SWT.NONE);
		_NRParentChFFLabel.setText("Fault Family:");
		_NRParentChFFCombo = new Combo(_NRParentChFilterGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_NRParentChFFCombo.setLayoutData(gd);
		_NRParentChFFCombo.addListener(SWT.Selection, _updateNRParentChFF);

		_NRParentChFMLabel = new Label(_NRParentChFilterGroup,SWT.NONE);
		_NRParentChFMLabel.setText("Fault Member:");
		_NRParentChFMCombo = new Combo(_NRParentChFilterGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_NRParentChFMCombo.setLayoutData(gd);
		_NRParentChFMCombo.addListener(SWT.Selection, _updateNRParentCh);

		_NRParentChFCLabel = new Label(_NRParentChFilterGroup,SWT.NONE);
		_NRParentChFCLabel.setText("Fault Code:");
		_NRParentChFCCombo = new Combo(_NRParentChFilterGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_NRParentChFCCombo.setLayoutData(gd);
		_NRParentChFCCombo.addListener(SWT.Selection, _updateNRParentCh);
		
		_NRParentChFilterLabel = new Label(_NRParentChFilterGroup,SWT.NONE);
		_NRParentChFilterLabel.setText("Filter RegEx:");
		_NRParentChFilterText = new Text(_NRParentChFilterGroup, SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_NRParentChFilterText.setLayoutData(gd);
		_NRParentChFilterText.addListener(SWT.Modify, _updateNRParentCh);
		
		_NRParentChAlarmList = new Table(_NRParentChGroup,SWT.BORDER);
		gd = new GridData();
		gd.verticalAlignment = SWT.FILL;
		gd.horizontalAlignment = SWT.FILL;
		gd.grabExcessVerticalSpace = true;
		gd.grabExcessHorizontalSpace = true;
		_NRParentChAlarmList.setLayoutData(gd);
		_NRParentChAlarmList.addListener(SWT.Selection, _addRule);
	}

	private void createNRChildrenWidgets() {

		_NRChildrenGroup = new Group(_compInitial, SWT.SHADOW_ETCHED_IN);
		GridLayout gl = new GridLayout();
		gl.numColumns = 2;
		GridData gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_NRChildrenGroup.setLayout(gl);
		_NRChildrenGroup.setLayoutData(gd);

		_NRChildrenFFLabel = new Label(_NRChildrenGroup,SWT.NONE);
		_NRChildrenFFLabel.setText("Fault Family:");
		_NRChildrenFFName = new Label(_NRChildrenGroup,SWT.NONE);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_NRChildrenFFName.setLayoutData(gd);

		_NRChildrenFMLabel = new Label(_NRChildrenGroup,SWT.NONE);
		_NRChildrenFMLabel.setText("Fault Member:");
		_NRChildrenFMName = new Label(_NRChildrenGroup,SWT.NONE);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_NRChildrenFMName.setLayoutData(gd);

		_NRChildrenFCLabel = new Label(_NRChildrenGroup,SWT.NONE);
		_NRChildrenFCLabel.setText("Fault Code:");
		_NRChildrenFCName = new Label(_NRChildrenGroup,SWT.NONE);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_NRChildrenFCName.setLayoutData(gd);

	}
	
	private void createMRParentWidgets() {
		_updateMRParentFF = new Listener() {
			public void handleEvent(Event event){
				_MRParentFMCombo.removeAll();
				FaultMember[] fml = _alarmManager.getFaultFamily(_MRParentFFCombo.getText()).getFaultMember();
				for (int j = 0; j < fml.length; j++) {
					_MRParentFMCombo.add(fml[j].getName());
				}
				_MRParentFCCombo.removeAll();
				FaultCode[] fcl = _alarmManager.getFaultFamily(_MRParentFFCombo.getText()).getFaultCode();
				for (int j = 0; j < fcl.length; j++) {
					_MRParentFCCombo.add(String.valueOf(fcl[j].getValue()));
				}
				System.out.println("Widget: " + event.widget.toString());
			}
		};
		_updateMRParent = new Listener() {
			public void handleEvent(Event event) {
				if(_tree.getSelection() == null || _tree.getSelection().length == 0)
					return;
				NodeType type = (NodeType)_tree.getSelection()[0].getData();
				if(type != NodeType.NODE_REDUCTION_PARENT_DATA)
					return;
				if(_MRParentFMCombo.getText().compareTo("") == 0 || _MRParentFCCombo.getText().compareTo("") == 0)
					return;
				String[] tr = getTriplet(_tree.getSelection()[0].getText());
				ReductionRule nrr = _reductionManager.getMRParentByTriplet(tr[0], tr[1], Integer.parseInt(tr[2]));
				ReductionRule nrr2 = _reductionManager.getMRParentByTriplet(_MRParentFFCombo.getText(), _MRParentFMCombo.getText(), Integer.parseInt(_MRParentFCCombo.getText()));
				if(nrr == null || nrr2 == null)
					return;
				//If nrr2 already exist, ask if it should merge both and dispose this one if affirmative.
				//TODO
				Alarm parent = nrr.getParent();
				Alarm nParent = nrr2.getParent();
				String[] ch = parent.getNodeChildren();
				//Remove all Children from nrr and add them to nrr2.
				for (int i = 0; i < ch.length; i++) {
					Alarm aCh = _alarmManager.getAlarm(ch[i]);
					if(aCh != null){
						parent.removeNodeChild(aCh);
						nParent.addNodeChild(aCh);
					}
				}
			}
		};
		_updateMRParentChFF = new Listener() {
			public void handleEvent(Event event){
				_MRParentChFMCombo.removeAll();
				_MRParentChFMCombo.add("Any");
				if(_MRParentChFFCombo.getText().compareTo("Any")!=0){
					FaultMember[] fml = _alarmManager.getFaultFamily(_MRParentChFFCombo.getText()).getFaultMember();
					for (int j = 0; j < fml.length; j++) {
						_MRParentChFMCombo.add(fml[j].getName());
					}
				}
				_MRParentChFMCombo.select(0);
				
				_MRParentChFCCombo.removeAll();
				_MRParentChFCCombo.add("Any");
				if(_MRParentChFFCombo.getText().compareTo("Any")!=0){
					FaultCode[] fcl = _alarmManager.getFaultFamily(_MRParentChFFCombo.getText()).getFaultCode();
					for (int j = 0; j < fcl.length; j++) {
						_MRParentChFCCombo.add(String.valueOf(fcl[j].getValue()));
					}
				}
				_MRParentChFCCombo.select(0);
				
				TreeItem []tmp = _tree.getSelection();
				if( tmp == null || tmp.length == 0 )
					return;

				TreeItem item = tmp[0];
				String[] triplet = getTriplet(item.getText());
				fillMRParentChAlarmList(triplet[0],triplet[1],Integer.parseInt(triplet[2]));
			}
		};
		_updateMRParentCh = new Listener() {
			public void handleEvent(Event event){
				TreeItem []tmp = _tree.getSelection();
				if( tmp == null || tmp.length == 0 )
					return;
				
				TreeItem item = tmp[0];
				String[] triplet = getTriplet(item.getText());
				fillMRParentChAlarmList(triplet[0],triplet[1],Integer.parseInt(triplet[2]));
			}
		};
		_MRParentGroup = new Group(_compInitial, SWT.SHADOW_ETCHED_IN);
		GridLayout gl = new GridLayout();
		gl.numColumns = 1;
		GridData gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.grabExcessVerticalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		gd.verticalAlignment = SWT.FILL;
		_MRParentGroup.setLayout(gl);
		_MRParentGroup.setLayoutData(gd);

		_MRParentFtGroup = new Group(_MRParentGroup, SWT.SHADOW_ETCHED_IN);
		gl = new GridLayout();
		gl.numColumns = 2;
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_MRParentFtGroup.setLayout(gl);
		_MRParentFtGroup.setLayoutData(gd);
		_MRParentFtGroup.setText("Father");
		
		_MRParentFFLabel = new Label(_MRParentFtGroup,SWT.NONE);
		_MRParentFFLabel.setText("Fault Family:");
		_MRParentFFCombo = new Combo(_MRParentFtGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_MRParentFFCombo.setLayoutData(gd);
		_MRParentFFCombo.addListener(SWT.Selection, _updateMRParentFF);

		_MRParentFMLabel = new Label(_MRParentFtGroup,SWT.NONE);
		_MRParentFMLabel.setText("Fault Member:");
		_MRParentFMCombo = new Combo(_MRParentFtGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_MRParentFMCombo.setLayoutData(gd);
		_MRParentFMCombo.addListener(SWT.Selection, _updateMRParent);

		_MRParentFCLabel = new Label(_MRParentFtGroup,SWT.NONE);
		_MRParentFCLabel.setText("Fault Code:");
		_MRParentFCCombo = new Combo(_MRParentFtGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_MRParentFCCombo.setLayoutData(gd);
		_MRParentFCCombo.addListener(SWT.Selection, _updateMRParent);
//
		_MRParentChGroup = new Group(_MRParentGroup, SWT.SHADOW_ETCHED_IN);
		gl = new GridLayout();
		gl.numColumns = 1;
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.grabExcessVerticalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		gd.verticalAlignment = SWT.FILL;
		_MRParentChGroup.setLayout(gl);
		_MRParentChGroup.setLayoutData(gd);
		_MRParentChGroup.setText("Children");
		
		_MRParentChFilterGroup = new Group(_MRParentChGroup, SWT.SHADOW_ETCHED_IN);
		gl = new GridLayout();
		gl.numColumns = 2;
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_MRParentChFilterGroup.setLayout(gl);
		_MRParentChFilterGroup.setLayoutData(gd);
		_MRParentChFilterGroup.setText("Filter");
		
		_MRParentChFFLabel = new Label(_MRParentChFilterGroup,SWT.NONE);
		_MRParentChFFLabel.setText("Fault Family:");
		_MRParentChFFCombo = new Combo(_MRParentChFilterGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_MRParentChFFCombo.setLayoutData(gd);
		_MRParentChFFCombo.addListener(SWT.Selection, _updateMRParentChFF);

		_MRParentChFMLabel = new Label(_MRParentChFilterGroup,SWT.NONE);
		_MRParentChFMLabel.setText("Fault Member:");
		_MRParentChFMCombo = new Combo(_MRParentChFilterGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_MRParentChFMCombo.setLayoutData(gd);
		_MRParentChFMCombo.addListener(SWT.Selection, _updateMRParentCh);

		_MRParentChFCLabel = new Label(_MRParentChFilterGroup,SWT.NONE);
		_MRParentChFCLabel.setText("Fault Code:");
		_MRParentChFCCombo = new Combo(_MRParentChFilterGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_MRParentChFCCombo.setLayoutData(gd);
		_MRParentChFCCombo.addListener(SWT.Selection, _updateMRParentCh);
		
		_MRParentChAlarmList = new Table(_MRParentChGroup,SWT.BORDER);
		gd = new GridData();
		gd.verticalAlignment = SWT.FILL;
		gd.horizontalAlignment = SWT.FILL;
		gd.grabExcessVerticalSpace = true;
		gd.grabExcessHorizontalSpace = true;
		_MRParentChAlarmList.setLayoutData(gd);
		_MRParentChAlarmList.addListener(SWT.Selection, _addRule);
	}
	
	private void createMRChildrenWidgets() {

		_MRChildrenGroup = new Group(_compInitial, SWT.SHADOW_ETCHED_IN);
		GridLayout gl = new GridLayout();
		gl.numColumns = 2;
		GridData gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_MRChildrenGroup.setLayout(gl);
		_MRChildrenGroup.setLayoutData(gd);

		_MRChildrenFFLabel = new Label(_MRChildrenGroup,SWT.NONE);
		_MRChildrenFFLabel.setText("Fault Family:");
		_MRChildrenFFName = new Label(_MRChildrenGroup,SWT.NONE);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_MRChildrenFFName.setLayoutData(gd);

		_MRChildrenFMLabel = new Label(_MRChildrenGroup,SWT.NONE);
		_MRChildrenFMLabel.setText("Fault Member:");
		_MRChildrenFMName = new Label(_MRChildrenGroup,SWT.NONE);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_MRChildrenFMName.setLayoutData(gd);

		_MRChildrenFCLabel = new Label(_MRChildrenGroup,SWT.NONE);
		_MRChildrenFCLabel.setText("Fault Code:");
		_MRChildrenFCName = new Label(_MRChildrenGroup,SWT.NONE);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_MRChildrenFCName.setLayoutData(gd);

	}

	/* (non-Javadoc)
	 * @see cl.utfsm.acs.acg.gui.IMyViewPart#refreshContents()
	 */
	public void refreshContents() {
		TreeItem kTree = null;
		Triplet t = null;

		_tree.removeAll();
		_reductionManager = AlarmSystemManager.getInstance().getReductionManager();
		_alarmManager = AlarmSystemManager.getInstance().getAlarmManager();
		for(int i=0; i!=2; i++) {
			TreeItem iTree = new TreeItem(_tree,SWT.NONE);
			iTree.setText((i==0 ? "Node Reductions" : "Multiplicity Reductions"));
			iTree.setData((i==0 ? NodeType.NODE_REDUCTION : NodeType.MULTIPLICITY_REDUCTION));

			/* Show the Node Reduction Rules */
			if( i == 0 ) {

				List<ReductionRule> nodeRR = _reductionManager.getNodeReductionRules();
				for (ReductionRule rule : nodeRR) {
					kTree = new TreeItem(iTree,SWT.NONE);
					t = rule.getParent().getTriplet();
					kTree.setText("<" + t.getFaultFamily() + "," + t.getFaultMember() + "," + t.getFaultCode() + ">");
					kTree.setData(NodeType.NODE_REDUCTION_PARENT_DATA);
					List<Alarm> chNodeRR = rule.getChildren();
					for(Alarm chRule : chNodeRR){
						TreeItem lTree = new TreeItem(kTree,SWT.NONE);
						t = chRule.getTriplet();
						lTree.setText("<" + t.getFaultFamily() + "," + t.getFaultMember() + "," + t.getFaultCode() + ">");
						lTree.setData(NodeType.NODE_REDUCTION_CHILDREN_DATA);
					}
				}
			}
			/* Show the Multiplicity Reduction Rules */
			else {
				List<ReductionRule> multiRR = _reductionManager.getMultiReductionRules();
				for (ReductionRule rule : multiRR) {
					kTree = new TreeItem(iTree,SWT.NONE);
					t = rule.getParent().getTriplet();
					kTree.setText("<" + t.getFaultFamily() + "," + t.getFaultMember() + "," + t.getFaultCode() + ">");
					kTree.setData(NodeType.MULTIPLICITY_REDUCTION_PARENT_DATA);
					List<Alarm> chNodeRR = rule.getChildren();
					for(Alarm chRule : chNodeRR){
						TreeItem lTree = new TreeItem(kTree,SWT.NONE);
						t = chRule.getTriplet();
						lTree.setText("<" + t.getFaultFamily() + "," + t.getFaultMember() + "," + t.getFaultCode() + ">");
						lTree.setData(NodeType.MULTIPLICITY_REDUCTION_CHILDREN_DATA);
					}
				}
			}
		}

		_tree.deselectAll();
	}

	private void fillNRParentWidgets(String ff, String fm, int fc) {
		//_updateNRParentFF.setEnabled(false);

		ReductionRule nrr = _reductionManager.getNRParentByTriplet(ff, fm, fc);

		// This should never happen anyways...
		if( nrr == null )
			return;

		Alarm parent = nrr.getParent();

		_NRParentFFCombo.removeAll();
		_NRParentChFFCombo.removeAll();
		_NRParentChFFCombo.add("Any");
		_NRParentChFFCombo.select(0);
		

		List<FaultFamily> ffl = _alarmManager.getAllAlarms();
		FaultFamily ffs = null;
		int i = -1;
		for (Iterator<FaultFamily> iterator = ffl.iterator(); iterator.hasNext();) {
			FaultFamily fft = iterator.next();
			_NRParentFFCombo.add(fft.getName());
			_NRParentChFFCombo.add(fft.getName());
			if(fft.getName().compareTo(parent.getTriplet().getFaultFamily()) == 0){
				i = _NRParentFFCombo.getItemCount() - 1;
				ffs = fft;
			}
		}
		_NRParentFFCombo.select(i);
		
		
		_NRParentFMCombo.removeAll();
		i = -1;
		FaultMember[] fml = ffs.getFaultMember();
		for (int j = 0; j < fml.length; j++) {
			_NRParentFMCombo.add(fml[j].getName());
			if(fml[j].getName().compareTo(parent.getTriplet().getFaultMember()) == 0)
				i = j;
		}
		_NRParentFMCombo.select(i);
		
		_NRParentFCCombo.removeAll();
		i = -1;
		FaultCode[] fcl = ffs.getFaultCode();
		for (int j = 0; j < fcl.length; j++) {
			_NRParentFCCombo.add(String.valueOf(fcl[j].getValue()));
			if(fcl[j].getValue() == parent.getTriplet().getFaultCode().intValue())
				i = j;
		}
		_NRParentFCCombo.select(i);
		
		_NRParentChFMCombo.removeAll();
		_NRParentChFMCombo.add("Any");
		_NRParentChFMCombo.select(0);
		
		_NRParentChFCCombo.removeAll();
		_NRParentChFCCombo.add("Any");
		_NRParentChFCCombo.select(0);
		
		fillNRParentChAlarmList(ff,fm,fc);
		
		//_updateNRParentFF.setEnabled(true);
	}
	
	public void fillNRParentChAlarmList(String ff, String fm, int fc){
		ReductionRule nrr = _reductionManager.getNRParentByTriplet(ff, fm, fc);

		// This should never happen anyways...
		if( nrr == null )
			return;

		Alarm parent = nrr.getParent();
		
		_NRParentChAlarmList.removeAll();
		List<Alarm> alarms = _alarmManager.getAlarms();
		for (Iterator<Alarm> iterator = alarms.iterator(); iterator.hasNext();) {
			Alarm alarm = iterator.next();
			if(alarm.getAlarmId().compareTo(parent.getAlarmId()) == 0)
				continue;
			if(_NRParentChFFCombo.getText().compareTo("Any") != 0)
				if(alarm.getTriplet().getFaultFamily().compareTo(_NRParentChFFCombo.getText()) != 0)
					continue;
			if(_NRParentChFMCombo.getText().compareTo("Any") != 0)
				if(alarm.getTriplet().getFaultMember().compareTo(_NRParentChFMCombo.getText()) != 0)
					continue;
			if(_NRParentChFCCombo.getText().compareTo("Any") != 0)
				if(alarm.getTriplet().getFaultCode() != Integer.parseInt(_NRParentChFCCombo.getText()))
					continue;
			if(_NRParentChFilterText.getText().compareTo("")!=0)
				if(!alarm.getAlarmId().matches(_NRParentChFilterText.getText()))
					continue;
			TableItem  t = new TableItem(_NRParentChAlarmList, SWT.None);
			t.setText(alarm.getAlarmId());
			List<Alarm> children = nrr.getChildren();
			for (Iterator<Alarm> iterator2 = children.iterator(); iterator2.hasNext();) {
				Alarm alarm2 = iterator2.next();
				if(alarm.getAlarmId().compareTo(alarm2.getAlarmId()) == 0){
					t.setImage(Activator.getDefault().getImageRegistry().get(Activator.IMG_TICKET));
					break;
				}
			}
		}
	}
	
	private void fillNRChildrenWidgets(String ff, String fm, int fc, String ff2, String fm2, int fc2) {
		ReductionRule nrr = _reductionManager.getNRParentByTriplet(ff, fm, fc);
		Alarm child = null;

		// This should never happen anyways...
		if( nrr == null )
			return;
		List<Alarm> children = nrr.getChildren();
		for(Alarm chnrr : children){
			if(chnrr.getTriplet().getFaultFamily().compareTo(ff2) == 0 && chnrr.getTriplet().getFaultMember().compareTo(fm2) == 0 && chnrr.getTriplet().getFaultCode() == fc2){
				child = chnrr;
				break;
			}
		}
		
		// This should never happen anyways...
		if(child == null)
			return;

		_NRChildrenFFName.setText(child.getTriplet().getFaultFamily());
		_NRChildrenFMName.setText(child.getTriplet().getFaultMember());
		_NRChildrenFCName.setText(String.valueOf(child.getTriplet().getFaultCode().intValue()));

	}

	private void fillMRParentWidgets(String ff, String fm, int fc) {
		ReductionRule mrr = _reductionManager.getMRParentByTriplet(ff, fm, fc);

		// This should never happen anyways...
		if( mrr == null )
			return;

		Alarm parent = mrr.getParent();

		_MRParentFFCombo.removeAll();
		_MRParentChFFCombo.removeAll();
		_MRParentChFFCombo.add("Any");
		_MRParentChFFCombo.select(0);

		List<FaultFamily> ffl = _alarmManager.getAllAlarms();
		FaultFamily ffs = null;
		int i = -1;
		for (Iterator<FaultFamily> iterator = ffl.iterator(); iterator.hasNext();) {
			FaultFamily fft = iterator.next();
			_MRParentFFCombo.add(fft.getName());
			_MRParentChFFCombo.add(fft.getName());
			if(fft.getName().compareTo(parent.getTriplet().getFaultFamily()) == 0){
				i = _MRParentFFCombo.getItemCount() - 1;
				ffs = fft;
			}
		}
		_MRParentFFCombo.select(i);
		
		
		_MRParentFMCombo.removeAll();
		i = -1;
		FaultMember[] fml = ffs.getFaultMember();
		for (int j = 0; j < fml.length; j++) {
			_MRParentFMCombo.add(fml[j].getName());
			if(fml[j].getName().compareTo(parent.getTriplet().getFaultMember()) == 0)
				i = j;
		}
		_MRParentFMCombo.select(i);
		
		_MRParentFCCombo.removeAll();
		i = -1;
		FaultCode[] fcl = ffs.getFaultCode();
		for (int j = 0; j < fcl.length; j++) {
			_MRParentFCCombo.add(String.valueOf(fcl[j].getValue()));
			if(fcl[j].getValue() == parent.getTriplet().getFaultCode().intValue())
				i = j;
		}
		_MRParentFCCombo.select(i);
		
		_MRParentChFMCombo.removeAll();
		_MRParentChFMCombo.add("Any");
		_MRParentChFMCombo.select(0);
		
		_MRParentChFCCombo.removeAll();
		_MRParentChFCCombo.add("Any");
		_MRParentChFCCombo.select(0);
		
		fillMRParentChAlarmList(ff,fm,fc);
	}
	
	private void fillMRChildrenWidgets(String ff, String fm, int fc, String ff2, String fm2, int fc2) {
		ReductionRule mrr = _reductionManager.getMRParentByTriplet(ff, fm, fc);
		Alarm child = null;

		// This should never happen anyways...
		if( mrr == null )
			return;
		List<Alarm> children = mrr.getChildren();
		for(Alarm chmrr : children){
			if(chmrr.getTriplet().getFaultFamily().compareTo(ff2) == 0 && chmrr.getTriplet().getFaultMember().compareTo(fm2) == 0 && chmrr.getTriplet().getFaultCode() == fc2){
				child = chmrr;
				break;
			}
		}
		
		// This should never happen anyways...
		if(child == null)
			return;

		_MRChildrenFFName.setText(child.getTriplet().getFaultFamily());
		_MRChildrenFMName.setText(child.getTriplet().getFaultMember());
		_MRChildrenFCName.setText(String.valueOf(child.getTriplet().getFaultCode().intValue()));
	}
	
	public void fillMRParentChAlarmList(String ff, String fm, int fc){
		ReductionRule mrr = _reductionManager.getNRParentByTriplet(ff, fm, fc);

		// This should never happen anyways...
		if( mrr == null )
			return;

		Alarm parent = mrr.getParent();
		
		_MRParentChAlarmList.removeAll();
		List<Alarm> alarms = _alarmManager.getAlarms();
		for (Iterator<Alarm> iterator = alarms.iterator(); iterator.hasNext();) {
			Alarm alarm = iterator.next();
			if(alarm.getAlarmId().compareTo(parent.getAlarmId()) == 0)
				continue;
			if(_MRParentChFFCombo.getText().compareTo("Any") != 0)
				if(alarm.getTriplet().getFaultFamily().compareTo(_MRParentChFFCombo.getText()) != 0)
					continue;
			if(_MRParentChFMCombo.getText().compareTo("Any") != 0)
				if(alarm.getTriplet().getFaultMember().compareTo(_MRParentChFMCombo.getText()) != 0)
					continue;
			if(_MRParentChFCCombo.getText().compareTo("Any") != 0)
				if(alarm.getTriplet().getFaultCode() != Integer.parseInt(_MRParentChFCCombo.getText()))
					continue;
			TableItem  t = new TableItem(_MRParentChAlarmList, SWT.None);
			t.setText(alarm.getAlarmId());
			List<Alarm> children = mrr.getChildren();
			for (Iterator<Alarm> iterator2 = children.iterator(); iterator2.hasNext();) {
				Alarm alarm2 = iterator2.next();
				if(alarm.getAlarmId().compareTo(alarm2.getAlarmId()) == 0){
					t.setImage(Activator.getDefault().getImageRegistry().get(Activator.IMG_TICKET));
					break;
				}
			}
		}
	}

	@Override
	public void setFocus() {
	}

	public void setEnabled(boolean v) {
		_tree.setEnabled(v);
		_NRParentGroup.setEnabled(v);
		_NRChildrenGroup.setEnabled(v);
		_MRParentGroup.setEnabled(v);
		_MRChildrenGroup.setEnabled(v);
	}

	private String[] getTriplet(String str) {
		str = str.replaceAll("^<","");
		str = str.replaceAll(">$","");
		String[] triplet = str.split(",");
		return triplet;
	}

	@Override
	public void fillWidgets() {
		// TODO Auto-generated method stub
		
	}
}
