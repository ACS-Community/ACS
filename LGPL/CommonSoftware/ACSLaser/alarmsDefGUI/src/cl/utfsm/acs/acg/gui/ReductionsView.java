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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.regex.PatternSyntaxException;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
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
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.dialogs.ListDialog;

import cern.laser.business.data.Alarm;
import cern.laser.business.data.Triplet;
import alma.acs.alarmsystem.generated.FaultCode;
import alma.acs.alarmsystem.generated.FaultFamily;
import alma.acs.alarmsystem.generated.FaultMember;

import cl.utfsm.acs.acg.core.AlarmSystemManager;
import cl.utfsm.acs.acg.core.AlarmManager;
import cl.utfsm.acs.acg.core.IllegalOperationException;
import cl.utfsm.acs.acg.core.ReductionManager;
import cl.utfsm.acs.acg.core.ReductionRule;

public class ReductionsView extends ViewPart implements IMyViewPart {
	
	public static String ID = "cl.utfsm.acs.acg.gui.reductionsview";

	private enum NodeType {
		NODE_REDUCTION,
		NODE_REDUCTION_PARENT_DATA,
		MULTIPLICITY_REDUCTION,
		MULTIPLICITY_REDUCTION_PARENT_DATA
	}

	private ReductionManager _reductionManager;
	private AlarmManager _alarmManager;

	/* High level widgets */
	private SashForm _sash;
	private Tree _tree;
	private Composite _compInitial;
	private Composite _reductionsComp;
	private Group _treeGroup;

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
	private Group _NRParentChFilterLevelGroup;
	private Button _NRParentChAllRadio;
	private Button _NRParentChSelectedRadio;
	private Button _NRParentChUnselectedRadio;
	private Combo _NRParentChFFCombo;
	private Label _NRParentChFMLabel;
	private Combo _NRParentChFMCombo;
	private Label _NRParentChFCLabel;
	private Combo _NRParentChFCCombo;
	private Label _NRParentChFilterLabel;
	private Text  _NRParentChFilterText;
	private Table _NRParentChAlarmList;
	private Label _NRParentErrorMessageLabel;

	/* MR Parent Widgets */
	private Group _MRParentGroup;
	private Group _MRParentFtGroup;
	private Label _MRParentFFLabel;
	private Combo _MRParentFFCombo;
	private Label _MRParentFMLabel;
	private Combo _MRParentFMCombo;
	private Label _MRParentFCLabel;
	private Combo _MRParentFCCombo;
	private Label _MRParentThresholdLabel;
	private Text _MRParentThresholdText;
	//
	private Group _MRParentChGroup;
	private Group _MRParentChFilterGroup;
	private Label _MRParentChFFLabel;
	private Group _MRParentChFilterLevelGroup;
	private Button _MRParentChAllRadio;
	private Button _MRParentChSelectedRadio;
	private Button _MRParentChUnselectedRadio;
	private Combo _MRParentChFFCombo;
	private Label _MRParentChFMLabel;
	private Combo _MRParentChFMCombo;
	private Label _MRParentChFCLabel;
	private Combo _MRParentChFCCombo;
	private Label _MRParentChFilterLabel;
	private Text  _MRParentChFilterText;
	private Table _MRParentChAlarmList;
	private Label _MRParentErrorMessageLabel;	
	
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
				TreeItem sel = null;
				TreeItem item = null;
				if(_tree.getSelection() == null || _tree.getSelection().length == 0)
					return;
				sel = _tree.getSelection()[0];
				NodeType type = (NodeType)sel.getData();
				item = sel;
				if(type == NodeType.NODE_REDUCTION_PARENT_DATA || type == NodeType.MULTIPLICITY_REDUCTION_PARENT_DATA)
					item = sel.getParentItem();
				type = (NodeType) item.getData();
				java.util.List<FaultFamily> ffs = _alarmManager.getAllAlarms();
				java.util.List<String> ffnames = new ArrayList<String>();
				for(FaultFamily ff: ffs) {
					if (ff.getFaultCodeCount() > 0 && ff.getFaultMemberCount() > 0)
						ffnames.add(ff.getName());
				}
				Collections.sort(ffnames,IGNORE_CASE_ORDER);
				ListDialog dialog = new ListDialog(PlatformUI.getWorkbench().getDisplay().getActiveShell());
				dialog.setTitle("Create Reduction Rule");
				dialog.setMessage("Select Parent Fault Family");
				dialog.setBlockOnOpen(true);
				dialog.setInput(ffnames);
				dialog.setContentProvider(new ArrayContentProvider());
				dialog.setLabelProvider(new LabelProvider()); 
				dialog.open();
				if(dialog.getReturnCode() == InputDialog.CANCEL)
					return;
				String ffname = (String) dialog.getResult()[0];
				FaultMember[] fms = _alarmManager.getFaultFamily(ffname).getFaultMember();
				java.util.List<String> fmnames = new ArrayList<String>();
				for(FaultMember fm: fms) {
					fmnames.add(fm.getName());
				}
				Collections.sort(ffnames,IGNORE_CASE_ORDER);
				dialog = new ListDialog(PlatformUI.getWorkbench().getDisplay().getActiveShell());
				dialog.setTitle("Create Reduction Rule");
				dialog.setMessage("Select Parent Fault Member");
				dialog.setBlockOnOpen(true);
				dialog.setInput(fmnames);
				dialog.setContentProvider(new ArrayContentProvider());
				dialog.setLabelProvider(new LabelProvider()); 
				dialog.open();
				if(dialog.getReturnCode() == InputDialog.CANCEL)
					return;
				String fmname = (String) dialog.getResult()[0];
				FaultCode[] fcs = _alarmManager.getFaultFamily(ffname).getFaultCode();
				java.util.List<String> fcvalues = new ArrayList<String>();
				for(FaultCode fc: fcs) {
					fcvalues.add(Integer.toString(fc.getValue()));
				}
				Collections.sort(ffnames,IGNORE_CASE_ORDER);
				dialog = new ListDialog(PlatformUI.getWorkbench().getDisplay().getActiveShell());
				dialog.setTitle("Create Reduction Rule");
				dialog.setMessage("Select Parent Fault Code");
				dialog.setBlockOnOpen(true);
				dialog.setInput(fcvalues);
				dialog.setContentProvider(new ArrayContentProvider());
				dialog.setLabelProvider(new LabelProvider()); 
				dialog.open();
				if(dialog.getReturnCode() == InputDialog.CANCEL)
					return;
				String fcvalue = (String) dialog.getResult()[0];
				ReductionRule parent = null;
				boolean error = false;
				if(type == NodeType.NODE_REDUCTION) {
					parent = _reductionManager.getNRParentByTriplet(ffname, fmname, Integer.parseInt(fcvalue));
					TreeItem[] chs = _tree.getItems()[0].getItems();
					for(TreeItem ch: chs) {
						if(ch.getText().compareTo("<"+ffname+","+fmname+","+fcvalue+">") == 0)
							error = true;
					}
				} else if(type == NodeType.MULTIPLICITY_REDUCTION) {
					parent = _reductionManager.getMRParentByTriplet(ffname, fmname, Integer.parseInt(fcvalue));
					TreeItem[] chs = _tree.getItems()[1].getItems();
					for(TreeItem ch: chs) {
						if(ch.getText().compareTo("<"+ffname+","+fmname+","+fcvalue+">") == 0)
							error = true;
					}
				}
				if(error || parent != null){
					ErrorDialog edialog = new ErrorDialog(ReductionsView.this.getViewSite().getShell(),
							"Reduction Rule Already Exists",
							"The reduction rule you are trying to create already exists",
							new Status(IStatus.ERROR,"cl.utfsm.acs.acg","The reduction rule parent already exists"),
							IStatus.ERROR);
					edialog.setBlockOnOpen(true);
					edialog.open();
					return;
				} else
					parent = new ReductionRule(_alarmManager.getAlarm(ffname+":"+fmname+":"+fcvalue));
				TreeItem pTree = new TreeItem(item,SWT.NONE);
				pTree.setText("<"+ffname+","+fmname+","+fcvalue+">");
				if(type == NodeType.NODE_REDUCTION) {
					pTree.setData(NodeType.NODE_REDUCTION_PARENT_DATA);
					parent.setIsNodeReduction(true);
				} else if(type == NodeType.MULTIPLICITY_REDUCTION) {
					pTree.setData(NodeType.MULTIPLICITY_REDUCTION_PARENT_DATA);
					parent.setIsNodeReduction(false);
				}
				_tree.setSelection(pTree);
				Event e = new Event();
				_tree.notifyListeners(SWT.Selection, e);
			}
		};
		
		_addRule = new Listener() {
			public void handleEvent(Event event) {
				Table t = (Table)event.widget;
				if(event.type == SWT.KeyUp)
					if(!(event.keyCode == SWT.CR || event.keyCode == ' '))
						return;
				if(event.type == SWT.MouseDoubleClick){
					Point pt = new Point(event.x,event.y);
					if(t.getItem(pt) == null)
						return;
				}
				boolean isNode;
				if(_tree.getSelection() == null || _tree.getSelection().length == 0)
					return;
				TreeItem tmp1 = _tree.getSelection()[0];
				if((NodeType)_tree.getSelection()[0].getData() == NodeType.NODE_REDUCTION_PARENT_DATA)
					isNode = true;
				else if((NodeType)_tree.getSelection()[0].getData() == NodeType.MULTIPLICITY_REDUCTION_PARENT_DATA)
					isNode = false;
				else
					return;
				String[] tr = getTriplet(tmp1.getText());
				ReductionRule parent;
				if(isNode)
					parent = _reductionManager.getNRParentByTriplet(tr[0], tr[1], Integer.parseInt(tr[2]));
				else
					parent = _reductionManager.getMRParentByTriplet(tr[0], tr[1], Integer.parseInt(tr[2]));
				if(t.getSelection() == null || t.getSelection().length == 0)
					return;
				TableItem item = t.getSelection()[0];
				Alarm p,c;
				if(parent == null) {
					if(isNode)
						p = _alarmManager.getAlarm(_NRParentFFCombo.getText()+":"+_NRParentFMCombo.getText()+":"+_NRParentFCCombo.getText());
					else
						p = _alarmManager.getAlarm(_MRParentFFCombo.getText()+":"+_MRParentFMCombo.getText()+":"+_MRParentFCCombo.getText());
					if(p == null) {
						if(isNode)
							_NRParentErrorMessageLabel.setText("Couldn't find parent alarm.");
						else
							_MRParentErrorMessageLabel.setText("Couldn't find parent alarm.");
						return;
					}
					c = null;
				} else {
					p = parent.getParent();
					c = parent.getChild(item.getText());
				}
					
				if(c == null){
					//Add child
					c = _alarmManager.getAlarm(item.getText());
					if(c == null) {
						if(isNode)
							_NRParentErrorMessageLabel.setText("Couldn't find child alarm.");
						else
							_MRParentErrorMessageLabel.setText("Couldn't find child alarm.");
						return;
					}
					try {
						if(isNode)
							_reductionManager.addNodeReductionRule(p, c);
						else {
							_reductionManager.addMultiReductionRule(p, c);
							if(parent == null)
								_reductionManager.updateMultiThreshold(p, Integer.parseInt(_MRParentThresholdText.getText()));
						}
						item.setImage(Activator.getDefault().getImageRegistry().get(Activator.IMG_TICKET));
						if(parent == null)
							_tree.getSelection()[0].setText("<" + p.getAlarmId().replace(':', ',') + ">");
						if(isNode)
							_NRParentErrorMessageLabel.setText("");
						else
							_MRParentErrorMessageLabel.setText("");
					} catch (IllegalOperationException e) {
						if(isNode)
							_NRParentErrorMessageLabel.setText(e.getMessage());
						else
							_MRParentErrorMessageLabel.setText(e.getMessage());
					}
				}
				else{
					//Remove child
					try {
						//ReductionRule rr;
						if(isNode) {
							_reductionManager.deleteNodeReductionRule(p, c);
							//rr = _reductionManager.getNRParentByTriplet(p.getTriplet().getFaultFamily(), p.getTriplet().getFaultMember(), p.getTriplet().getFaultCode());
						}
						else {
							_reductionManager.deleteMultiReductionRule(p, c);
							//rr = _reductionManager.getMRParentByTriplet(p.getTriplet().getFaultFamily(), p.getTriplet().getFaultMember(), p.getTriplet().getFaultCode());
						}
					} catch (IllegalOperationException e) {
						e.printStackTrace();
					}
					item.setImage((org.eclipse.swt.graphics.Image)null);
				}
				fillMRParentChAlarmList(tr[0], tr[1], Integer.parseInt(tr[2]));
				sortReductionRulesList();
				Triplet t2 = p.getTriplet();
				if(isNode) selectElementFromTree("<"+t2.getFaultFamily()+","+t2.getFaultMember()+","+t2.getFaultCode()+">", true);
				else selectElementFromTree("<"+t2.getFaultFamily()+","+t2.getFaultMember()+","+t2.getFaultCode()+">", false);
			}
		};
		
		_removeElement = new Listener() {
			public void handleEvent(Event event) {
				boolean choice = MessageDialog.openQuestion( 
						ReductionsView.this.getViewSite().getShell(),
						"Confirmation",
						"Are you sure you want to delete this element"
				);
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
							//Remove all the NODE REDUCTION Rules in which this node is parent.
							ReductionRule rr = _reductionManager.getNRParentByTriplet(tr[0], tr[1], Integer.parseInt(tr[2]));
							if(rr != null) {
								Alarm p = rr.getParent();
								List<Alarm> chL = rr.getChildren();
								Alarm[] als = new Alarm[chL.size()];
								chL.toArray(als);
								for(int i = 0; i < als.length; i++)
									_reductionManager.deleteNodeReductionRule(p, als[i]);
							}
						}
						else if(type == NodeType.MULTIPLICITY_REDUCTION_PARENT_DATA){
							//Remove all the MULTIPLICITY REDUCTION Rules in which this node is a parent.
							ReductionRule rr = _reductionManager.getMRParentByTriplet(tr[0], tr[1], Integer.parseInt(tr[2]));
							if(rr != null) {
								Alarm p = rr.getParent();
								List<Alarm> chL = rr.getChildren();
								Alarm[] als = new Alarm[chL.size()];
								chL.toArray(als);
								for(int i = 0; i < als.length; i++)
									_reductionManager.deleteMultiReductionRule(p, als[i]);
							}
						}
					} catch (NullPointerException e){
						ErrorDialog error = new ErrorDialog(ReductionsView.this.getViewSite().getShell(),
								"Cannot delete the item",
								e.getMessage(),
								new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.getMessage()),
								IStatus.ERROR);
					error.setBlockOnOpen(true);
					error.open();
					} catch (IllegalOperationException e) {
						ErrorDialog error = new ErrorDialog(ReductionsView.this.getViewSite().getShell(),
								"Cannot delete the item",
								e.getMessage(),
								new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.getMessage()),
								IStatus.ERROR);
					error.setBlockOnOpen(true);
					error.open();
					}
					sel = _tree.getSelection()[0].getParentItem();
					_tree.getSelection()[0].dispose();
					_tree.setSelection(sel);
					Event e = new Event();
					_tree.notifyListeners(SWT.Selection, e);
				}
			}
		};
		_sash = new SashForm(parent,SWT.NONE);
		_sash.setLayout(new FillLayout());
		
		_reductionsComp = new Composite(_sash,SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.numColumns = 1;
		_reductionsComp.setLayout(layout);
		
		_treeGroup = new Group(_reductionsComp,SWT.SHADOW_ETCHED_IN);
		GridData gd = new GridData();
		gd.horizontalAlignment = SWT.FILL;
		gd.verticalAlignment = SWT.FILL;
		gd.grabExcessHorizontalSpace = true;
		gd.grabExcessVerticalSpace = true;
		_treeGroup.setLayoutData(gd);
		GridLayout gl = new GridLayout();
		gl.numColumns = 1;
		_treeGroup.setLayout(gl);
		_treeGroup.setText("Reduction Rules List");

		_tree = new Tree(_treeGroup,SWT.VIRTUAL | SWT.BORDER);
		gd = new GridData();
		gd.verticalAlignment = SWT.FILL;
		gd.horizontalAlignment = SWT.FILL;
		gd.grabExcessVerticalSpace = true;
		gd.grabExcessHorizontalSpace = true;
		_tree.setLayoutData(gd);
		Menu treePopUp = new Menu(parent);
		/*
		MenuItem treePopUpDelete = new MenuItem(treePopUp,SWT.PUSH);
		treePopUpDelete.setText("Delete");
		treePopUpDelete.addListener(SWT.Selection, _removeElement);
		MenuItem treePopUpAddRule = new MenuItem(treePopUp,SWT.PUSH);
		treePopUpAddRule.setText("Add Rule");
		treePopUpAddRule.addListener(SWT.Selection, _addElement);
		MenuItem treePopUpAddChildren = new MenuItem(treePopUp,SWT.PUSH);
		treePopUpAddChildren.setText("Add Children");
		//treePopUpAddChildren.addListener(SWT.Selection, _addElement);
		*/
		_tree.setMenu(treePopUp);
		treePopUp.addListener(SWT.Show, new Listener() {
			public void handleEvent(Event e) {
				TreeItem sel = _tree.getSelection()[0];
				NodeType type = (NodeType) sel.getData();
				Menu treePopUp = _tree.getMenu();
				MenuItem[] items = treePopUp.getItems();
				for (int i = 0; i < items.length; i++)
					items[i].dispose();
				MenuItem mitem;
				switch(type) {
				case NODE_REDUCTION:
					mitem = new MenuItem(treePopUp,SWT.PUSH);
					mitem.setText("Add Node Reduction Rule Parent");
					mitem.addListener(SWT.Selection, _addElement);
					break;
				case NODE_REDUCTION_PARENT_DATA:
					mitem = new MenuItem(treePopUp,SWT.PUSH);
					mitem.setText("Delete Node Reduction Rules for this parent");
					mitem.addListener(SWT.Selection, _removeElement);
					break;
				case MULTIPLICITY_REDUCTION:
					mitem = new MenuItem(treePopUp,SWT.PUSH);
					mitem.setText("Add Multiplicity Reduction Rule Parent");
					mitem.addListener(SWT.Selection, _addElement);
					break;
				case MULTIPLICITY_REDUCTION_PARENT_DATA:
					mitem = new MenuItem(treePopUp,SWT.PUSH);
					mitem.setText("Delete Multiplicity Reduction Rules for this parent");
					mitem.addListener(SWT.Selection, _removeElement);
					break;
				default:
					for (int i = 0; i < items.length; i++)
						items[i].dispose();
					break;
				}
			}
		});
		
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
					((GridData)_NRParentGroup.getLayoutData()).exclude = true;
					_MRParentGroup.setVisible(false);
					((GridData)_MRParentGroup.getLayoutData()).exclude = true;
				}
				else if( type == NodeType.NODE_REDUCTION_PARENT_DATA ) {
					_NRParentGroup.setVisible(true);
					((GridData)_NRParentGroup.getLayoutData()).exclude = false;
					_MRParentGroup.setVisible(false);
					((GridData)_MRParentGroup.getLayoutData()).exclude = true;
					
					String[] triplet = getTriplet(item.getText());
					fillNRParentWidgets(triplet[0],triplet[1],Integer.parseInt(triplet[2]));
					_NRParentGroup.moveAbove(c);
					_compInitial.layout();
				}
				else if( type == NodeType.MULTIPLICITY_REDUCTION ){
					_NRParentGroup.setVisible(false);
					((GridData)_NRParentGroup.getLayoutData()).exclude = true;
					_MRParentGroup.setVisible(false);
					((GridData)_MRParentGroup.getLayoutData()).exclude = true;
				}
				else if( type == NodeType.MULTIPLICITY_REDUCTION_PARENT_DATA ) {
					_NRParentGroup.setVisible(false);
					((GridData)_NRParentGroup.getLayoutData()).exclude = true;
					_MRParentGroup.setVisible(true);
					((GridData)_MRParentGroup.getLayoutData()).exclude = false;

					String[] triplet = getTriplet(item.getText());
					fillMRParentWidgets(triplet[0],triplet[1],Integer.parseInt(triplet[2]));

					_MRParentGroup.moveAbove(c);
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
		createMRParentWidgets();

		_NRParentGroup.setVisible(false);
		_MRParentGroup.setVisible(false);
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
				_NRParentErrorMessageLabel.setText("Please choose a Fault Member and a Fault Code.");
			}
		};
		_updateNRParent = new Listener() {
			public void handleEvent(Event event) {
				if(_tree.getSelection() == null || _tree.getSelection().length == 0)
					return;
				NodeType type = (NodeType)_tree.getSelection()[0].getData();
				if(type != NodeType.NODE_REDUCTION_PARENT_DATA)
					return;
				if(_NRParentFMCombo.getText().isEmpty()) {
					_NRParentErrorMessageLabel.setText("Please choose a Fault Member.");
					return;
				}
				if(_NRParentFCCombo.getText().isEmpty()) {
					_NRParentErrorMessageLabel.setText("Please choose a Fault Code.");
					return;
				}
				String[] tr = getTriplet(_tree.getSelection()[0].getText());
				ReductionRule nrr = _reductionManager.getNRParentByTriplet(tr[0], tr[1], Integer.parseInt(tr[2]));
				ReductionRule nrr2 = _reductionManager.getNRParentByTriplet(_NRParentFFCombo.getText(), _NRParentFMCombo.getText(), Integer.parseInt(_NRParentFCCombo.getText()));
				Alarm parent = null;
				Alarm nParent;
				fillNRParentChAlarmList(_NRParentFFCombo.getText(),_NRParentFMCombo.getText(),Integer.parseInt(_NRParentFCCombo.getText()));
				if(nrr == null) {
					_NRParentErrorMessageLabel.setText("There's no Reduction Rule (no children) for the selected Alarm.");
					return;
				}
				else
					parent = nrr.getParent();
				if(nrr2 == null)
					nParent = _alarmManager.getAlarm(new String(_NRParentFFCombo.getText()+":"+_NRParentFMCombo.getText()+":"+_NRParentFCCombo.getText()));
				else {
					nParent = nrr2.getParent();
					if(nrr != nrr2) {
						ErrorDialog edialog = new ErrorDialog(ReductionsView.this.getViewSite().getShell(),
								"Reduction Rule Already Exists",
								"The reduction rule you are trying to create already exists",
								new Status(IStatus.ERROR,"cl.utfsm.acs.acg","The reduction rule parent already exists"),
								IStatus.ERROR);
						edialog.setBlockOnOpen(true);
						edialog.open();
						return;
					}
				}
				if(parent != null) {
					String[] ch = parent.getNodeChildren();
					//Remove all Children from nrr and add them to nrr2.
					for (int i = 0; i < ch.length; i++) {
						Alarm aCh = _alarmManager.getAlarm(ch[i]);
						if(aCh != null){
							try {
								if(!_reductionManager.deleteNodeReductionRule(parent, aCh)) {
									_NRParentErrorMessageLabel.setText("One or more children alarms didn't exist.");
									continue;
								}
								_reductionManager.addNodeReductionRule(nParent, aCh);
							} catch (IllegalOperationException e) {
								_NRParentErrorMessageLabel.setText("The parent alarm didn't exist.");
							}
						}
					}
					_tree.getSelection()[0].setText("<" + nParent.getAlarmId().replace(':', ',') + ">");
					_NRParentErrorMessageLabel.setText("");
					if(tr[0].compareTo(_NRParentFFCombo.getText()) != 0 || tr[1].compareTo(_NRParentFMCombo.getText()) != 0 || tr[2].compareTo(_NRParentFCCombo.getText()) != 0) {
						sortReductionRulesList();
						Triplet t = nParent.getTriplet();
						selectElementFromTree("<"+t.getFaultFamily()+","+t.getFaultMember()+","+t.getFaultCode()+">", true);
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
		_NRParentFtGroup.setText("Parent Alarm");
		
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
		_NRParentChGroup.setText("Alarms to Hide");
		
		_NRParentChFilterGroup = new Group(_NRParentChGroup, SWT.SHADOW_ETCHED_IN);
		gl = new GridLayout();
		gl.numColumns = 2;
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_NRParentChFilterGroup.setLayout(gl);
		_NRParentChFilterGroup.setLayoutData(gd);
		_NRParentChFilterGroup.setText("Filter Options");
		
		_NRParentChFilterLevelGroup = new Group(_NRParentChFilterGroup, SWT.SHADOW_ETCHED_IN);
		gl = new GridLayout();
		gl.numColumns = 3;
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalSpan = 2;
		gd.horizontalAlignment = SWT.FILL;
		_NRParentChFilterLevelGroup.setLayout(gl);
		_NRParentChFilterLevelGroup.setLayoutData(gd);
		_NRParentChFilterLevelGroup.setText("Show");
		
		_NRParentChAllRadio = new Button (_NRParentChFilterLevelGroup, SWT.RADIO);
		_NRParentChAllRadio.setText("All");
		_NRParentChAllRadio.addListener(SWT.Selection, _updateNRParentCh);
		_NRParentChSelectedRadio = new Button (_NRParentChFilterLevelGroup, SWT.RADIO);
		_NRParentChSelectedRadio.setText("Selected");
		_NRParentChSelectedRadio.addListener(SWT.Selection, _updateNRParentCh);
		_NRParentChUnselectedRadio = new Button (_NRParentChFilterLevelGroup, SWT.RADIO);
		_NRParentChUnselectedRadio.setText("Unselected");
		_NRParentChUnselectedRadio.addListener(SWT.Selection, _updateNRParentCh);
		
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
		_NRParentChAlarmList.addListener(SWT.KeyUp, _addRule);
		_NRParentChAlarmList.addListener(SWT.MouseDoubleClick, _addRule);
		
		_NRParentErrorMessageLabel = new Label(_NRParentGroup, SWT.NONE);
		_NRParentErrorMessageLabel.setText("");
		_NRParentErrorMessageLabel.setForeground(getViewSite().getShell().getDisplay().getSystemColor(SWT.COLOR_RED));
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		gd.horizontalSpan = 2;
		_NRParentErrorMessageLabel.setLayoutData(gd);
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
				//_MRParentThresholdText.setText("");
				_MRParentErrorMessageLabel.setText("Please choose a Fault Member and a Fault Code.");
			}
		};
		_updateMRParent = new Listener() {
			public void handleEvent(Event event) {
				if(_tree.getSelection() == null || _tree.getSelection().length == 0)
					return;
				NodeType type = (NodeType)_tree.getSelection()[0].getData();
				if(type != NodeType.MULTIPLICITY_REDUCTION_PARENT_DATA)
					return;
				if(_MRParentFMCombo.getText().isEmpty()) {
					_MRParentErrorMessageLabel.setText("Please choose a Fault Member.");
					return;
				}
				if(_MRParentFCCombo.getText().isEmpty()) {
					_MRParentErrorMessageLabel.setText("Please choose a Fault Code.");
					return;
				}
				if(_MRParentThresholdText.getText().isEmpty()) {
					_MRParentErrorMessageLabel.setText("Please set a Threshold value.");
					return;
				}
				int thr;
				try {
					thr = Integer.parseInt(_MRParentThresholdText.getText());
				} catch(NumberFormatException e) {
					_MRParentErrorMessageLabel.setText("Incorrect Threshold. A number is required.");
					return;
				}
				String[] tr = getTriplet(_tree.getSelection()[0].getText());
				ReductionRule mrr = _reductionManager.getMRParentByTriplet(tr[0], tr[1], Integer.parseInt(tr[2]));
				ReductionRule mrr2 = _reductionManager.getMRParentByTriplet(_MRParentFFCombo.getText(), _MRParentFMCombo.getText(), Integer.parseInt(_MRParentFCCombo.getText()));
				Alarm parent = null;
				Alarm mParent;
				fillMRParentChAlarmList(_MRParentFFCombo.getText(),_MRParentFMCombo.getText(),Integer.parseInt(_MRParentFCCombo.getText()));
				if(mrr == null) {
					_MRParentErrorMessageLabel.setText("There's no Reduction Rule (no children) for the selected Alarm.");
					return;
				}
				else
					parent = mrr.getParent();
				if(mrr2 == null)
					mParent = _alarmManager.getAlarm(new String(_MRParentFFCombo.getText()+":"+_MRParentFMCombo.getText()+":"+_MRParentFCCombo.getText()));
				else {
					mParent = mrr2.getParent();
					if(mrr != mrr2) {
						ErrorDialog edialog = new ErrorDialog(ReductionsView.this.getViewSite().getShell(),
								"Reduction Rule Already Exists",
								"The reduction rule you are trying to create already exists",
								new Status(IStatus.ERROR,"cl.utfsm.acs.acg","The reduction rule parent already exists"),
								IStatus.ERROR);
						edialog.setBlockOnOpen(true);
						edialog.open();
						return;
					}
				}
				if(parent != null) {
					String[] ch = parent.getMultiplicityChildren();
					//Remove all Children from mrr and add them to mrr2.
					try {
						for (int i = 0; i < ch.length; i++) {
							Alarm aCh = _alarmManager.getAlarm(ch[i]);
							if(aCh != null){
								if(!_reductionManager.deleteMultiReductionRule(parent, aCh)) {
									_MRParentErrorMessageLabel.setText("One or more children alarms didn't exist.");
									continue;
								}
								_reductionManager.addMultiReductionRule(mParent, aCh);

							}
						}
						_reductionManager.updateMultiThreshold(mParent, thr);
					} catch (IllegalOperationException e) {
						_MRParentErrorMessageLabel.setText("The parent alarm didn't exist.");
					}
					_tree.getSelection()[0].setText("<" + mParent.getAlarmId().replace(':', ',') + ">");
					_MRParentErrorMessageLabel.setText("");
				}
				if (mrr != null && mrr.getChildrenCount() < mrr.getThreshold()) {
					_MRParentErrorMessageLabel.setText("You need to have at least threshold ("+mrr.getThreshold()+") childs for this triplet.");
					return;
				}
				if(tr[0].compareTo(_MRParentFFCombo.getText()) != 0 || tr[1].compareTo(_MRParentFMCombo.getText()) != 0 || tr[2].compareTo(_MRParentFCCombo.getText()) != 0) {
					sortReductionRulesList();
					Triplet t = mParent.getTriplet();
					selectElementFromTree("<"+t.getFaultFamily()+","+t.getFaultMember()+","+t.getFaultCode()+">", false);
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
		_MRParentFtGroup.setText("Primary Alarm");
		
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
		
		_MRParentThresholdLabel = new Label(_MRParentFtGroup,SWT.NONE);
		_MRParentThresholdLabel.setText("Threshold:");
		_MRParentThresholdText = new Text(_MRParentFtGroup, SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_MRParentThresholdText.setLayoutData(gd);
		_MRParentThresholdText.addListener(SWT.Modify, _updateMRParent);
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
		_MRParentChGroup.setText("Alarms to Ignore");
		
		_MRParentChFilterGroup = new Group(_MRParentChGroup, SWT.SHADOW_ETCHED_IN);
		gl = new GridLayout();
		gl.numColumns = 2;
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_MRParentChFilterGroup.setLayout(gl);
		_MRParentChFilterGroup.setLayoutData(gd);
		_MRParentChFilterGroup.setText("Filter Options");
		
		_MRParentChFilterLevelGroup = new Group(_MRParentChFilterGroup, SWT.SHADOW_ETCHED_IN);
		gl = new GridLayout();
		gl.numColumns = 3;
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalSpan = 2;
		gd.horizontalAlignment = SWT.FILL;
		_MRParentChFilterLevelGroup.setLayout(gl);
		_MRParentChFilterLevelGroup.setLayoutData(gd);
		_MRParentChFilterLevelGroup.setText("Show");
		
		_MRParentChAllRadio = new Button (_MRParentChFilterLevelGroup, SWT.RADIO);
		_MRParentChAllRadio.setText("All");
		_MRParentChAllRadio.addListener(SWT.Selection, _updateMRParentCh);
		_MRParentChSelectedRadio = new Button (_MRParentChFilterLevelGroup, SWT.RADIO);
		_MRParentChSelectedRadio.setText("Selected");
		_MRParentChSelectedRadio.addListener(SWT.Selection, _updateMRParentCh);
		_MRParentChUnselectedRadio = new Button (_MRParentChFilterLevelGroup, SWT.RADIO);
		_MRParentChUnselectedRadio.setText("Unselected");
		_MRParentChUnselectedRadio.addListener(SWT.Selection, _updateMRParentCh);
		
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
		
		_MRParentChFilterLabel = new Label(_MRParentChFilterGroup,SWT.NONE);
		_MRParentChFilterLabel.setText("Filter RegEx:");
		_MRParentChFilterText = new Text(_MRParentChFilterGroup, SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_MRParentChFilterText.setLayoutData(gd);
		_MRParentChFilterText.addListener(SWT.Modify, _updateMRParentCh);
		
		_MRParentChAlarmList = new Table(_MRParentChGroup,SWT.BORDER);
		gd = new GridData();
		gd.verticalAlignment = SWT.FILL;
		gd.horizontalAlignment = SWT.FILL;
		gd.grabExcessVerticalSpace = true;
		gd.grabExcessHorizontalSpace = true;
		_MRParentChAlarmList.setLayoutData(gd);
		_MRParentChAlarmList.addListener(SWT.KeyUp, _addRule);
		_MRParentChAlarmList.addListener(SWT.MouseDoubleClick, _addRule);
		
		_MRParentErrorMessageLabel = new Label(_MRParentGroup, SWT.NONE);
		_MRParentErrorMessageLabel.setText("");
		_MRParentErrorMessageLabel.setForeground(getViewSite().getShell().getDisplay().getSystemColor(SWT.COLOR_RED));
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		gd.horizontalSpan = 2;
		_MRParentErrorMessageLabel.setLayoutData(gd);
	}

	/* (non-Javadoc)
	 * @see cl.utfsm.acs.acg.gui.IMyViewPart#refreshContents()
	 */
	public void refreshContents() {
		_tree.removeAll();
		_reductionManager = AlarmSystemManager.getInstance().getReductionManager();
		_alarmManager = AlarmSystemManager.getInstance().getAlarmManager();
		sortReductionRulesList();
		_tree.deselectAll();
	}

	private void fillNRParentWidgets(String ff, String fm, int fc) {
		//_updateNRParentFF.setEnabled(false);

		ReductionRule nrr = _reductionManager.getNRParentByTriplet(ff, fm, fc);
		Alarm parent = null;
		// This should happen only when creating a new rule...
		if( nrr == null ) {
			parent = _alarmManager.getAlarm(ff+":"+fm+":"+fc);
			_NRParentErrorMessageLabel.setText("No Reduction Rule for this triplet.");
		} else
			parent = nrr.getParent();
		_NRParentFFCombo.removeAll();
		_NRParentChFFCombo.removeAll();
		_NRParentChFFCombo.add("Any");
		_NRParentChFFCombo.select(0);

		List<FaultFamily> ffList = _alarmManager.getAllAlarms();
		List<String> tmp = new ArrayList<String>();
		List<FaultFamily> sortedFFList = new ArrayList<FaultFamily>();
		for(FaultFamily tff: ffList)
			tmp.add(tff.getName());
		Collections.sort(tmp,IGNORE_CASE_ORDER);
		for(String sff: tmp)
			sortedFFList.add(_alarmManager.getFaultFamily(sff));
		
		ffList = sortedFFList;
		
		FaultFamily ffs = null;
		int i = -1;
		for (Iterator<FaultFamily> iterator = ffList.iterator(); iterator.hasNext();) {
			FaultFamily fft = iterator.next();
			if(fft.getFaultCodeCount() > 0 && fft.getFaultMemberCount() > 0)
				_NRParentFFCombo.add(fft.getName());
			_NRParentChFFCombo.add(fft.getName());
			if(parent != null && fft.getName().compareTo(parent.getTriplet().getFaultFamily()) == 0){
				i = _NRParentFFCombo.getItemCount() - 1;
				ffs = fft;
			}
		}
		_NRParentFFCombo.select(i);
		
		_NRParentFMCombo.removeAll();
		if(ffs != null) {
			i = -1;
			FaultMember[] fml = ffs.getFaultMember();
			List<FaultMember> fmList = Arrays.asList(fml);
			tmp = new ArrayList<String>();
			List<FaultMember> sortedFMList =  new ArrayList<FaultMember>();
			for(FaultMember tfm: fmList)
				tmp.add(tfm.getName());
			Collections.sort(tmp, IGNORE_CASE_ORDER);
			for(String sfm: tmp)
				for(FaultMember tfm: fmList)
					if(sfm.compareTo(tfm.getName()) == 0)
						sortedFMList.add(tfm);
			
			fmList = sortedFMList;
			
			for (FaultMember fmt: fmList) {
				_NRParentFMCombo.add(fmt.getName());
				if(fmt.getName().compareTo(parent.getTriplet().getFaultMember()) == 0)
					i = _NRParentFMCombo.getItemCount() - 1;
			}
			_NRParentFMCombo.select(i);
		}
		
		_NRParentFCCombo.removeAll();
		if(ffs != null) {
			i = -1;
			FaultCode[] fcl = ffs.getFaultCode();
			List<FaultCode> fcList = Arrays.asList(fcl);
			tmp = new ArrayList<String>();
			List<FaultCode> sortedFCList =  new ArrayList<FaultCode>();
			for(FaultCode tfc: fcList)
				tmp.add(Integer.toString(tfc.getValue()));
			Collections.sort(tmp);
			for(String sfc: tmp)
				for(FaultCode tfc: fcList)
					if(sfc.compareTo(Integer.toString(tfc.getValue())) == 0)
						sortedFCList.add(tfc);
			
			fcList = sortedFCList;
			
			for (FaultCode fct: fcList) {
				_NRParentFCCombo.add(Integer.toString(fct.getValue()));
				if(fct.getValue() == parent.getTriplet().getFaultCode())
					i = _NRParentFCCombo.getItemCount() - 1;
			}
			_NRParentFCCombo.select(i);
		}
		
		_NRParentChFMCombo.removeAll();
		_NRParentChFMCombo.add("Any");
		_NRParentChFMCombo.select(0);
		
		_NRParentChFCCombo.removeAll();
		_NRParentChFCCombo.add("Any");
		_NRParentChFCCombo.select(0);

		if(ff.isEmpty() && fm.isEmpty() && fc == 0) {
			if(!_NRParentFFCombo.getText().isEmpty() && !_NRParentFMCombo.getText().isEmpty() && !_NRParentFCCombo.getText().isEmpty())
				fillNRParentChAlarmList(_NRParentFFCombo.getText(),_NRParentFMCombo.getText(),Integer.parseInt(_NRParentFCCombo.getText()));
		}
		else
			fillNRParentChAlarmList(ff,fm,fc);
		//_updateNRParentFF.setEnabled(true);
	}
	
	public void fillNRParentChAlarmList(String ff, String fm, int fc){
		_NRParentErrorMessageLabel.setText("");
		_NRParentChAlarmList.removeAll();
		ReductionRule nrr = _reductionManager.getNRParentByTriplet(ff, fm, fc);
		Alarm parent;
		// This should never happen anyways...
		if( nrr == null ) {
			parent = _alarmManager.getAlarm(ff+":"+fm+":"+fc);
			_NRParentErrorMessageLabel.setText("No Reduction Rule for this triplet.");
		}
		else
			parent = nrr.getParent();
		if(parent == null) {
			_NRParentErrorMessageLabel.setText("Couldn't find the selected Alarm.");
			return;
		}
		List<Alarm> alarms = _alarmManager.getAlarms();
		List<String> tmp = new ArrayList<String>();
		List<Alarm> sortedAlarms = new ArrayList<Alarm>();
		for(Alarm al: alarms)
			tmp.add(al.getIdentifier());
		Collections.sort(tmp,IGNORE_CASE_ORDER);
		for(String sal: tmp)
			sortedAlarms.add(_alarmManager.getAlarm(sal));
		alarms = sortedAlarms;
		try {
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
				if(_NRParentChSelectedRadio.getSelection() && (nrr == null || nrr.getChild(alarm.getAlarmId()) == null))
					continue;
				if(_NRParentChUnselectedRadio.getSelection() && (nrr != null && nrr.getChild(alarm.getAlarmId()) != null))
					continue;
				TableItem  t = new TableItem(_NRParentChAlarmList, SWT.None);
				t.setText(alarm.getAlarmId());
				if(nrr != null) {
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
		} catch(Exception e) {
			_NRParentErrorMessageLabel.setText(e.getMessage());
		}
	}

	private void fillMRParentWidgets(String ff, String fm, int fc) {
		ReductionRule mrr = _reductionManager.getMRParentByTriplet(ff, fm, fc);
		Alarm parent = null;
		// This should happen only when creating a new rule...
		if( mrr == null ) {
			parent = _alarmManager.getAlarm(ff+":"+fm+":"+fc);
			_MRParentErrorMessageLabel.setText("No Reduction Rule for this triplet.");
		}
		else {
			parent = mrr.getParent();
			if (mrr.getChildrenCount() < mrr.getThreshold())
				_MRParentErrorMessageLabel.setText("You need to have at least threshold ("+mrr.getThreshold()+") childs for this triplet.");
		}
		_MRParentFFCombo.removeAll();
		_MRParentChFFCombo.removeAll();
		_MRParentChFFCombo.add("Any");
		_MRParentChFFCombo.select(0);
		
		List<FaultFamily> ffList = _alarmManager.getAllAlarms();
		List<String> tmp = new ArrayList<String>();
		List<FaultFamily> sortedFFList = new ArrayList<FaultFamily>();
		for(FaultFamily tff: ffList)
			tmp.add(tff.getName());
		Collections.sort(tmp, IGNORE_CASE_ORDER);
		for(String sff: tmp)
			sortedFFList.add(_alarmManager.getFaultFamily(sff));
		
		ffList = sortedFFList;
		
		FaultFamily ffs = null;
		int i = -1;
		for (Iterator<FaultFamily> iterator = ffList.iterator(); iterator.hasNext();) {
			FaultFamily fft = iterator.next();
			if(fft.getFaultCodeCount() > 0 && fft.getFaultMemberCount() > 0)
				_MRParentFFCombo.add(fft.getName());
			_MRParentChFFCombo.add(fft.getName());
			if(parent != null && fft.getName().compareTo(parent.getTriplet().getFaultFamily()) == 0){
				i = _MRParentFFCombo.getItemCount() - 1;
				ffs = fft;
			}
		}
		_MRParentFFCombo.select(i);
		
		_MRParentFMCombo.removeAll();
		if(ffs != null) {
			i = -1;
			FaultMember[] fml = ffs.getFaultMember();
			List<FaultMember> fmList = Arrays.asList(fml);
			tmp = new ArrayList<String>();
			List<FaultMember> sortedFMList =  new ArrayList<FaultMember>();
			for(FaultMember tfm: fmList)
				tmp.add(tfm.getName());
			Collections.sort(tmp, IGNORE_CASE_ORDER);
			for(String sfm: tmp)
				for(FaultMember tfm: fmList)
					if(sfm.compareTo(tfm.getName()) == 0)
						sortedFMList.add(tfm);
			
			fmList = sortedFMList;
			
			for (FaultMember fmt: fmList) {
				_MRParentFMCombo.add(fmt.getName());
				if(fmt.getName().compareTo(parent.getTriplet().getFaultMember()) == 0)
					i = _MRParentFMCombo.getItemCount() - 1;
			}
			_MRParentFMCombo.select(i);
		}
		
		_MRParentFCCombo.removeAll();
		if(ffs != null) {
			i = -1;
			FaultCode[] fcl = ffs.getFaultCode();
			List<FaultCode> fcList = Arrays.asList(fcl);
			tmp = new ArrayList<String>();
			List<FaultCode> sortedFCList =  new ArrayList<FaultCode>();
			for(FaultCode tfc: fcList)
				tmp.add(Integer.toString(tfc.getValue()));
			Collections.sort(tmp);
			for(String sfc: tmp)
				for(FaultCode tfc: fcList)
					if(sfc.compareTo(Integer.toString(tfc.getValue())) == 0)
						sortedFCList.add(tfc);
			
			fcList = sortedFCList;
			
			for (FaultCode fct: fcList) {
				_MRParentFCCombo.add(Integer.toString(fct.getValue()));
				if(fct.getValue() == parent.getTriplet().getFaultCode())
					i = _MRParentFCCombo.getItemCount() - 1;
			}
			_MRParentFCCombo.select(i);
		}
		
		_MRParentThresholdText.setText("");
		if(mrr != null)
			_MRParentThresholdText.setText(Integer.toString(mrr.getThreshold()));
		
		_MRParentChFMCombo.removeAll();
		_MRParentChFMCombo.add("Any");
		_MRParentChFMCombo.select(0);
		
		_MRParentChFCCombo.removeAll();
		_MRParentChFCCombo.add("Any");
		_MRParentChFCCombo.select(0);

		if(ff.isEmpty() && fm.isEmpty() && fc == 0) {
			if(!_MRParentFFCombo.getText().isEmpty() && !_MRParentFMCombo.getText().isEmpty() && !_MRParentFCCombo.getText().isEmpty())
				fillMRParentChAlarmList(_MRParentFFCombo.getText(),_MRParentFMCombo.getText(),Integer.parseInt(_MRParentFCCombo.getText()));
		}
		else
			fillMRParentChAlarmList(ff,fm,fc);
		//_updateNRParentFF.setEnabled(true);
	}

	public void fillMRParentChAlarmList(String ff, String fm, int fc) {
		_MRParentErrorMessageLabel.setText("");
		_MRParentChAlarmList.removeAll();
		ReductionRule mrr = _reductionManager.getMRParentByTriplet(ff, fm, fc);
		Alarm parent;
		// This should only happen when creating a new rule...
		if( mrr == null ) {
			parent = _alarmManager.getAlarm(ff+":"+fm+":"+fc);
			_MRParentErrorMessageLabel.setText("No Reduction Rule for this triplet.");
		}
		else
			parent = mrr.getParent();
		if(parent == null) {
			_MRParentErrorMessageLabel.setText("Couldn't find the selected Alarm.");
			return;
		}
		if(_MRParentThresholdText.getText().isEmpty()) {
			_MRParentErrorMessageLabel.setText("Please set a Threshold value.");
			return;
		}
		try {
			Integer.parseInt(_MRParentThresholdText.getText());
		} catch(NumberFormatException e) {
			_MRParentErrorMessageLabel.setText("Incorrect Threshold. A number is required.");
			return;
		}
		List<Alarm> alarms = _alarmManager.getAlarms();
		List<String> tmp = new ArrayList<String>();
		List<Alarm> sortedAlarms = new ArrayList<Alarm>();
		for(Alarm al: alarms)
			tmp.add(al.getIdentifier());
		Collections.sort(tmp, IGNORE_CASE_ORDER);
		for(String sal: tmp)
			sortedAlarms.add(_alarmManager.getAlarm(sal));
		alarms = sortedAlarms;
		try {
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
				if(_MRParentChFilterText.getText().compareTo("")!=0)
					if(!alarm.getAlarmId().matches(_MRParentChFilterText.getText()))
						continue;
				if(_MRParentChSelectedRadio.getSelection() && (mrr == null || mrr.getChild(alarm.getAlarmId()) == null))
					continue;
				if(_MRParentChUnselectedRadio.getSelection() && (mrr != null && mrr.getChild(alarm.getAlarmId()) != null))
					continue;
				TableItem  t = new TableItem(_MRParentChAlarmList, SWT.None);
				t.setText(alarm.getAlarmId());
				if(mrr != null) {
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
		if (mrr != null && mrr.getChildrenCount() < mrr.getThreshold())
			_MRParentErrorMessageLabel.setText("You need to have at least threshold ("+mrr.getThreshold()+") childs for this triplet.");
		} catch(PatternSyntaxException e) {
			_MRParentErrorMessageLabel.setText(e.getMessage());
		}
	}

	@Override
	public void setFocus() {
	}

	public void setEnabled(boolean v) {
		_tree.setEnabled(v);
		_NRParentGroup.setEnabled(v);
		_MRParentGroup.setEnabled(v);
	}

	private String[] getTriplet(String str) {
		str = str.replaceAll("^<","");
		str = str.replaceAll(">$","");
		String[] triplet = str.split(",");
		return triplet;
	}
	
	public void sortReductionRulesList() {
		_tree.removeAll();
		for(int i=0; i!=2; i++) {
			TreeItem iTree = new TreeItem(_tree,SWT.NONE);
			iTree.setText((i==0 ? "Node Reductions" : "Multiplicity Reductions"));
			iTree.setData((i==0 ? NodeType.NODE_REDUCTION : NodeType.MULTIPLICITY_REDUCTION));
			if( i == 0 ) sortNodeReductionRuleList(iTree);
			else sortMultiReductionRuleList(iTree);
		}
	}
	
	public void sortNodeReductionRuleList(TreeItem iTree) {
		List<ReductionRule> rrList = _reductionManager.getNodeReductionRules();
		List<ReductionRule> sortedRRList = new ArrayList<ReductionRule>();
		List<String> tmp = new ArrayList<String>();
		for(ReductionRule rr: rrList) {
			Triplet t = rr.getParent().getTriplet();
			String name = "<" + t.getFaultFamily() + "," + t.getFaultMember() + "," + t.getFaultCode() + ">";
			tmp.add(name);
		}
		Collections.sort(tmp,IGNORE_CASE_ORDER);
		for(String srr: tmp)
			for(ReductionRule rr: rrList) {
				Triplet t = rr.getParent().getTriplet();
				String name = "<" + t.getFaultFamily() + "," + t.getFaultMember() + "," + t.getFaultCode() + ">";
				if(name.compareTo(srr) == 0)
					sortedRRList.add(rr);
			}
		rrList = sortedRRList;
		
		for (ReductionRule rule : rrList) {
			TreeItem kTree = new TreeItem(iTree,SWT.NONE);
			Triplet t = rule.getParent().getTriplet();
			kTree.setText("<" + t.getFaultFamily() + "," + t.getFaultMember() + "," + t.getFaultCode() + ">");
			kTree.setData(NodeType.NODE_REDUCTION_PARENT_DATA);
		}		
	}
	
	public void sortMultiReductionRuleList(TreeItem iTree) {
		List<ReductionRule> rrList = _reductionManager.getMultiReductionRules();
		List<ReductionRule> sortedRRList = new ArrayList<ReductionRule>();
		List<String> tmp = new ArrayList<String>();
		for(ReductionRule rr: rrList) {
			Triplet t = rr.getParent().getTriplet();
			String name = "<" + t.getFaultFamily() + "," + t.getFaultMember() + "," + t.getFaultCode() + ">";
			tmp.add(name);
		}
		Collections.sort(tmp, IGNORE_CASE_ORDER);
		for(String srr: tmp)
			for(ReductionRule rr: rrList) {
				Triplet t = rr.getParent().getTriplet();
				String name = "<" + t.getFaultFamily() + "," + t.getFaultMember() + "," + t.getFaultCode() + ">";
				if(name.compareTo(srr) == 0)
					sortedRRList.add(rr);
			}
		rrList = sortedRRList;
		
		for (ReductionRule rule : rrList) {
			TreeItem kTree = new TreeItem(iTree,SWT.NONE);
			Triplet t = rule.getParent().getTriplet();
			kTree.setText("<" + t.getFaultFamily() + "," + t.getFaultMember() + "," + t.getFaultCode() + ">");
			kTree.setData(NodeType.MULTIPLICITY_REDUCTION_PARENT_DATA);
		}
	}
	
	public void selectElementFromTree(String triplet, boolean isNodeReduction){
		if(triplet == null)
			return;
		TreeItem[] its;
		TreeItem sel = null;
		if(isNodeReduction)
			its = _tree.getItems()[0].getItems();
		else
			its = _tree.getItems()[1].getItems();
		for(TreeItem it: its)
			if(it.getText().compareTo(triplet) == 0)
				sel = it;
		if(sel == null)
			return;
		_tree.setSelection(sel);
		Event e = new Event();
		_tree.notifyListeners(SWT.Selection, e);
	}

	@Override
	public void fillWidgets() {
		// TODO Auto-generated method stub
		
	}
	public void setReadOnly(boolean v){
		if(v) {
			_tree.setMenu(null);
			_NRParentChAlarmList.setMenu(null);
			_NRParentChAlarmList.removeListener(SWT.KeyUp, _addRule);
			_NRParentChAlarmList.removeListener(SWT.MouseDoubleClick, _addRule);
			_MRParentChAlarmList.setMenu(null);
			_MRParentChAlarmList.removeListener(SWT.KeyUp, _addRule);
			_MRParentChAlarmList.removeListener(SWT.MouseDoubleClick, _addRule);
		} else {
			_tree.setMenu(null);
			_NRParentChAlarmList.setMenu(null);
			_NRParentChAlarmList.addListener(SWT.KeyUp, _addRule);
			_NRParentChAlarmList.addListener(SWT.MouseDoubleClick, _addRule);
			_MRParentChAlarmList.setMenu(null);
			_MRParentChAlarmList.addListener(SWT.KeyUp, _addRule);
			_MRParentChAlarmList.addListener(SWT.MouseDoubleClick, _addRule);
		}
		_NRParentFFCombo.setEnabled(!v);
		_NRParentFMCombo.setEnabled(!v);
		_NRParentFCCombo.setEnabled(!v);
		_MRParentFFCombo.setEnabled(!v);
		_MRParentFMCombo.setEnabled(!v);
		_MRParentFCCombo.setEnabled(!v);
		_MRParentThresholdText.setEnabled(!v);
		//
		//private Button _NRParentChAllRadio;
		//private Button _NRParentChSelectedRadio;
		//private Button _NRParentChUnselectedRadio;
		//private Combo _NRParentChFFCombo;
		//private Combo _NRParentChFMCombo;
		//private Combo _NRParentChFCCombo;
		//private Text  _NRParentChFilterText;
	
		//private Button _MRParentChAllRadio;
		//private Button _MRParentChSelectedRadio;
		//private Button _MRParentChUnselectedRadio;
		//private Combo _MRParentChFFCombo;
		//private Combo _MRParentChFMCombo;
		//private Combo _MRParentChFCCombo;
		//private Text  _MRParentChFilterText;
		//private Table _MRParentChAlarmList;
	}
	
	static final Comparator<String> IGNORE_CASE_ORDER =
		new Comparator<String>() {
			public int compare(String e1, String e2) {
				return e1.toLowerCase().compareTo(e2.toLowerCase());
			}
	};
}
