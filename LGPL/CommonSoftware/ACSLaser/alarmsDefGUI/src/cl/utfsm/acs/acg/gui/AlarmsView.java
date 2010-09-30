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
import java.util.Collections;
import java.util.List;

import org.apache.xerces.util.URI;
import org.apache.xerces.util.URI.MalformedURIException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
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
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import cl.utfsm.acs.acg.core.AlarmManager;
import cl.utfsm.acs.acg.core.SourceManager;
import cl.utfsm.acs.acg.core.CategoryManager;
import cl.utfsm.acs.acg.core.AlarmSystemManager;
import cl.utfsm.acs.acg.core.IllegalOperationException;

import alma.acs.alarmsystem.generated.Alarms;
import alma.acs.alarmsystem.generated.FaultCode;
import alma.acs.alarmsystem.generated.FaultFamily;
import alma.acs.alarmsystem.generated.FaultMember;
import alma.acs.alarmsystem.generated.FaultMemberDefault;
import alma.acs.alarmsystem.generated.Contact;
import alma.acs.alarmsystem.generated.Location;
import alma.acs.alarmsystem.generated.Category;
import cern.laser.business.data.Source;
import org.eclipse.swt.widgets.Shell;


public class AlarmsView extends ViewPart implements IMyViewPart {

	public static String ID = "cl.utfsm.acs.acg.gui.alarmsview";

	/**
	 * Type used to identify the different type of nodes of the alarm tree
	 * that is shown in the left side of this view.
	 * Depending on the selected node, we must show different
	 * things on the right side of the view.
	 */
	private enum NodeType {
		FAULT_FAMILY,
		FAULT_CODE_LIST,
		FAULT_CODE_DATA,
		FAULT_MEMBER_DATA,
		FAULT_MEMBER_LIST,
		FAULT_MEMBER_DEFAULT
	}
	
	private Shell _shell;

	private AlarmManager _alarmManager;
	private SourceManager _sourceManager;
	private CategoryManager _categoryManager;
	
	/* High level widgets */
	private Composite _alarmsComp;
	private Composite _compInitial;
	private SashForm _sash;
	
	/* Left side widgets */
	private Tree _tree;
	private Composite _alarmsButtonsComp;
	private Button _addAlarmButton;
	private Button _deleteAlarmButton;
	private Group _treeGroup;
	
	/* FF information */
	private Text  _ffNameText;
	private Label _ffNameLabel;
	private Text  _ffHelpURLText;
	private Label _ffHelpURLLabel;
	private Text  _ffContactNameText;
	private Label _ffContactNameLabel;
	private Text  _ffContactMailText;
	private Label _ffContactMailLabel;
	private Text  _ffContactGSMText;
	private Label _ffContactGSMLabel;
	private Label _ffSourceLabel;
	private Combo _ffSourceCombo;
	private Label _ffCategoryLabel;
	private Table _ffCategoryList;
	private Label _ffErrorMessageLabel;
	private Group _FFgroup;

	/* FC and FM List information */
	private Label _fcfmLabel;
	private Group _FCFMgroup;
	
	/* FC information */
	private Text  _fcValueText;
	private Label _fcValueLabel;
	private Text  _fcPriorityText;
	private Label _fcPriorityLabel;
	private Text  _fcCauseText;
	private Label _fcCauseLabel;
	private Text  _fcActionText;
	private Label _fcActionLabel;
	private Text  _fcConsequenceText;
	private Label _fcConsequenceLabel;
	private Text  _fcProblemText;
	private Label _fcProblemLabel;
	private Label _fcErrorMessageLabel;
	private Group _FCgroup;

	/* FM information */
	private Text  _fmNameText;
	private Label _fmNameLabel;
	private Text  _fmLocBuildingText;
	private Label _fmLocBuildingLabel;
	private Text  _fmLocRoomText;
	private Label _fmLocRoomLabel;
	private Text  _fmLocFloorText;
	private Label _fmLocFloorLabel;
	private Text  _fmLocMnemonicText;
	private Label _fmLocMnemonicLabel;
	private Text  _fmLocPositionText;
	private Label _fmLocPositionLabel;
	private Group _fmLocGroup;
	private Label _fmErrorMessageLabel;
	private Group _FMgroup;
	
	/* FMD information */
	/* FM information */
	private Text  _fmdLocBuildingText;
	private Label _fmdLocBuildingLabel;
	private Text  _fmdLocRoomText;
	private Label _fmdLocRoomLabel;
	private Text  _fmdLocFloorText;
	private Label _fmdLocFloorLabel;
	private Text  _fmdLocMnemonicText;
	private Label _fmdLocMnemonicLabel;
	private Text  _fmdLocPositionText;
	private Label _fmdLocPositionLabel;
	private Group _fmdLocGroup;
	private Label _fmdErrorMessageLabel;
	private Group _FMDgroup;
	
	/* Listeners*/
	Listener _addFaultFamily;
	Listener _updateFaultFamily;
	Listener _deleteElement;
	Listener _addFaultCode;
	Listener _addFaultMember;
	Listener _addFaultMemberDefault;
	Listener _addCategory;
	
	@Override
	public void createPartControl(Composite parent) {
		setTitleToolTip("Configuration of Fault Families, Fault Members and Fault Codes");
		setTitleImage(Activator.getDefault().getImageRegistry().get(Activator.IMG_ALARM));
		_shell = parent.getShell();
		createViewWidgets(parent);
		refreshContents();
	}

	/* (non-Javadoc)
	 * @see cl.utfsm.acs.acg.gui.IMyViewPart#refreshContents()
	 */
	public void refreshContents() {

		/* We must fill the Tree with the FF, FM and FCs */
		_alarmManager = AlarmSystemManager.getInstance().getAlarmManager();
		_sourceManager = AlarmSystemManager.getInstance().getSourceManager();
		_categoryManager = AlarmSystemManager.getInstance().getCategoryManager();
		
		/*TODO
		List<FaultFamily> ffList = _alarmManager.getAllAlarms();
		List<FaultFamily> sortedFFList = new ArrayList<FaultFamily>();

		List<String> tmp = new ArrayList<String>();
		for (Iterator<FaultFamily> iterator = ffList.iterator(); iterator.hasNext();) {
			tmp.add(((FaultFamily)iterator.next()).getName().toLowerCase());
		}
		Collections.sort(tmp);
		for (Iterator<String> iterator = tmp.iterator(); iterator.hasNext();) {
			String name = (String) iterator.next();
			for (Iterator<FaultFamily> iterator2 = ffList.iterator(); iterator2.hasNext();) {
				FaultFamily ff = (FaultFamily) iterator2.next();
				if( ff.getName().toLowerCase().compareTo(name) == 0 ) {
					sortedFFList.add(ff);
					break;
				}
			}
		}
		ffList = sortedFFList;

		_tree.removeAll();
		for (FaultFamily family : ffList) {
			TreeItem iTree = new TreeItem(_tree,SWT.NONE);
			iTree.setData(NodeType.FAULT_FAMILY);
			iTree.setText(family.getName());
			iTree.setImage(Activator.getDefault().getImageRegistry().get(Activator.IMG_ALARM));
			for(int j=0; j!=2; j++) {
				TreeItem jTree = new TreeItem(iTree,SWT.NONE);
				jTree.setText((j==0 ? "Fault Codes" : "Fault Members"));
				jTree.setData((j==0 ? NodeType.FAULT_CODE_LIST : NodeType.FAULT_MEMBER_LIST));
				jTree.setImage((j==0?
						Activator.getDefault().getImageRegistry().get(Activator.IMG_FAULTCODES) :
						Activator.getDefault().getImageRegistry().get(Activator.IMG_FAULTMEMBERS) ));
				
				if( j == 0 ) {
					FaultCode[] faultCodes = family.getFaultCode();
					for (int i = 0; i < faultCodes.length; i++) {
						TreeItem kTree = new TreeItem(jTree, SWT.NONE);
						kTree.setText(Integer.toString(faultCodes[i].getValue()));
						kTree.setData(NodeType.FAULT_CODE_DATA);
					}
					if(faultCodes.length == 0) {
						iTree.setForeground(new Color(iTree.getDisplay(), 255, 0, 0));
						jTree.setForeground(new Color(jTree.getDisplay(), 255, 0, 0));
					}
				}
				else {
					FaultMember[] faultMembers = family.getFaultMember();
					FaultMemberDefault fmd = family.getFaultMemberDefault();
					if( fmd != null ) {
						TreeItem kTree = new TreeItem(jTree,SWT.NONE);
						kTree.setText("Default Member");
						//kTree.setForeground(new Color(kTree.getDisplay(), 255, 0, 0));
						kTree.setData(NodeType.FAULT_MEMBER_DEFAULT);
					}
					for (int i = 0; i < faultMembers.length; i++) {
						TreeItem kTree = new TreeItem(jTree,SWT.NONE);
						kTree.setText(faultMembers[i].getName());
						kTree.setData(NodeType.FAULT_MEMBER_DATA);
						
					}
					if(faultMembers.length == 0 && fmd == null) {
						iTree.setForeground(new Color(iTree.getDisplay(), 255, 0, 0));
						jTree.setForeground(new Color(jTree.getDisplay(), 255, 0, 0));
					}
				}
			}
		}
		*/
		sortFaultFamilyList();
		_tree.deselectAll();
		_FFgroup.setVisible(false);
		((GridData)_FFgroup.getLayoutData()).exclude = true;
		_FMgroup.setVisible(false);
		((GridData)_FMgroup.getLayoutData()).exclude = true;
		_FCgroup.setVisible(false);
		((GridData)_FCgroup.getLayoutData()).exclude = true;
		_FMDgroup.setVisible(false);
		((GridData)_FMDgroup.getLayoutData()).exclude = true;
		_FCFMgroup.setVisible(false);
		((GridData)_FCFMgroup.getLayoutData()).exclude = true;
	}

	private void createViewWidgets(Composite parent) {
		Listener hoverTree = new Listener() {
			public void handleEvent(Event event){
				Point coords = new Point(event.x,event.y);
				TreeItem it = _tree.getItem(coords);
				String tooltip = "";
				if(it == null){
					_tree.setToolTipText(tooltip);
					return;
				}
				NodeType type = (NodeType) it.getData();
				switch(type){
					case FAULT_FAMILY:{
						tooltip = _alarmManager.getFaultFamily(it.getText()).getName();
						break;
					}
					case FAULT_CODE_DATA:{
						tooltip = _alarmManager.getFaultCode(it.getParentItem().getParentItem().getText(), new Integer(it.getText())).getProblemDescription();
						break;
					}
					case FAULT_MEMBER_DATA:{
						tooltip = _alarmManager.getFaultMember(it.getParentItem().getParentItem().getText(), it.getText()).getName();
						break;
					}
				}
				_tree.setToolTipText(tooltip);
			}
		};
		_deleteElement = new Listener() {
			public void handleEvent(Event event) {
			      boolean choice = MessageDialog.openQuestion( 
		        		  AlarmsView.this.getViewSite().getShell(),
		            "Confirmation",
		            "Are you sure you want to delete this element");
			      if (choice == true ){
			      
				TreeItem sel = null;
				if(_tree.getSelection() == null || _tree.getSelection().length == 0)
					return;
				NodeType type = (NodeType)_tree.getSelection()[0].getData();
				if(type == NodeType.FAULT_CODE_LIST || type == NodeType.FAULT_MEMBER_LIST)
					return;
				String alarm = _tree.getSelection()[0].getText();
				try {
					if(type == NodeType.FAULT_FAMILY)
						_alarmManager.deleteFaultFamily(_alarmManager.getFaultFamily(alarm));
					else if(type == NodeType.FAULT_CODE_DATA){
						String ff = _tree.getSelection()[0].getParentItem().getParentItem().getText();
						_alarmManager.deleteFaultCode(_alarmManager.getFaultFamily(ff),_alarmManager.getFaultCode(ff,new Integer(alarm).intValue()));
					} else if(type == NodeType.FAULT_MEMBER_DATA){
						String ff = _tree.getSelection()[0].getParentItem().getParentItem().getText();
						_alarmManager.deleteFaultMember(_alarmManager.getFaultFamily(ff),_alarmManager.getFaultMember(ff,alarm));
					} else if(type == NodeType.FAULT_MEMBER_DEFAULT) {
						String ff = _tree.getSelection()[0].getParentItem().getParentItem().getText();
						_alarmManager.setFaultMemberDefault(_alarmManager.getFaultFamily(ff),null);
					}
				} catch (IllegalOperationException e){
					ErrorDialog error = new ErrorDialog(AlarmsView.this.getViewSite().getShell(),
							"Cannot delete the item",
							e.getMessage(),
							new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.getMessage()),
							IStatus.ERROR);
					error.setBlockOnOpen(true);
					error.open();
					return;
				} catch (NullPointerException e){
					ErrorDialog error = new ErrorDialog(AlarmsView.this.getViewSite().getShell(),
							"Cannot delete the item",
							e.getMessage(),
							new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.getMessage()),
							IStatus.ERROR);
					error.setBlockOnOpen(true);
					error.open();
					return;
				}
				if(type != NodeType.FAULT_FAMILY){
					sel = _tree.getSelection()[0].getParentItem();
					TreeItem tff = sel.getParentItem();
					FaultFamily fft = _alarmManager.getFaultFamily(tff.getText());
					if(fft.getFaultCodeCount() == 0 || (fft.getFaultMemberCount() == 0 && fft.getFaultMemberDefault() == null)) {
						sel.setForeground(new Color(sel.getDisplay(), 255, 0, 0));
						tff.setForeground(new Color(tff.getDisplay(), 255, 0, 0));
					} else {
						sel.setForeground(new Color(sel.getDisplay(), 0, 0, 0));
						tff.setForeground(new Color(tff.getDisplay(), 0, 0, 0));
					}
					_tree.getSelection()[0].dispose();
					_tree.setSelection(sel);
					Event e = new Event();
					_tree.notifyListeners(SWT.Selection, e);
				}
				else{
					_tree.getSelection()[0].dispose();
					if(_tree.getItemCount() > 0)
						_tree.setSelection(_tree.getItems()[0]);
					Event e = new Event();
					_tree.notifyListeners(SWT.Selection, e);
				}
			}
		}
			
		};
		
		_addFaultMember = new Listener() {
			public void handleEvent(Event event) {
				TreeItem tmp = _tree.getSelection()[0];
				TreeItem tmp2 = null;
				while(tmp != null){
					tmp2 = tmp;
					tmp = tmp.getParentItem();
				}
				String ff = tmp2.getText();
				InputDialog dialog = new InputDialog(AlarmsView.this.getViewSite().getShell(),
                        "New Fault Member",
                        "Enter the Fault Member name",
                        null,
                        new IInputValidator() {
							public String isValid(String newText) {
								if( newText.trim().compareTo("") == 0 )
									return "The name is empty";
								return null;
							}
						 }
				);
				dialog.setBlockOnOpen(true);
				dialog.open();
				int returnCode = dialog.getReturnCode();
				if (returnCode == InputDialog.OK) {
					FaultMember newFaultMember = new FaultMember();
					newFaultMember.setName(dialog.getValue());
					try {
						_alarmManager.addFaultMember(_alarmManager.getFaultFamily(ff),newFaultMember);
					} catch (IllegalOperationException e) {
						ErrorDialog error = new ErrorDialog(AlarmsView.this.getViewSite().getShell(),
								"Cannot add the new Fault Member",
								e.getMessage(),
								new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.getMessage()),
								IStatus.ERROR);
						error.setBlockOnOpen(true);
						error.open();
						return;
					} catch (NullPointerException e) {
						ErrorDialog error = new ErrorDialog(AlarmsView.this.getViewSite().getShell(),
								"Cannot add the new Fault Member",
								e.getMessage(),
								new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.getMessage()),
								IStatus.ERROR);
						error.setBlockOnOpen(true);
						error.open();
						return;
					}
					sortFaultFamilyList();
					selectElementFromTree(ff, dialog.getValue(), null);
				}
			}
		};
		
		_addFaultMemberDefault = new Listener() {
			public void handleEvent(Event event) {
				TreeItem tmp = _tree.getSelection()[0];
				TreeItem tmp2 = null;
				while(tmp != null){
					tmp2 = tmp;
					tmp = tmp.getParentItem();
				}
				String ff = tmp2.getText();
				FaultMemberDefault newFaultMemberDefault = new FaultMemberDefault();
				try {
					_alarmManager.setFaultMemberDefault(_alarmManager.getFaultFamily(ff),newFaultMemberDefault);
				} catch (IllegalOperationException e) {
					ErrorDialog error = new ErrorDialog(AlarmsView.this.getViewSite().getShell(),
							"Cannot add the new Default Fault Member",
							e.getMessage(),
							new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.getMessage()),
							IStatus.ERROR);
					error.setBlockOnOpen(true);
					error.open();
					return;
				} catch (NullPointerException e) {
					ErrorDialog error = new ErrorDialog(AlarmsView.this.getViewSite().getShell(),
							"Cannot add the new Default Fault Member",
							e.getMessage(),
							new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.getMessage()),
							IStatus.ERROR);
					error.setBlockOnOpen(true);
					error.open();
					return;
				}
				sortFaultFamilyList();
				selectElementFromTree(ff, "Default Member", null);
			}
		};
		
		_addFaultCode = new Listener() {
			public void handleEvent(Event event) {
				TreeItem tmp = _tree.getSelection()[0];
				TreeItem tmp2 = null;
				while(tmp != null){
					tmp2 = tmp;
					tmp = tmp.getParentItem();
				}
				String ff = tmp2.getText();
				InputDialog dialog = new InputDialog(AlarmsView.this.getViewSite().getShell(),
                        "New Fault Code",
                        "Enter the Fault Code Value",
                        null,
                        new IInputValidator() {
							public String isValid(String newText) {
								if( newText.trim().compareTo("") == 0 )
									return "The value is empty";
								try{
									new Integer(newText);
								}catch(NumberFormatException e){
									return "The value is not a number"; 
								}
								return null;
							}
						 }
				);
				dialog.setBlockOnOpen(true);
				dialog.open();
				int returnCode = dialog.getReturnCode();
				if (returnCode == InputDialog.OK) {
					FaultCode newFaultCode = new FaultCode();
					newFaultCode.setValue(new Integer(dialog.getValue()).intValue());
					try {
						_alarmManager.addFaultCode(_alarmManager.getFaultFamily(ff),newFaultCode);
					} catch (IllegalOperationException e) {
						ErrorDialog error = new ErrorDialog(AlarmsView.this.getViewSite().getShell(),
								"Cannot add the new Fault Code",
								e.getMessage(),
								new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.getMessage()),
								IStatus.ERROR);
						error.setBlockOnOpen(true);
						error.open();
						return;
					} catch (NullPointerException e) {
						ErrorDialog error = new ErrorDialog(AlarmsView.this.getViewSite().getShell(),
								"Cannot add the new Fault Code",
								e.getMessage(),
								new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.getMessage()),
								IStatus.ERROR);
						error.setBlockOnOpen(true);
						error.open();
						return;
					}
					sortFaultFamilyList();
					selectElementFromTree(ff, null, dialog.getValue());
				}
			}
		};
		
		_addFaultFamily = new Listener() {
			public void handleEvent(Event event) {
				InputDialog dialog = new InputDialog(AlarmsView.this.getViewSite().getShell(),
				                         "New Alarm",
				                         "Enter the Fault Family name",
				                         null,
				                         new IInputValidator() {
											public String isValid(String newText) {
												if( newText.trim().compareTo("") == 0 )
													return "The name is empty";
												return null;
											}
										 }
				);
				dialog.setBlockOnOpen(true);
				dialog.open();
				int returnCode = dialog.getReturnCode();
				if (returnCode == InputDialog.OK) {
					FaultFamily newAlarm = new FaultFamily();
					newAlarm.setName(dialog.getValue());
					try {
						_alarmManager.addFaultFamily(newAlarm);
					} catch (IllegalOperationException e) {
						ErrorDialog error = new ErrorDialog(AlarmsView.this.getViewSite().getShell(),
											"Cannot add the new Alarm",
											e.getMessage(),
											new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.getMessage()),
											IStatus.ERROR);
						error.setBlockOnOpen(true);
						error.open();
					} catch (NullPointerException e) {
						ErrorDialog error = new ErrorDialog(AlarmsView.this.getViewSite().getShell(),
											"Cannot add the new Alarm",
											e.getMessage(),
											new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.getMessage()),
											IStatus.ERROR);
						error.setBlockOnOpen(true);
						error.open();
						return;
					}
					sortFaultFamilyList();
					selectElementFromTree(dialog.getValue(), null, null);
				}
			}
		};
		
		_sash = new SashForm(parent,SWT.NONE);
		_sash.setLayout(new FillLayout());

		_alarmsComp = new Composite(_sash,SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.numColumns = 1;
		_alarmsComp.setLayout(layout);
		
		_treeGroup = new Group(_alarmsComp,SWT.SHADOW_ETCHED_IN);
		GridData gd = new GridData();
		gd.horizontalAlignment = SWT.FILL;
		gd.verticalAlignment = SWT.FILL;
		gd.grabExcessHorizontalSpace = true;
		gd.grabExcessVerticalSpace = true;
		_treeGroup.setLayoutData(gd);
		GridLayout gl = new GridLayout();
		gl.numColumns = 1;
		_treeGroup.setLayout(gl);
		_treeGroup.setText("Fault Family List");
		
		/* The tree used to list the FF, FM and FCs */
		_tree = new Tree(_treeGroup,SWT.VIRTUAL | SWT.BORDER);
		gd = new GridData();
		gd.verticalAlignment = SWT.FILL;
		gd.horizontalAlignment = SWT.FILL;
		gd.grabExcessVerticalSpace = true;
		gd.grabExcessHorizontalSpace = true;
		_tree.setLayoutData(gd);
		//Menu treePopUp = new Menu(parent, SWT.POP_UP);
		Menu treePopUp = new Menu(parent);
		_tree.setMenu(treePopUp);
		treePopUp.addListener(SWT.Show, new Listener() {
			public void handleEvent(Event e) {
				//Point point = new Point(e.x, e.y);
				//TreeItem sel = _tree.getItem(point);
				TreeItem[] sel = _tree.getSelection();
				Menu treePopUp = _tree.getMenu();
				MenuItem[] items = treePopUp.getItems();
				for (int i = 0; i < items.length; i++)
					items[i].dispose();
				MenuItem mitem;
				if(sel == null || sel.length == 0) {
					mitem = new MenuItem(treePopUp,SWT.PUSH);
					mitem.setText("Add Fault Family");
					mitem.addListener(SWT.Selection, _addFaultFamily);
					return;
				}
				NodeType type = (NodeType) sel[0].getData();
				switch(type) {
				case FAULT_FAMILY:
					mitem = new MenuItem(treePopUp,SWT.PUSH);
					mitem.setText("Add Fault Family");
					mitem.addListener(SWT.Selection, _addFaultFamily);
					mitem = new MenuItem(treePopUp,SWT.PUSH);
					mitem.setText("Delete Fault Family");
					mitem.addListener(SWT.Selection, _deleteElement);
					break;
				case FAULT_CODE_LIST:
					mitem = new MenuItem(treePopUp,SWT.PUSH);
					mitem.setText("Add Fault Code");
					mitem.addListener(SWT.Selection, _addFaultCode);
					break;
				case FAULT_CODE_DATA:
					mitem = new MenuItem(treePopUp,SWT.PUSH);
					mitem.setText("Add Fault Code");
					mitem.addListener(SWT.Selection, _addFaultCode);
					mitem = new MenuItem(treePopUp,SWT.PUSH);
					mitem.setText("Delete Fault Code");
					mitem.addListener(SWT.Selection, _deleteElement);
					break;
				case FAULT_MEMBER_LIST:
					mitem = new MenuItem(treePopUp,SWT.PUSH);
					mitem.setText("Add Fault Member");
					mitem.addListener(SWT.Selection, _addFaultMember);
					if(_alarmManager.getFaultFamily(sel[0].getParentItem().getText()).getFaultMemberDefault() == null) {
						mitem = new MenuItem(treePopUp,SWT.PUSH);
						mitem.setText("Add Default Fault Member");
						mitem.addListener(SWT.Selection, _addFaultMemberDefault);
					}
					break;
				case FAULT_MEMBER_DATA:
					mitem = new MenuItem(treePopUp,SWT.PUSH);
					mitem.setText("Add Fault Member");
					mitem.addListener(SWT.Selection, _addFaultMember);
					mitem = new MenuItem(treePopUp,SWT.PUSH);
					mitem.setText("Delete Fault Member");
					mitem.addListener(SWT.Selection, _deleteElement);
					break;
				case FAULT_MEMBER_DEFAULT:
					mitem = new MenuItem(treePopUp,SWT.PUSH);
					mitem.setText("Delete Default Fault Member");
					mitem.addListener(SWT.Selection, _deleteElement);
					break;
				default:
					for (int i = 0; i < items.length; i++)
						items[i].dispose();
					break;
				}
			}
		}
		);
		_tree.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}

			public void widgetSelected(SelectionEvent e) {
				TreeItem []tmp = ((Tree)e.widget).getSelection();
				if( tmp == null || tmp.length == 0){
					_FFgroup.setVisible(false);
					((GridData)_FFgroup.getLayoutData()).exclude = true;
					_FMgroup.setVisible(false);
					((GridData)_FMgroup.getLayoutData()).exclude = true;
					_FCgroup.setVisible(false);
					((GridData)_FCgroup.getLayoutData()).exclude = true;
					_FMDgroup.setVisible(false);
					((GridData)_FMDgroup.getLayoutData()).exclude = true;
					_FCFMgroup.setVisible(false);
					((GridData)_FCFMgroup.getLayoutData()).exclude = true;
					return;
				}

				TreeItem item = tmp[0];
				NodeType type = (NodeType)item.getData();

				/* Delete the label the first time we select something */
				Control c = _compInitial.getChildren()[0];
				if( c instanceof Label ) {
					c.dispose();
					_compInitial.layout();
					c = _compInitial.getChildren()[0];
				}

				if( type == NodeType.FAULT_FAMILY ) {
					_FFgroup.setVisible(true);
					((GridData)_FFgroup.getLayoutData()).exclude = false;
					_FMgroup.setVisible(false);
					((GridData)_FMgroup.getLayoutData()).exclude = true;
					_FCgroup.setVisible(false);
					((GridData)_FCgroup.getLayoutData()).exclude = true;
					_FMDgroup.setVisible(false);
					((GridData)_FMDgroup.getLayoutData()).exclude = true;
					_FCFMgroup.setVisible(false);
					((GridData)_FCFMgroup.getLayoutData()).exclude = true;

					//_FFgroup.moveAbove(c);
					fillFFWidgets(item.getText());
				}
				else if( type == NodeType.FAULT_CODE_LIST ) {
					_FFgroup.setVisible(false);
					((GridData)_FFgroup.getLayoutData()).exclude = true;
					_FMgroup.setVisible(false);
					((GridData)_FMgroup.getLayoutData()).exclude = true;
					_FCgroup.setVisible(false);
					((GridData)_FCgroup.getLayoutData()).exclude = true;
					_FMDgroup.setVisible(false);
					((GridData)_FMDgroup.getLayoutData()).exclude = true;
					_FCFMgroup.setVisible(true);
					((GridData)_FCFMgroup.getLayoutData()).exclude = false;
					fillFCFMWidgets();
				}
				else if( type == NodeType.FAULT_CODE_DATA ) {
					_FFgroup.setVisible(false);
					((GridData)_FFgroup.getLayoutData()).exclude = true;
					_FMgroup.setVisible(false);
					((GridData)_FMgroup.getLayoutData()).exclude = true;
					_FCgroup.setVisible(true);
					((GridData)_FCgroup.getLayoutData()).exclude = false;
					_FMDgroup.setVisible(false);
					((GridData)_FMDgroup.getLayoutData()).exclude = true;
					_FCFMgroup.setVisible(false);
					((GridData)_FCFMgroup.getLayoutData()).exclude = true;

					//_FCgroup.moveAbove(c);
					fillFCWidgets(Integer.parseInt(item.getText()), item.getParentItem().getParentItem().getText());
				}
				else if( type == NodeType.FAULT_MEMBER_LIST ) {
					_FFgroup.setVisible(false);
					((GridData)_FFgroup.getLayoutData()).exclude = true;
					_FMgroup.setVisible(false);
					((GridData)_FMgroup.getLayoutData()).exclude = true;
					_FCgroup.setVisible(false);
					((GridData)_FCgroup.getLayoutData()).exclude = true;
					_FMDgroup.setVisible(false);
					((GridData)_FMDgroup.getLayoutData()).exclude = true;
					_FCFMgroup.setVisible(true);
					((GridData)_FCFMgroup.getLayoutData()).exclude = false;
					fillFCFMWidgets();
				}
				else if( type == NodeType.FAULT_MEMBER_DATA ) {
					_FFgroup.setVisible(false);
					((GridData)_FFgroup.getLayoutData()).exclude = true;
					_FMgroup.setVisible(true);
					((GridData)_FMgroup.getLayoutData()).exclude = false;
					_FCgroup.setVisible(false);
					((GridData)_FCgroup.getLayoutData()).exclude = true;
					_FMDgroup.setVisible(false);
					((GridData)_FMDgroup.getLayoutData()).exclude = true;
					_FCFMgroup.setVisible(false);
					((GridData)_FCFMgroup.getLayoutData()).exclude = true;

					//_FMgroup.moveAbove(c);
					fillFMWidgets(item.getText(), item.getParentItem().getParentItem().getText());
				}
				else if( type == NodeType.FAULT_MEMBER_DEFAULT ) {
					_FFgroup.setVisible(false);
					((GridData)_FFgroup.getLayoutData()).exclude = true;
					_FMgroup.setVisible(false);
					((GridData)_FMgroup.getLayoutData()).exclude = true;
					_FCgroup.setVisible(false);
					((GridData)_FCgroup.getLayoutData()).exclude = true;
					_FMDgroup.setVisible(true);
					((GridData)_FMDgroup.getLayoutData()).exclude = false;
					_FCFMgroup.setVisible(false);
					((GridData)_FCFMgroup.getLayoutData()).exclude = true;
					
					fillFMDWidgets(item.getParentItem().getParentItem().getText());
				}
				_compInitial.layout();
			}
		
		});
		_tree.addListener(SWT.MouseHover, hoverTree);
		
		_alarmsButtonsComp = new Composite(_alarmsComp,SWT.NONE);
		layout = new GridLayout();
		layout.numColumns = 2;
		_alarmsButtonsComp.setLayout(layout);
		
		_addAlarmButton = new Button(_alarmsButtonsComp, SWT.None);
		_addAlarmButton.setText("Add");
		_addAlarmButton.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_ADD));
		
		_deleteAlarmButton = new Button(_alarmsButtonsComp, SWT.None);
		_deleteAlarmButton.setText("Delete");
		_deleteAlarmButton.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_ETOOL_DELETE));
				
		_addAlarmButton.addListener(SWT.Selection, _addFaultFamily);

		_deleteAlarmButton.addListener(SWT.Selection, _deleteElement);
		
		/* Top widget of the right side */
		_compInitial = new Composite(_sash, SWT.SHADOW_ETCHED_IN);
		_compInitial.setLayout(new GridLayout());

		new Label(_compInitial,SWT.NONE).setText("Select an element");
		
		/* FF/FM/FC Details */
		createFFWidgets();
		createFCWidgets();
		createFMWidgets();
		createFMDWidgets();
		createFCFMWidgets();

		/* At the beginning we only show a label */
		_FFgroup.setVisible(false);
		_FCgroup.setVisible(false);
		_FMgroup.setVisible(false);
		_FMDgroup.setVisible(false);
		_FCFMgroup.setVisible(false);

		_sash.setWeights(new int[] {3, 5});
	}

	private void createFCWidgets() {
		Listener updateFaultCode = new Listener() {
			public void handleEvent(Event event) {
				int val;
				if(_tree.getSelection() == null || _tree.getSelection().length == 0)
					return;
				TreeItem tmp = _tree.getSelection()[0];
				int tfc = Integer.parseInt(tmp.getText());
				String tff = tmp.getParentItem().getParentItem().getText();
				FaultCode fct = new FaultCode();
				try{
					val = Integer.parseInt(_fcValueText.getText());
					if(val <= 0) {
						_fcErrorMessageLabel.setText("FaultCode is Negative or Zero. A positive number is required.");
						return;
					}
					fct.setValue(val);
				}catch(NumberFormatException e){
					_fcErrorMessageLabel.setText("FaultCode is not a Number. A positive number is required.");
					return;
				}
				try{
					val = Integer.parseInt(_fcPriorityText.getText());
					if(val < 0 || val > 3) {
						_fcErrorMessageLabel.setText("Incorrect Priority. A number in the range [0;3] is required.");
						return;
					}
					fct.setPriority(val);
				}catch(NumberFormatException e){
					_fcErrorMessageLabel.setText("Priority is not a number. A number in the range [0;3] is required.");
					return;
				}
				if(!_fcCauseText.getText().isEmpty())
					fct.setCause(_fcCauseText.getText());
				if(!_fcActionText.getText().isEmpty())
					fct.setAction(_fcActionText.getText());
				if(!_fcConsequenceText.getText().isEmpty())
					fct.setConsequence(_fcConsequenceText.getText());
				if(_fcProblemText.getText().isEmpty()) {
					_fcErrorMessageLabel.setText("Problem Description is Required.");
					return;
				}
				fct.setProblemDescription(_fcProblemText.getText());
				_fcErrorMessageLabel.setText("");
				try{
					_alarmManager.updateFaultCode(_alarmManager.getFaultFamily(tff), _alarmManager.getFaultCode(tff, tfc),fct);
					tmp.setText(_fcValueText.getText());
					if(fct.getValue() != tfc) {
						sortFaultFamilyList();
						selectElementFromTree(tff, null, Integer.toString(fct.getValue()));
					}
				}catch(IllegalOperationException e){
					_fcErrorMessageLabel.setText(e.getMessage());
				}catch(NullPointerException e){
					_fcErrorMessageLabel.setText(e.getMessage());
				}
			}
		};

		_FCgroup = new Group(_compInitial,SWT.SHADOW_ETCHED_IN);
		_FCgroup.setText("Fault Code detail");
		GridData gd = new GridData();
		gd.horizontalAlignment = SWT.FILL;
		gd.grabExcessHorizontalSpace = true;
		gd.verticalAlignment = SWT.FILL;
		gd.grabExcessVerticalSpace = true;
		_FCgroup.setLayoutData(gd);
		GridLayout gl = new GridLayout();
		gl.numColumns = 2;
		_FCgroup.setLayout(gl);

		_fcValueLabel = new Label(_FCgroup,SWT.NONE);
		_fcValueLabel.setText("Value");
		_fcValueText = new Text(_FCgroup,SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_fcValueText.setLayoutData(gd);
		_fcValueText.addListener(SWT.Modify,updateFaultCode);

		_fcPriorityLabel = new Label(_FCgroup,SWT.NONE);
		_fcPriorityLabel.setText("Priority");
		_fcPriorityText = new Text(_FCgroup,SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_fcPriorityText.setLayoutData(gd);
		_fcPriorityText.addListener(SWT.Modify,updateFaultCode);

		_fcCauseLabel = new Label(_FCgroup,SWT.NONE);
		_fcCauseLabel.setText("Cause");
		_fcCauseText = new Text(_FCgroup,SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_fcCauseText.setLayoutData(gd);
		_fcCauseText.addListener(SWT.Modify,updateFaultCode);

		_fcActionLabel = new Label(_FCgroup,SWT.NONE);
		_fcActionLabel.setText("Action");
		_fcActionText = new Text(_FCgroup,SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_fcActionText.setLayoutData(gd);
		_fcActionText.addListener(SWT.Modify,updateFaultCode);

		_fcConsequenceLabel = new Label(_FCgroup,SWT.NONE);
		_fcConsequenceLabel.setText("Consequence");
		_fcConsequenceText = new Text(_FCgroup,SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_fcConsequenceText.setLayoutData(gd);
		_fcConsequenceText.addListener(SWT.Modify,updateFaultCode);

		_fcProblemLabel = new Label(_FCgroup,SWT.NONE);
		_fcProblemLabel.setText("Problem description");
		_fcProblemText = new Text(_FCgroup,SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_fcProblemText.setLayoutData(gd);
		_fcProblemText.addListener(SWT.Modify,updateFaultCode);
		
		_fcErrorMessageLabel = new Label(_FCgroup, SWT.NONE);
		_fcErrorMessageLabel.setText("");
		_fcErrorMessageLabel.setForeground(getViewSite().getShell().getDisplay().getSystemColor(SWT.COLOR_RED));
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		gd.horizontalSpan = 2;
		_fcErrorMessageLabel.setLayoutData(gd);
	}

	private void createFMWidgets() {
		
		Listener updateFaultMember = new Listener() {
			public void handleEvent(Event event) {
				TreeItem tmp = _tree.getSelection()[0];
				String tfm = tmp.getText();
				String tff = tmp.getParentItem().getParentItem().getText();
				FaultMember fmt = new FaultMember();
				//TODO: Error icon or something similar
				if(_fmNameText.getText().isEmpty()) {
					_fmErrorMessageLabel.setText("FaultMember Name Missing!");
					return;
				}
				if(_fmNameText.getText().contains(" ")) {
					_fmErrorMessageLabel.setText("Invalid FaultMember Name. No spaces allowed.");
					return;
				}
				fmt.setName(_fmNameText.getText());
				Location lt = new Location();
				if(!_fmLocBuildingText.getText().isEmpty()) 
					lt.setBuilding(_fmLocBuildingText.getText());
				if(!_fmLocFloorText.getText().isEmpty())
					lt.setFloor(_fmLocFloorText.getText());
				if(!_fmLocRoomText.getText().isEmpty())
					lt.setRoom(_fmLocRoomText.getText());
				if(!_fmLocMnemonicText.getText().isEmpty())
					lt.setMnemonic(_fmLocMnemonicText.getText());
				if(!_fmLocPositionText.getText().isEmpty())
					lt.setPosition(_fmLocPositionText.getText());
				fmt.setLocation(lt);
				_fmErrorMessageLabel.setText("");
				try{
					_alarmManager.updateFaultMember(_alarmManager.getFaultFamily(tff), _alarmManager.getFaultMember(tff, tfm),fmt);
					tmp.setText(_fmNameText.getText());
					if(tfm.compareTo(fmt.getName()) != 0) {
						sortFaultFamilyList();
						selectElementFromTree(tff, fmt.getName(), null);
					}
				}catch(IllegalOperationException e){
					_fmErrorMessageLabel.setText(e.getMessage());
				}catch(NullPointerException e){
					_fmErrorMessageLabel.setText(e.getMessage());
				}
			}
		};

		_FMgroup = new Group(_compInitial,SWT.SHADOW_ETCHED_IN);
		_FMgroup.setText("Fault Member detail");
		GridData gd = new GridData();
		gd.horizontalAlignment = SWT.FILL;
		gd.grabExcessHorizontalSpace = true;
		gd.verticalAlignment = SWT.FILL;
		gd.grabExcessVerticalSpace = true;
		_FMgroup.setLayoutData(gd);
		GridLayout gl = new GridLayout();
		gl.numColumns = 2;
		_FMgroup.setLayout(gl);

		_fmNameLabel = new Label(_FMgroup,SWT.NONE);
		_fmNameLabel.setText("Name");
		_fmNameText = new Text(_FMgroup,SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_fmNameText.setLayoutData(gd);
		_fmNameText.addListener(SWT.Modify, updateFaultMember);

		_fmLocGroup = new Group(_FMgroup, SWT.SHADOW_ETCHED_IN);
		_fmLocGroup.setText("Location");
		gl = new GridLayout();
		gl.numColumns = 2;
		gd = new GridData();
		gd.horizontalAlignment = SWT.FILL;
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalSpan = 2;
		_fmLocGroup.setLayout(gl);
		_fmLocGroup.setLayoutData(gd);

		_fmLocBuildingLabel = new Label(_fmLocGroup,SWT.NONE);
		_fmLocBuildingLabel.setText("Building");
		_fmLocBuildingText = new Text(_fmLocGroup,SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_fmLocBuildingText.setLayoutData(gd);
		_fmLocBuildingText.addListener(SWT.Modify, updateFaultMember);

		_fmLocFloorLabel = new Label(_fmLocGroup,SWT.NONE);
		_fmLocFloorLabel.setText("Floor");
		_fmLocFloorText = new Text(_fmLocGroup,SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_fmLocFloorText.setLayoutData(gd);
		_fmLocFloorText.addListener(SWT.Modify, updateFaultMember);

		_fmLocRoomLabel = new Label(_fmLocGroup,SWT.NONE);
		_fmLocRoomLabel.setText("Room");
		_fmLocRoomText = new Text(_fmLocGroup,SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_fmLocRoomText.setLayoutData(gd);
		_fmLocRoomText.addListener(SWT.Modify, updateFaultMember);

		_fmLocMnemonicLabel = new Label(_fmLocGroup,SWT.NONE);
		_fmLocMnemonicLabel.setText("Mnemonic");
		_fmLocMnemonicText = new Text(_fmLocGroup,SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_fmLocMnemonicText.setLayoutData(gd);
		_fmLocMnemonicText.addListener(SWT.Modify, updateFaultMember);

		_fmLocPositionLabel = new Label(_fmLocGroup,SWT.NONE);
		_fmLocPositionLabel.setText("Position");
		_fmLocPositionText = new Text(_fmLocGroup,SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_fmLocPositionText.setLayoutData(gd);
		_fmLocPositionText.addListener(SWT.Modify, updateFaultMember);
		
		_fmErrorMessageLabel = new Label(_FMgroup, SWT.NONE);
		_fmErrorMessageLabel.setText("");
		_fmErrorMessageLabel.setForeground(getViewSite().getShell().getDisplay().getSystemColor(SWT.COLOR_RED));
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		gd.horizontalSpan = 2;
		_fmErrorMessageLabel.setLayoutData(gd);
	}
	
	private void createFCFMWidgets() {
		_FCFMgroup = new Group(_compInitial,SWT.SHADOW_ETCHED_IN);
		GridData gd = new GridData();
		gd.horizontalAlignment = SWT.FILL;
		gd.grabExcessHorizontalSpace = true;
		_FCFMgroup.setLayoutData(gd);
		GridLayout gl = new GridLayout();
		gl.numColumns = 1;
		_FCFMgroup.setLayout(gl);

		_fcfmLabel = new Label(_FCFMgroup,SWT.NONE);
		_fcfmLabel.setLayoutData(gd);
		_fcfmLabel.setText("");
	}
	
	private void fillFCFMWidgets() {
		NodeType type = (NodeType) _tree.getSelection()[0].getData();
		if(type == NodeType.FAULT_CODE_LIST)
			_fcfmLabel.setText("List of Fault Codes");
		else if(type == NodeType.FAULT_MEMBER_LIST)
			_fcfmLabel.setText("List of Fault Members");
	}
	
	private void createFMDWidgets() {
		Listener updateFaultMemberDefault = new Listener() {
			public void handleEvent(Event event) {
				TreeItem tmp = _tree.getSelection()[0];
				String tff = tmp.getParentItem().getParentItem().getText();
				FaultMemberDefault fmdt = new FaultMemberDefault();
				Location lt = new Location();
				if(!_fmdLocBuildingText.getText().isEmpty()) 
					lt.setBuilding(_fmdLocBuildingText.getText());
				if(!_fmdLocFloorText.getText().isEmpty())
					lt.setFloor(_fmdLocFloorText.getText());
				if(!_fmdLocRoomText.getText().isEmpty())
					lt.setRoom(_fmdLocRoomText.getText());
				if(!_fmdLocMnemonicText.getText().isEmpty())
					lt.setMnemonic(_fmdLocMnemonicText.getText());
				if(!_fmdLocPositionText.getText().isEmpty())
					lt.setPosition(_fmdLocPositionText.getText());
				fmdt.setLocation(lt);
				_fmdErrorMessageLabel.setText("");
				try{
					_alarmManager.setFaultMemberDefault(_alarmManager.getFaultFamily(tff), fmdt);
					tmp.setText("Default Member");
					//sortFaultFamilyList();
					//selectElementFromTree(tff, "Default Member", null);
				}catch(IllegalOperationException e){
					_fmdErrorMessageLabel.setText(e.getMessage());
				}catch(NullPointerException e){
					_fmdErrorMessageLabel.setText(e.getMessage());
				}
			}
		};

		_FMDgroup = new Group(_compInitial,SWT.SHADOW_ETCHED_IN);
		_FMDgroup.setText("Default Fault Member details");
		GridData gd = new GridData();
		gd.horizontalAlignment = SWT.FILL;
		gd.grabExcessHorizontalSpace = true;
		gd.verticalAlignment = SWT.FILL;
		gd.grabExcessVerticalSpace = true;
		_FMDgroup.setLayoutData(gd);
		GridLayout gl = new GridLayout();
		gl.numColumns = 2;
		_FMDgroup.setLayout(gl);

		_fmdLocGroup = new Group(_FMDgroup, SWT.SHADOW_ETCHED_IN);
		_fmdLocGroup.setText("Location");
		gl = new GridLayout();
		gl.numColumns = 2;
		gd = new GridData();
		gd.horizontalAlignment = SWT.FILL;
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalSpan = 2;
		_fmdLocGroup.setLayout(gl);
		_fmdLocGroup.setLayoutData(gd);

		_fmdLocBuildingLabel = new Label(_fmdLocGroup,SWT.NONE);
		_fmdLocBuildingLabel.setText("Building");
		_fmdLocBuildingText = new Text(_fmdLocGroup,SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_fmdLocBuildingText.setLayoutData(gd);
		_fmdLocBuildingText.addListener(SWT.Modify, updateFaultMemberDefault);

		_fmdLocFloorLabel = new Label(_fmdLocGroup,SWT.NONE);
		_fmdLocFloorLabel.setText("Floor");
		_fmdLocFloorText = new Text(_fmdLocGroup,SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_fmdLocFloorText.setLayoutData(gd);
		_fmdLocFloorText.addListener(SWT.Modify, updateFaultMemberDefault);

		_fmdLocRoomLabel = new Label(_fmdLocGroup,SWT.NONE);
		_fmdLocRoomLabel.setText("Room");
		_fmdLocRoomText = new Text(_fmdLocGroup,SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_fmdLocRoomText.setLayoutData(gd);
		_fmdLocRoomText.addListener(SWT.Modify, updateFaultMemberDefault);

		_fmdLocMnemonicLabel = new Label(_fmdLocGroup,SWT.NONE);
		_fmdLocMnemonicLabel.setText("Mnemonic");
		_fmdLocMnemonicText = new Text(_fmdLocGroup,SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_fmdLocMnemonicText.setLayoutData(gd);
		_fmdLocMnemonicText.addListener(SWT.Modify, updateFaultMemberDefault);

		_fmdLocPositionLabel = new Label(_fmdLocGroup,SWT.NONE);
		_fmdLocPositionLabel.setText("Position");
		_fmdLocPositionText = new Text(_fmdLocGroup,SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_fmdLocPositionText.setLayoutData(gd);
		_fmdLocPositionText.addListener(SWT.Modify, updateFaultMemberDefault);
		
		_fmdErrorMessageLabel = new Label(_FMDgroup, SWT.NONE);
		_fmdErrorMessageLabel.setText("");
		_fmdErrorMessageLabel.setForeground(getViewSite().getShell().getDisplay().getSystemColor(SWT.COLOR_RED));
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		gd.horizontalSpan = 2;
		_fmdErrorMessageLabel.setLayoutData(gd);
	}

	private void createFFWidgets() {
		_updateFaultFamily = new Listener() {
			public void handleEvent(Event event) {
				TreeItem tmp = _tree.getSelection()[0];
				String ff = tmp.getText();
				FaultFamily fft = new FaultFamily();
				//TODO: Error icon or something similar
				if(_ffNameText.getText().isEmpty()) {
					_ffErrorMessageLabel.setText("FaultFamily Name Missing!");
					return;
				}
				if(_ffNameText.getText().contains(" ")) {
					_ffErrorMessageLabel.setText("Invalid FaultFamily Name. No spaces allowed.");
					return;
				}
				fft.setName(_ffNameText.getText());
				if(!_ffHelpURLText.getText().isEmpty()) {
					URI hurl;
					try {
						hurl = new URI(_ffHelpURLText.getText());
					} catch (MalformedURIException e1) {
						_ffErrorMessageLabel.setText("Malformed URL!");
						return;
					}
					fft.setHelpUrl(hurl.toString());
				}
				fft.setAlarmSource(_ffSourceCombo.getText());
				Contact ct = new Contact();
				if(_ffContactNameText.getText().isEmpty()) {
					_ffErrorMessageLabel.setText("Contact Name Missing!");
					return;
				}
				ct.setName(_ffContactNameText.getText());
				if(!_ffContactMailText.getText().isEmpty())
					ct.setEmail(_ffContactMailText.getText());
				if(!_ffContactGSMText.getText().isEmpty())
					ct.setGsm(_ffContactGSMText.getText());
				fft.setContact(ct);
				
				_ffErrorMessageLabel.setText("");
				try{
					_alarmManager.updateFaultFamily(_alarmManager.getFaultFamily(ff), fft);
					tmp.setText(_ffNameText.getText());
					IWorkbenchWindow _window = getViewSite().getWorkbenchWindow();
					IViewReference[] views = _window.getActivePage().getViewReferences();
					IMyViewPart view = ((IMyViewPart)views[2].getView(false));
					view.fillWidgets();
					if(ff.compareTo(fft.getName()) != 0) {
						sortFaultFamilyList();
						selectElementFromTree(fft.getName(), null, null);
					}
				}catch(IllegalOperationException e){
					_ffErrorMessageLabel.setText(e.getMessage());
				}catch(NullPointerException e){
					e.printStackTrace();
					_ffErrorMessageLabel.setText(e.getMessage());
				}
			}
		};
		
		_addCategory = new Listener() {
			public void handleEvent(Event event) {
				if(event.type == SWT.KeyUp)
					if(!(event.keyCode == SWT.CR || event.keyCode == ' '))
						return;
				
				if(event.type == SWT.MouseDoubleClick){
					Point pt = new Point(event.x,event.y);
					if(_ffCategoryList.getItem(pt) == null)
						return;
				}
					
				TreeItem[] tmp1 = _tree.getSelection();
				if(tmp1 == null || tmp1.length == 0)
					return;
				String ff = tmp1[0].getText();
				TableItem[] tmp2 = _ffCategoryList.getSelection();
				if(tmp2 == null || tmp2.length == 0)
					return;
				TableItem item = tmp2[0];
				Category c = _categoryManager.getCategoryByPath(item.getText());
				try{
					String[] ffs = c.getAlarms().getFaultFamily();
					for (int i = 0; i < ffs.length; i++) {
						if(ff.compareTo(ffs[i]) == 0){
							c.getAlarms().removeFaultFamily(ff);
							item.setImage((org.eclipse.swt.graphics.Image)null);
							IWorkbenchWindow _window = getViewSite().getWorkbenchWindow();
							IViewReference[] views = _window.getActivePage().getViewReferences();
							IMyViewPart view = ((IMyViewPart)views[2].getView(false));
							view.fillWidgets();
							return;
						}
					}
					c.getAlarms().addFaultFamily(ff);
					item.setImage(Activator.getDefault().getImageRegistry().get(Activator.IMG_TICKET));
				}catch(NullPointerException e){
					item.setImage((org.eclipse.swt.graphics.Image)null);
					Alarms alarms = new Alarms();
					alarms.addFaultFamily(ff.toString());
			  		alarms.setFaultFamily(0, ff.toString());	
			  		c.setAlarms(alarms);
					item.setImage(Activator.getDefault().getImageRegistry().get(Activator.IMG_TICKET));
				}
				IWorkbenchWindow _window = getViewSite().getWorkbenchWindow();
				IViewReference[] views = _window.getActivePage().getViewReferences();
				IMyViewPart view = ((IMyViewPart)views[2].getView(false));
				view.fillWidgets();
			}
		};
		
		_FFgroup = new Group(_compInitial,SWT.SHADOW_ETCHED_IN);
		_FFgroup.setText("Fault Family details");
		GridData gd = new GridData();
		gd.horizontalAlignment = SWT.FILL;
		gd.grabExcessHorizontalSpace = true;
		gd.verticalAlignment = SWT.FILL;
		gd.grabExcessVerticalSpace = true;
		_FFgroup.setLayoutData(gd);
		GridLayout gl = new GridLayout();
		gl.numColumns = 2;
		_FFgroup.setLayout(gl);

		_ffNameLabel = new Label(_FFgroup,SWT.NONE);
		_ffNameLabel.setText("Fault Family name");
		_ffNameText = new Text(_FFgroup,SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_ffNameText.setLayoutData(gd);
		_ffNameText.addListener(SWT.Modify, _updateFaultFamily);

		_ffHelpURLLabel = new Label(_FFgroup,SWT.NONE);
		_ffHelpURLLabel.setText("Help URL");
		_ffHelpURLText = new Text(_FFgroup, SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_ffHelpURLText.setLayoutData(gd);
		_ffHelpURLText.addListener(SWT.Modify, _updateFaultFamily);

		_ffContactNameLabel = new Label(_FFgroup, SWT.NONE);
		_ffContactNameLabel.setText("Contact name");
		_ffContactNameText = new Text(_FFgroup, SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_ffContactNameText.setLayoutData(gd);
		_ffContactNameText.addListener(SWT.Modify, _updateFaultFamily);

		_ffContactMailLabel = new Label(_FFgroup, SWT.NONE);
		_ffContactMailLabel.setText("Contact e-mail");
		_ffContactMailText = new Text(_FFgroup, SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_ffContactMailText.setLayoutData(gd);
		_ffContactMailText.addListener(SWT.Modify, _updateFaultFamily);

		_ffContactGSMLabel = new Label(_FFgroup, SWT.NONE);
		_ffContactGSMLabel.setText("Contact GSM");
		_ffContactGSMText = new Text(_FFgroup, SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_ffContactGSMText.setLayoutData(gd);
		_ffContactGSMText.addListener(SWT.Modify, _updateFaultFamily);
		
		_ffSourceLabel = new Label(_FFgroup, SWT.NONE);
		_ffSourceLabel.setText("Source");
		_ffSourceCombo = new Combo(_FFgroup, SWT.DROP_DOWN | SWT.READ_ONLY);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_ffSourceCombo.setLayoutData(gd);
		_ffSourceCombo.setEnabled(false);
		_ffSourceCombo.addListener(SWT.Modify, _updateFaultFamily);
		
		_ffCategoryLabel = new Label(_FFgroup, SWT.NONE);
		_ffCategoryLabel.setText("Categories:");
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		gd.horizontalSpan = 2;
		_ffCategoryLabel.setLayoutData(gd);
		
		_ffCategoryList = new Table(_FFgroup,SWT.BORDER);
		gd = new GridData();
		gd.verticalAlignment = SWT.FILL;
		gd.horizontalAlignment = SWT.FILL;
		gd.grabExcessVerticalSpace = true;
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalSpan = 2;
		_ffCategoryList.setLayoutData(gd);
		_ffCategoryList.addListener(SWT.KeyUp, _addCategory);
		_ffCategoryList.addListener(SWT.MouseDoubleClick, _addCategory);
		Menu categoryPopUp = new Menu(_ffCategoryList);
		_ffCategoryList.setMenu(categoryPopUp);
		categoryPopUp.addListener(SWT.Show, new Listener() {
			public void handleEvent(Event e) {
				TableItem[] sel = _ffCategoryList.getSelection();
				Menu categoryPopUp = _ffCategoryList.getMenu();
				MenuItem[] items = categoryPopUp.getItems();
				for (int i = 0; i < items.length; i++)
					items[i].dispose();
				if(sel == null || sel.length == 0)
					return;
				MenuItem mitem;
				mitem = new MenuItem(categoryPopUp,SWT.PUSH);
				if(sel[0].getImage() == null) {
					mitem.setText("Add to Category");
					mitem.addListener(SWT.Selection, _addCategory);
				} else {
					mitem.setText("Remove from Category");
					mitem.addListener(SWT.Selection, _addCategory);
				}
			}
		}
		);
		
		_ffErrorMessageLabel = new Label(_FFgroup, SWT.NONE);
		_ffErrorMessageLabel.setText("");
		_ffErrorMessageLabel.setForeground(getViewSite().getShell().getDisplay().getSystemColor(SWT.COLOR_RED));
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		gd.horizontalSpan = 2;
		_ffErrorMessageLabel.setLayoutData(gd);
	}

	private void fillFFWidgets(String name) {

		//_ffNameText.removeListener(SWT.Modify, _updateFaultFamily);
		//_ffHelpURLText.removeListener(SWT.Modify, _updateFaultFamily);
		//_ffContactNameText.removeListener(SWT.Modify, _updateFaultFamily);
		//_ffContactMailText.removeListener(SWT.Modify, _updateFaultFamily);
		//_ffContactGSMText.removeListener(SWT.Modify, _updateFaultFamily);
		//_ffSourceCombo.removeListener(SWT.Modify, _updateFaultFamily);
		//_ffCategoryCombo.removeListener(SWT.Modify, updateFaultFamily);
		
		FaultFamily ff = _alarmManager.getFaultFamily(name);

		// This should never happen anyways...
		if( ff == null )
			return;

		String helpUrl = "";
		String contactName = "";
		String contactEmail = "";
		String contactGsm = "";
		String source = "";

		if( ff.getHelpUrl() != null )
			helpUrl = ff.getHelpUrl().trim();

		if( ff.getContact() != null ) {
			if( ff.getContact().getName() != null )
				contactName = ff.getContact().getName().trim();
			if( ff.getContact().getEmail() != null )
				contactEmail = ff.getContact().getEmail().trim();
			if( ff.getContact().getGsm() != null )
				contactGsm = ff.getContact().getGsm().trim();
		}
		
		_ffSourceCombo.removeAll();
			
		if(ff.getAlarmSource() != null)
			source = ff.getAlarmSource();
		
		sortCategoryList(name);
		
		Source[] _sourceList = _sourceManager.getAllSources();
		for(int i = 0; i < _sourceList.length ; i++){
			_ffSourceCombo.add(_sourceList[i].getName());
		}
		if(_ffSourceCombo.getItemCount() == 0)
			_ffSourceCombo.add("ALARM_SYSTEM_SOURCES");

		_ffNameText.setText(name);
		_ffHelpURLText.setText(helpUrl);
		_ffContactNameText.setText(contactName);
		_ffContactMailText.setText(contactEmail);
		_ffContactGSMText.setText(contactGsm);
		for(int i = 0; i < _ffSourceCombo.getItemCount(); i++){
			if(_ffSourceCombo.getItem(i).compareTo(source) == 0)
				_ffSourceCombo.select(i);
		}
		
		//_ffNameText.addListener(SWT.Modify, _updateFaultFamily);
		//_ffHelpURLText.addListener(SWT.Modify, _updateFaultFamily);
		//_ffContactNameText.addListener(SWT.Modify, _updateFaultFamily);
		//_ffContactMailText.addListener(SWT.Modify, _updateFaultFamily);
		//_ffContactGSMText.addListener(SWT.Modify, _updateFaultFamily);
		//_ffSourceCombo.addListener(SWT.Modify, _updateFaultFamily);
		//_ffCategoryCombo.addListener(SWT.Modify, _updateFaultFamily);
	}

	private void fillFCWidgets(int value, String ffName) {

		FaultCode fc = _alarmManager.getFaultCode(ffName, value);

		// This should never happen anyways...
		if( fc == null )
			return;

		String val = "";
		String priority = "";
		String cause= "";
		String action= "";
		String consequence= "";
		String problem = "";

		val = Integer.toString(fc.getValue());
		if(fc.hasPriority())
			priority = Integer.toString(fc.getPriority());
		if(fc.getCause() != null)
			cause = fc.getCause().trim();
		if(fc.getAction() != null)
			action = fc.getAction().trim();
		if(fc.getConsequence() != null)
			consequence = fc.getConsequence().trim();
		if(fc.getProblemDescription() != null)
			problem = fc.getProblemDescription().trim();

		_fcValueText.setText(val);
		_fcPriorityText.setText(priority);
		_fcCauseText.setText(cause);
		_fcActionText.setText(action);
		_fcConsequenceText.setText(consequence);
		_fcProblemText.setText(problem);

	}

	private void fillFMWidgets(String fmName, String ffName) {
		FaultMember fm = _alarmManager.getFaultMember(ffName, fmName);

		// This should never happen anyways...
		if( fm == null )
			return;

		String name = "";
		String building = "";
		String floor = "";
		String room = "";
		String mnemonic = "";
		String position = "";

		name = fm.getName();
		if(fm.getLocation() != null){
			if(fm.getLocation().getBuilding() != null)
				building = fm.getLocation().getBuilding().trim();
			if(fm.getLocation().getFloor() != null)
				floor = fm.getLocation().getFloor().trim();
			if(fm.getLocation().getRoom() != null)
				room = fm.getLocation().getRoom().trim();
			if(fm.getLocation().getMnemonic() != null)
				mnemonic = fm.getLocation().getMnemonic().trim();
			if(fm.getLocation().getPosition() != null)
				position = fm.getLocation().getPosition().trim();
		}


		_fmNameText.setText(name);
		_fmLocBuildingText.setText(building);
		_fmLocFloorText.setText(floor);
		_fmLocRoomText.setText(room);
		_fmLocMnemonicText.setText(mnemonic);
		_fmLocPositionText.setText(position);
	}
	
	private void fillFMDWidgets(String ffName) {
		FaultMemberDefault fmd = _alarmManager.getFaultMemberDefault(ffName);

		// This should never happen anyways...
		if( fmd == null )
			return;

		String building = "";
		String floor = "";
		String room = "";
		String mnemonic = "";
		String position = "";

		if(fmd.getLocation() != null){
			if(fmd.getLocation().getBuilding() != null)
				building = fmd.getLocation().getBuilding().trim();
			if(fmd.getLocation().getFloor() != null)
				floor = fmd.getLocation().getFloor().trim();
			if(fmd.getLocation().getRoom() != null)
				room = fmd.getLocation().getRoom().trim();
			if(fmd.getLocation().getMnemonic() != null)
				mnemonic = fmd.getLocation().getMnemonic().trim();
			if(fmd.getLocation().getPosition() != null)
				position = fmd.getLocation().getPosition().trim();
		}

		_fmdLocBuildingText.setText(building);
		_fmdLocFloorText.setText(floor);
		_fmdLocRoomText.setText(room);
		_fmdLocMnemonicText.setText(mnemonic);
		_fmdLocPositionText.setText(position);
	}

	@Override
	public void setFocus() {
	}

	/* (non-Javadoc)
	 * @see cl.utfsm.acs.acg.gui.IMyViewPart#setEnabled(boolean)
	 */
	public void setEnabled(boolean v) {
		/* Left side widgets */
		_tree.setEnabled(v);
		_addAlarmButton.setEnabled(v);
		_deleteAlarmButton.setEnabled(v);
		
		/* FF information */
		_ffNameText.setEnabled(v);
		_ffHelpURLText.setEnabled(v);
		_ffContactNameText.setEnabled(v);
		_ffContactMailText.setEnabled(v);
		_ffContactGSMText.setEnabled(v);
		//_ffSourceCombo.setEnabled(v);
		_ffCategoryList.setEnabled(v);

		/* FC information */
		_fcValueText.setEnabled(v);
		_fcPriorityText.setEnabled(v);
		_fcCauseText.setEnabled(v);
		_fcActionText.setEnabled(v);
		_fcConsequenceText.setEnabled(v);
		_fcProblemText.setEnabled(v);
		
		/* FM information */
		_fmNameText.setEnabled(v);
		_fmLocBuildingText.setEnabled(v);
		_fmLocRoomText.setEnabled(v);
		_fmLocFloorText.setEnabled(v);
		_fmLocMnemonicText.setEnabled(v);
		_fmLocPositionText.setEnabled(v);
		
		/* FMD Information */
		_fmdLocBuildingText.setEnabled(v);
		_fmdLocRoomText.setEnabled(v);
		_fmdLocFloorText.setEnabled(v);
		_fmdLocMnemonicText.setEnabled(v);
		_fmdLocPositionText.setEnabled(v);
	}
	@Override
	public void fillWidgets() {
		// TODO Auto-generated method stub
		if(_tree.getSelection() == null || _tree.getSelection().length == 0)
			return;
		TreeItem ti = _tree.getSelection()[0];
		NodeType type = (NodeType) ti.getData();
		String ffName;
		switch(type) {
		case FAULT_FAMILY:
			fillFFWidgets(ti.getText());
			break;
		case FAULT_CODE_LIST:
			fillFCFMWidgets();
			break;
		case FAULT_CODE_DATA:
			ffName = ti.getParentItem().getParentItem().getText();
			fillFCWidgets(Integer.parseInt(ti.getText()), ffName);
			break;
		case FAULT_MEMBER_LIST:
			fillFCFMWidgets();
			break;
		case FAULT_MEMBER_DATA:
			ffName = ti.getParentItem().getParentItem().getText();
			fillFMWidgets(ti.getText(), ffName);
			break;
		case FAULT_MEMBER_DEFAULT:
			ffName = ti.getParentItem().getParentItem().getText();
			fillFMDWidgets(ffName);
			break;
		default:
			break;
		}
	}
	
	public void sortCategoryList(String name) {
		_ffCategoryList.removeAll();
		List<Category> catList = _categoryManager.getAllCategories();
		List<String> sortedCatList = new ArrayList<String>();
		for(Category cat: catList)
			sortedCatList.add(cat.getPath().toLowerCase());
		Collections.sort(sortedCatList);
		
		for (String sc: sortedCatList) {
			Category cat = null;
			for(Category c: catList)
				if(c.getPath().toLowerCase().compareTo(sc) == 0)
					cat = c;
			if(cat == null)
				return;
			if(cat.getAlarms() == null) {
				TableItem  t = new TableItem(_ffCategoryList, SWT.None);
				t.setText(cat.getPath());
				if(cat.getIsDefault()){
					FontData fd = t.getFont().getFontData()[0];
					fd.setStyle(SWT.BOLD);
					t.setFont(new Font(_shell.getDisplay(),fd));
				}
			} else {
				String[] ffs = cat.getAlarms().getFaultFamily();
				TableItem  t = new TableItem(_ffCategoryList, SWT.None);
				t.setText(cat.getPath());
				if(cat.getIsDefault()){
					FontData fd = t.getFont().getFontData()[0];
					fd.setStyle(SWT.BOLD);
					t.setFont(new Font(_shell.getDisplay(),fd));
				}
				for (int i = 0; i < ffs.length; i++) {
					if(ffs[i].compareTo(name) == 0)
						t.setImage(Activator.getDefault().getImageRegistry().get(Activator.IMG_TICKET));
				}
			}
		}
	}
	
	public void sortFaultFamilyList() {
		List<FaultFamily> ffList = _alarmManager.getAllAlarms();
		List<FaultFamily> sortedFFList = new ArrayList<FaultFamily>();

		/* We get a separate tmp list with the names,
		 * sort it, and then sort the original ffList */
		List<String> tmp = new ArrayList<String>();
		for(FaultFamily ff: ffList)
			tmp.add(ff.getName().toLowerCase());
		Collections.sort(tmp);
		for(String sff: tmp)
			for(FaultFamily ff: ffList)
				if(ff.getName().toLowerCase().compareTo(sff) == 0)
					sortedFFList.add(ff);
		ffList = sortedFFList;
		
		_tree.removeAll();
		for (FaultFamily family : ffList) {
			TreeItem iTree = new TreeItem(_tree,SWT.NONE);
			iTree.setData(NodeType.FAULT_FAMILY);
			iTree.setText(family.getName());
			iTree.setImage(Activator.getDefault().getImageRegistry().get(Activator.IMG_ALARM));
			for(int j=0; j!=2; j++) {
				TreeItem jTree = new TreeItem(iTree,SWT.NONE);
				jTree.setText((j==0 ? "Fault Codes" : "Fault Members"));
				jTree.setData((j==0 ? NodeType.FAULT_CODE_LIST : NodeType.FAULT_MEMBER_LIST));
				jTree.setImage((j==0?
						Activator.getDefault().getImageRegistry().get(Activator.IMG_FAULTCODES) :
						Activator.getDefault().getImageRegistry().get(Activator.IMG_FAULTMEMBERS) ));
				if( j == 0 )
					sortFaultCodeList(family, jTree);
				else
					sortFaultMemberList(family, jTree);
			}
		}
	}
	
	public void sortFaultMemberList(FaultFamily family, TreeItem jTree) {
		TreeItem iTree = jTree.getParentItem();
		FaultMember[] faultMembers = family.getFaultMember();
		List<FaultMember> fmList = new ArrayList<FaultMember>();
		for(FaultMember fm: faultMembers)
			fmList.add(fm);
		List<FaultMember> sortedFMList = new ArrayList<FaultMember>();

		/* We get a separate tmp list with the names,
		 * sort it, and then sort the original ffList */
		List<String> tmp = new ArrayList<String>();
		for(FaultMember fm: fmList)
			tmp.add(fm.getName().toLowerCase());
		Collections.sort(tmp);
		for(String sfm: tmp)
			for(FaultMember fm: fmList)
				if(fm.getName().toLowerCase().compareTo(sfm) == 0)
					sortedFMList.add(fm);
		fmList = sortedFMList;
		
		FaultMemberDefault fmd = family.getFaultMemberDefault();
		if( fmd != null ) {
			TreeItem kTree = new TreeItem(jTree,SWT.NONE);
			kTree.setText("Default Member");
			//kTree.setForeground(new Color(kTree.getDisplay(), 255, 0, 0));
			kTree.setData(NodeType.FAULT_MEMBER_DEFAULT);
		}
		for (FaultMember fm: fmList) {
			TreeItem kTree = new TreeItem(jTree,SWT.NONE);
			kTree.setText(fm.getName());
			kTree.setData(NodeType.FAULT_MEMBER_DATA);
		}
		if(fmList.isEmpty() && fmd == null) {
			iTree.setForeground(new Color(iTree.getDisplay(), 255, 0, 0));
			jTree.setForeground(new Color(jTree.getDisplay(), 255, 0, 0));
		}		
	}
	
	public void sortFaultCodeList(FaultFamily family, TreeItem jTree) {
		TreeItem iTree = jTree.getParentItem();
		FaultCode[] faultCodes = family.getFaultCode();
		List<FaultCode> fcList = new ArrayList<FaultCode>();
		for(FaultCode fc: faultCodes)
			fcList.add(fc);
		List<FaultCode> sortedFCList = new ArrayList<FaultCode>();

		/* We get a separate tmp list with the names,
		 * sort it, and then sort the original ffList */
		List<Integer> tmp = new ArrayList<Integer>();
		for(FaultCode fc: fcList)
			tmp.add(new Integer(fc.getValue()));
		Collections.sort(tmp);
		for(Integer ifc: tmp)
			for(FaultCode fc: fcList)
				if(ifc.intValue() == fc.getValue())
					sortedFCList.add(fc);
		fcList = sortedFCList;
		for (FaultCode fc: fcList) {
			TreeItem kTree = new TreeItem(jTree, SWT.NONE);
			kTree.setText(Integer.toString(fc.getValue()));
			kTree.setData(NodeType.FAULT_CODE_DATA);
		}
		if(fcList.isEmpty()) {
			iTree.setForeground(new Color(iTree.getDisplay(), 255, 0, 0));
			jTree.setForeground(new Color(jTree.getDisplay(), 255, 0, 0));
		}
	}
	
	public void selectElementFromTree(String ff, String fm, String fc){
		if(ff == null)
			return;
		if(fm != null && fc != null)
			return;
		TreeItem[] its = _tree.getItems();
		TreeItem sel = null;
		for(TreeItem it: its)
			if(it.getText().compareTo(ff) == 0)
				sel = it;
		if(fm != null || fc != null) {
			String search = null;
			if(fm != null) {
				its = sel.getItems()[1].getItems();
				search = fm;
			} else {
				its = sel.getItems()[0].getItems();
				search = fc;
			}
			for(TreeItem it: its)
				if(it.getText().compareTo(search) == 0)
					sel = it;
		}
		_tree.setSelection(sel);
		Event e = new Event();
		_tree.notifyListeners(SWT.Selection, e);
	}
	
	public void setReadOnly(boolean v){
		if(v) {
			_tree.setMenu(null);
			_ffCategoryList.setMenu(null);
			_ffCategoryList.removeListener(SWT.KeyUp, _addCategory);
			_ffCategoryList.removeListener(SWT.MouseDoubleClick, _addCategory);
		} else {
			_tree.setMenu(null);
			_ffCategoryList.setMenu(null);
			_ffCategoryList.addListener(SWT.KeyUp, _addCategory);
			_ffCategoryList.addListener(SWT.MouseDoubleClick, _addCategory);
		}
		_addAlarmButton.setEnabled(!v);
		_deleteAlarmButton.setEnabled(!v);
		_ffNameText.setEnabled(!v);
		_ffHelpURLText.setEnabled(!v);
		_ffContactNameText.setEnabled(!v);
		_ffContactMailText.setEnabled(!v);
		_ffContactGSMText.setEnabled(!v);
		_ffSourceCombo.setEnabled(!v);
		_fcValueText.setEnabled(!v);
		_fcPriorityText.setEnabled(!v);
		_fcCauseText.setEnabled(!v);
		_fcActionText.setEnabled(!v);
		_fcConsequenceText.setEnabled(!v);
		_fcProblemText.setEnabled(!v);
		_fmNameText.setEnabled(!v);
		_fmLocBuildingText.setEnabled(!v);
		_fmLocRoomText.setEnabled(!v);
		_fmLocFloorText.setEnabled(!v);
		_fmLocMnemonicText.setEnabled(!v);
		_fmLocPositionText.setEnabled(!v);
		_fmdLocBuildingText.setEnabled(!v);
		_fmdLocRoomText.setEnabled(!v);
		_fmdLocFloorText.setEnabled(!v);
		_fmdLocMnemonicText.setEnabled(!v);
		_fmdLocPositionText.setEnabled(!v);
	}
}
