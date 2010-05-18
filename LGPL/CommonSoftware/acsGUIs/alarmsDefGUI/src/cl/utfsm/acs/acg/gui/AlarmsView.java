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
import java.util.Iterator;
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
	
	/* Listeners*/
	Listener updateFaultFamily;
	
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
		List<FaultFamily> ffList = _alarmManager.getAllAlarms();
		List<FaultFamily> sortedFFList = new ArrayList<FaultFamily>();

		/* We get a separate tmp list with the names,
		 * sort it, and then sort the original ffList */
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
				
				/* Populate with the Fault codes */
				if( j == 0 ) {
					FaultCode[] faultCodes = family.getFaultCode();
					for (int i = 0; i < faultCodes.length; i++) {
						TreeItem kTree = new TreeItem(jTree, SWT.NONE);
						kTree.setText(Integer.toString(faultCodes[i].getValue()));
						kTree.setData(NodeType.FAULT_CODE_DATA);
					}
				}
				/* Populate with the Fault members */
				else {
					FaultMember[] faultMembers = family.getFaultMember();
					FaultMemberDefault fmd = family.getFaultMemberDefault();
					if( fmd != null ) {
						TreeItem kTree = new TreeItem(jTree,SWT.NONE);
						kTree.setText("Default Member");
						kTree.setForeground(new Color(kTree.getDisplay(), 255, 0, 0));
						kTree.setData(NodeType.FAULT_MEMBER_DEFAULT);
					}
					for (int i = 0; i < faultMembers.length; i++) {
						TreeItem kTree = new TreeItem(jTree,SWT.NONE);
						kTree.setText(faultMembers[i].getName());
						kTree.setData(NodeType.FAULT_MEMBER_DATA);
						
					}
				}
			}
		}
		_tree.deselectAll();
		_FFgroup.setVisible(false);
		((GridData)_FFgroup.getLayoutData()).exclude = true;
		_FMgroup.setVisible(false);
		((GridData)_FMgroup.getLayoutData()).exclude = true;
		_FCgroup.setVisible(false);
		((GridData)_FCgroup.getLayoutData()).exclude = true;
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
		Listener deleteElement = new Listener() {
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
				if(type == NodeType.FAULT_CODE_LIST || type == NodeType.FAULT_MEMBER_LIST || type == NodeType.FAULT_MEMBER_DEFAULT)
					return;
				String alarm = _tree.getSelection()[0].getText();
				try {
					if(type == NodeType.FAULT_FAMILY)
						_alarmManager.deleteFaultFamily(_alarmManager.getFaultFamily(alarm));
					else if(type == NodeType.FAULT_CODE_DATA){
						String ff = _tree.getSelection()[0].getParentItem().getParentItem().getText();
						_alarmManager.deleteFaultCode(_alarmManager.getFaultFamily(ff),_alarmManager.getFaultCode(ff,new Integer(alarm).intValue()));
					}
					else if(type == NodeType.FAULT_MEMBER_DATA){
						String ff = _tree.getSelection()[0].getParentItem().getParentItem().getText();
						_alarmManager.deleteFaultMember(_alarmManager.getFaultFamily(ff),_alarmManager.getFaultMember(ff,alarm));
					}
				} catch (IllegalOperationException e){
					ErrorDialog error = new ErrorDialog(AlarmsView.this.getViewSite().getShell(),
							"Cannot delete the item",
							e.getMessage(),
							new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.toString()),
							IStatus.ERROR);
					error.setBlockOnOpen(true);
					error.open();
				} catch (NullPointerException e){
					ErrorDialog error = new ErrorDialog(AlarmsView.this.getViewSite().getShell(),
							"Cannot delete the item",
							e.getMessage(),
							new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.toString()),
							IStatus.ERROR);
					error.setBlockOnOpen(true);
					error.open();
				}
				if(type != NodeType.FAULT_FAMILY){
					sel = _tree.getSelection()[0].getParentItem();
					_tree.getSelection()[0].dispose();
					_tree.setSelection(sel);
					Event e = new Event();
					_tree.notifyListeners(SWT.Selection, e);
				}
				else{
					_tree.getSelection()[0].dispose();
					//_tree.setSelection(_tree.getItems()[0]);
					Event e = new Event();
					_tree.notifyListeners(SWT.Selection, e);
				}
			}
		}
			
		};
		
		Listener addFaultMember = new Listener() {
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
								new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.toString()),
								IStatus.ERROR);
						error.setBlockOnOpen(true);
						error.open();
						return;
					} catch (NullPointerException e) {
						ErrorDialog error = new ErrorDialog(AlarmsView.this.getViewSite().getShell(),
								"Cannot add the new Fault Member",
								e.getMessage(),
								new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.toString()),
								IStatus.ERROR);
						error.setBlockOnOpen(true);
						error.open();
						return;
					}
					TreeItem mTree = new TreeItem(tmp2.getItems()[1], SWT.NONE);
					mTree.setText(dialog.getValue());
					mTree.setData(NodeType.FAULT_MEMBER_DATA);
					_tree.setSelection(mTree);
					Event e = new Event();
					_tree.notifyListeners(SWT.Selection, e);
				}
			}
		};
		
		Listener addFaultCode = new Listener() {
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
								new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.toString()),
								IStatus.ERROR);
						error.setBlockOnOpen(true);
						error.open();
						return;
					} catch (NullPointerException e) {
						ErrorDialog error = new ErrorDialog(AlarmsView.this.getViewSite().getShell(),
								"Cannot add the new Fault Code",
								e.getMessage(),
								new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.toString()),
								IStatus.ERROR);
						error.setBlockOnOpen(true);
						error.open();
						return;
					}
					TreeItem cTree = new TreeItem(tmp2.getItems()[0], SWT.NONE);
					cTree.setText(dialog.getValue());
					cTree.setData(NodeType.FAULT_CODE_DATA);
					_tree.setSelection(cTree);
					Event e = new Event();
					_tree.notifyListeners(SWT.Selection, e);
				}
			}
		};
		
		_sash = new SashForm(parent,SWT.NONE);
		_sash.setLayout(new FillLayout());

		_alarmsComp = new Composite(_sash,SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.numColumns = 1;
		_alarmsComp.setLayout(layout);
		
		/* The tree used to list the FF, FM and FCs */
		_tree = new Tree(_alarmsComp,SWT.VIRTUAL | SWT.BORDER);
		GridData gd = new GridData();
		gd.verticalAlignment = SWT.FILL;
		gd.horizontalAlignment = SWT.FILL;
		gd.grabExcessVerticalSpace = true;
		gd.grabExcessHorizontalSpace = true;
		_tree.setLayoutData(gd);
		//Menu treePopUp = new Menu(parent, SWT.POP_UP);
		Menu treePopUp = new Menu(parent);
		MenuItem treePopUpDelete = new MenuItem(treePopUp,SWT.PUSH);
		treePopUpDelete.setText("Delete");
		treePopUpDelete.addListener(SWT.Selection, deleteElement);
		MenuItem treePopUpAddFaultMember = new MenuItem(treePopUp,SWT.PUSH);
		treePopUpAddFaultMember.setText("Add Fault Member");
		treePopUpAddFaultMember.addListener(SWT.Selection, addFaultMember);
		MenuItem treePopUpAddFaultCode = new MenuItem(treePopUp,SWT.PUSH);
		treePopUpAddFaultCode.setText("Add Fault Code");
		treePopUpAddFaultCode.addListener(SWT.Selection, addFaultCode);
		_tree.setMenu(treePopUp);
		_tree.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}

			public void widgetSelected(SelectionEvent e) {
				TreeItem []tmp = ((Tree)e.widget).getSelection();
				if( tmp == null || tmp.length == 0){
					//_FFgroup.setVisible(false);
					((GridData)_FFgroup.getLayoutData()).exclude = true;
					//_FMgroup.setVisible(false);
					((GridData)_FMgroup.getLayoutData()).exclude = true;
					//_FCgroup.setVisible(false);
					((GridData)_FCgroup.getLayoutData()).exclude = true;
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
				}
				else if( type == NodeType.FAULT_CODE_DATA ) {
					_FFgroup.setVisible(false);
					((GridData)_FFgroup.getLayoutData()).exclude = true;
					_FMgroup.setVisible(false);
					((GridData)_FMgroup.getLayoutData()).exclude = true;
					_FCgroup.setVisible(true);
					((GridData)_FCgroup.getLayoutData()).exclude = false;

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
				}
				else if( type == NodeType.FAULT_MEMBER_DATA ) {
					_FFgroup.setVisible(false);
					((GridData)_FFgroup.getLayoutData()).exclude = true;
					_FMgroup.setVisible(true);
					((GridData)_FMgroup.getLayoutData()).exclude = false;
					_FCgroup.setVisible(false);
					((GridData)_FCgroup.getLayoutData()).exclude = true;

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
				
		_addAlarmButton.addListener(SWT.Selection, new Listener() {

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
											new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.toString()),
											IStatus.ERROR);
						error.setBlockOnOpen(true);
						error.open();
					} catch (NullPointerException e) {
						ErrorDialog error = new ErrorDialog(AlarmsView.this.getViewSite().getShell(),
											"Cannot add the new Alarm",
											e.getMessage(),
											new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.toString()),
											IStatus.ERROR);
						error.setBlockOnOpen(true);
						error.open();
					}
					TreeItem iTree = new TreeItem(_tree,SWT.NONE);
					iTree.setData(NodeType.FAULT_FAMILY);
					iTree.setText(dialog.getValue());
					iTree.setImage(Activator.getDefault().getImageRegistry().get(Activator.IMG_ALARM));
					for(int j=0; j!=2; j++) {
						TreeItem jTree = new TreeItem(iTree,SWT.NONE);
						jTree.setText((j==0 ? "Fault Codes" : "Fault Members"));
						jTree.setData((j==0 ? NodeType.FAULT_CODE_LIST : NodeType.FAULT_MEMBER_LIST));
						jTree.setImage((j==0?
								Activator.getDefault().getImageRegistry().get(Activator.IMG_FAULTCODES) :
								Activator.getDefault().getImageRegistry().get(Activator.IMG_FAULTMEMBERS) ));
					}
					_tree.setSelection(iTree);
					Event e = new Event();
					_tree.notifyListeners(SWT.Selection, e);
				}
			}
		});

		_deleteAlarmButton.addListener(SWT.Selection, deleteElement);
		
		/* Top widget of the right side */
		_compInitial = new Composite(_sash, SWT.SHADOW_ETCHED_IN);
		_compInitial.setLayout(new GridLayout());

		new Label(_compInitial,SWT.NONE).setText("Select an element");
		
		/* FF/FM/FC Details */
		createFFWidgets();
		createFCWidgets();
		createFMWidgets();

		/* At the beginning we only show a label */
		_FFgroup.setVisible(false);
		_FCgroup.setVisible(false);
		_FMgroup.setVisible(false);

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

	private void createFFWidgets() {
		updateFaultFamily = new Listener() {
			public void handleEvent(Event event) {
				TreeItem tmp = _tree.getSelection()[0];
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
				
				/*
				List<Category> catList = _categoryManager.getAllCategories();
				for (Iterator<Category> iterator = catList.iterator(); iterator.hasNext();) {
					Category cat = (Category) iterator.next();
					String[] ffs = cat.getAlarms().getFaultFamily();
					for (int i = 0; i < ffs.length; i++) {
						if(ffs[i].compareTo(tmp.getText()) == 0)
							cat.getAlarms().removeFaultFamily(tmp.getText());
					}
					if(cat.getPath().compareTo(_ffCategoryCombo.getText()) == 0){
						cat.getAlarms().addFaultFamily(_ffNameText.getText());
					}
				}*/
				_ffErrorMessageLabel.setText("");
				try{
					_alarmManager.updateFaultFamily(_alarmManager.getFaultFamily(tmp.getText()), fft);
					tmp.setText(_ffNameText.getText());
				}catch(IllegalOperationException e){
					_ffErrorMessageLabel.setText(e.getMessage());
				}catch(NullPointerException e){
					_ffErrorMessageLabel.setText(e.getMessage());
				}
			}
		};
		
		Listener _addCategory = new Listener() {
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

		_ffHelpURLLabel = new Label(_FFgroup,SWT.NONE);
		_ffHelpURLLabel.setText("Help URL");
		_ffHelpURLText = new Text(_FFgroup, SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_ffHelpURLText.setLayoutData(gd);

		_ffContactNameLabel = new Label(_FFgroup, SWT.NONE);
		_ffContactNameLabel.setText("Contact name");
		_ffContactNameText = new Text(_FFgroup, SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_ffContactNameText.setLayoutData(gd);

		_ffContactMailLabel = new Label(_FFgroup, SWT.NONE);
		_ffContactMailLabel.setText("Contact e-mail");
		_ffContactMailText = new Text(_FFgroup, SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_ffContactMailText.setLayoutData(gd);

		_ffContactGSMLabel = new Label(_FFgroup, SWT.NONE);
		_ffContactGSMLabel.setText("Contact GSM");
		_ffContactGSMText = new Text(_FFgroup, SWT.SINGLE | SWT.BORDER);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_ffContactGSMText.setLayoutData(gd);
		
		_ffSourceLabel = new Label(_FFgroup, SWT.NONE);
		_ffSourceLabel.setText("Source");
		_ffSourceCombo = new Combo(_FFgroup, SWT.DROP_DOWN | SWT.READ_ONLY);
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		_ffSourceCombo.setLayoutData(gd);
		
		_ffCategoryLabel = new Label(_FFgroup, SWT.NONE);
		_ffCategoryLabel.setText("Category");
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

		_ffNameText.removeListener(SWT.Modify, updateFaultFamily);
		_ffHelpURLText.removeListener(SWT.Modify, updateFaultFamily);
		_ffContactNameText.removeListener(SWT.Modify, updateFaultFamily);
		_ffContactMailText.removeListener(SWT.Modify, updateFaultFamily);
		_ffContactGSMText.removeListener(SWT.Modify, updateFaultFamily);
		_ffSourceCombo.removeListener(SWT.Modify, updateFaultFamily);
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
				contactName = ff.getContact().getName();
			if( ff.getContact().getEmail() != null )
				contactEmail = ff.getContact().getEmail();
			if( ff.getContact().getGsm() != null )
				contactGsm = ff.getContact().getGsm();
		}
		
		_ffSourceCombo.removeAll();
		_ffCategoryList.removeAll();
			
		if(ff.getAlarmSource() != null)
			source = ff.getAlarmSource();
		
		List<Category> catList = _categoryManager.getAllCategories();
		for (Iterator<Category> iterator = catList.iterator(); iterator.hasNext();) {
			Category cat = (Category) iterator.next();
			try{
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
			}catch(NullPointerException e){
				TableItem  t = new TableItem(_ffCategoryList, SWT.None);
				t.setText(cat.getPath());
				if(cat.getIsDefault()){
					FontData fd = t.getFont().getFontData()[0];
					fd.setStyle(SWT.BOLD);
					t.setFont(new Font(_shell.getDisplay(),fd));
				}
			}
		}
		
		
		Source[] _sourceList = _sourceManager.getAllSources();
		for(int i = 0; i < _sourceList.length ; i++){
			_ffSourceCombo.add(_sourceList[i].getName());
		}

		_ffNameText.setText(name);
		_ffHelpURLText.setText(helpUrl);
		_ffContactNameText.setText(contactName);
		_ffContactMailText.setText(contactEmail);
		_ffContactGSMText.setText(contactGsm);
		for(int i = 0; i < _ffSourceCombo.getItemCount(); i++){
			if(_ffSourceCombo.getItem(i).compareTo(source) == 0)
				_ffSourceCombo.select(i);
		}
		
		_ffNameText.addListener(SWT.Modify, updateFaultFamily);
		_ffHelpURLText.addListener(SWT.Modify, updateFaultFamily);
		_ffContactNameText.addListener(SWT.Modify, updateFaultFamily);
		_ffContactMailText.addListener(SWT.Modify, updateFaultFamily);
		_ffContactGSMText.addListener(SWT.Modify, updateFaultFamily);
		_ffSourceCombo.addListener(SWT.Modify, updateFaultFamily);
		//_ffCategoryCombo.addListener(SWT.Modify, updateFaultFamily);
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
			cause = fc.getCause();
		if(fc.getAction() != null)
			action = fc.getAction();
		if(fc.getConsequence() != null)
			consequence = fc.getConsequence();
		if(fc.getProblemDescription() != null)
			problem = fc.getProblemDescription();

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
				building = fm.getLocation().getBuilding();
			if(fm.getLocation().getFloor() != null)
				floor = fm.getLocation().getFloor();
			if(fm.getLocation().getRoom() != null)
				room = fm.getLocation().getRoom();
			if(fm.getLocation().getMnemonic() != null)
				mnemonic = fm.getLocation().getMnemonic();
			if(fm.getLocation().getPosition() != null)
				position = fm.getLocation().getPosition();
		}


		_fmNameText.setText(name);
		_fmLocBuildingText.setText(building);
		_fmLocFloorText.setText(floor);
		_fmLocRoomText.setText(room);
		_fmLocMnemonicText.setText(mnemonic);
		_fmLocPositionText.setText(position);
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
		_ffSourceCombo.setEnabled(v);
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
	}

	@Override
	public void fillWidgets() {
		// TODO Auto-generated method stub
		
	}

}
