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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.eclipse.ui.part.ViewPart;

import alma.acs.alarmsystem.generated.Alarms;
import alma.acs.alarmsystem.generated.Category;
import alma.acs.alarmsystem.generated.FaultFamily;

import cl.utfsm.acs.acg.core.AlarmManager;
import cl.utfsm.acs.acg.core.AlarmSystemManager;
import cl.utfsm.acs.acg.core.CategoryManager;
import cl.utfsm.acs.acg.core.IllegalOperationException;

/**
 * This class presents to the user all the information regarding to the
 * Categories of the Alarm System configuration. It makes uses of the
 * {@link CategoryManager} object to retrieve and set all the information
 * from and to the CDB.
 * @author rtobar
 */
public class CategoriesView extends ViewPart implements IMyViewPart {

	public static String ID = "cl.utfsm.acs.acg.gui.categoriesview";
	
	/** The {@link CategoryManager} instance associated to this
	 * view. The CategoryManager is in charge of handling the consistency 
	 * of the information that the user inputs in this screen with the
	 * rest of the information that ACG uses.
	 */
	private CategoryManager _categoryManager;
	private AlarmManager _alarmManager;

	/* Widget objects */
	/*** Left **/
	private List _categoriesList;
	private Button _addCategoryButton;
	private Button _deleteCategoryButton;
	private Group _listGroup;
	
	/*** Right ***/
	private Composite _compInitial;
	private Group  _comp;
	private Label  _pathLabel;
	private Text   _pathText;
	private Label  _descriptionLabel;
	private Text   _descriptionText;
	private Label  _isDefaultLabel;
	private Button _isDefaultCheck;
	private Label  _ffLabel;
	private Label  _errorMessageLabel;
	private List   _ffList;

	@Override
	public void createPartControl(Composite parent) {
		setTitleToolTip("Configuration of Alarms Categories.\nCategories group Fault Families.");
		setTitleImage(Activator.getDefault().getImageRegistry().get(Activator.IMG_LINK));
		createViewWidgets(parent);
		refreshContents();
	}

	private void createViewWidgets(Composite parent) {
		SashForm sash = new SashForm(parent, SWT.HORIZONTAL);
		sash.setLayout(new FillLayout());

		/* Left pane */
		Composite categoriesComp = new Composite(sash,SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.numColumns = 1;
		categoriesComp.setLayout(layout);
		
		_listGroup = new Group(categoriesComp,SWT.SHADOW_ETCHED_IN);
		GridData gd = new GridData();
		gd.horizontalAlignment = SWT.FILL;
		gd.verticalAlignment = SWT.FILL;
		gd.grabExcessHorizontalSpace = true;
		gd.grabExcessVerticalSpace = true;
		_listGroup.setLayoutData(gd);
		GridLayout gl = new GridLayout();
		gl.numColumns = 1;
		_listGroup.setLayout(gl);
		_listGroup.setText("Categories List");
		
		_categoriesList = new List(_listGroup,SWT.BORDER | SWT.V_SCROLL);
		_categoriesList.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		_categoriesList.addSelectionListener(new SelectionListener() {
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
			public void widgetSelected(SelectionEvent e) {
				Control c = _compInitial.getChildren()[0];
				if( c instanceof Label ){
					c.dispose();
				}
				_comp.setVisible(true);
				_comp.layout();

				/* Fill with the contents of the selected category */
				/* The default category is stored as the data of the _categoryList */
				/* and is shown with a "*" in the list */
				if(_categoriesList.getSelection() == null || _categoriesList.getSelection().length == 0) {
					_comp.setVisible(false);
					_comp.layout();
					return;
				}
				String categoryName = _categoriesList.getSelection()[0];
				if(categoryName.startsWith("*"))
					fillCategoryInfo((String)_categoriesList.getData());
				else
					fillCategoryInfo(categoryName);
				if(_ffList.getItemCount() == 0)
					_errorMessageLabel.setText("You have to select at least one Fault Family");
			}
		});

		/* Add and remove buttons */
		Composite categoriesButtonsComp = new Composite(categoriesComp,SWT.NONE);
		layout = new GridLayout();
		layout.numColumns = 2;
		categoriesButtonsComp.setLayout(layout);
		categoriesButtonsComp.setLayoutData(new GridData(SWT.FILL, SWT.BOTTOM,true,false));
		
		_addCategoryButton = new Button(categoriesButtonsComp, SWT.None);
		_addCategoryButton.setText("Add");
		_addCategoryButton.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_ADD));
		
		_deleteCategoryButton = new Button(categoriesButtonsComp, SWT.None);
		_deleteCategoryButton.setText("Delete");
		_deleteCategoryButton.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_ETOOL_DELETE));
		
		Listener addCategory = new Listener(){
			public void handleEvent(Event event) {
				InputDialog dialog = new InputDialog(CategoriesView.this.getViewSite().getShell(),
						"New Category",
						"Enter the Category name",
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
					if(_categoryManager.getCategoryByPath(dialog.getValue()) != null) {
						ErrorDialog error = new ErrorDialog(CategoriesView.this.getViewSite().getShell(),
								"Category already exist",
								"The Category "+dialog.getValue()+" already exists in the current configuration",
								new Status(IStatus.ERROR,"cl.utfsm.acs.acg","The Category "+dialog.getValue()+" already exists in the current configuration"),
								IStatus.ERROR);
						error.setBlockOnOpen(true);
						error.open();
						return;
					}
					Category newCategory = new Category();
					newCategory.setPath(dialog.getValue());
					InputDialog dialog2 = new InputDialog(CategoriesView.this.getViewSite().getShell(),
							"Category Description",
							"Enter the Description for the Category",
							null,
							new IInputValidator() {
								public String isValid(String newText) {
									if( newText.trim().compareTo("") == 0 )
										return "The name is empty";
									return null;
								}
							 }
					);
					dialog2.setBlockOnOpen(true);
					dialog2.open();
					String description = dialog2.getValue();
					if(description == null)
						return;
					if (returnCode == InputDialog.OK)
						newCategory.setDescription(description);
					
					java.util.List<String> ffnames = sortFullFaultFamilyList();
					
					ListSelectionDialog dialog3 = new ListSelectionDialog(
							PlatformUI.getWorkbench().getDisplay().getActiveShell(),
							ffnames,
							new ArrayContentProvider(), 
							new LabelProvider(), 
							""
					);

					dialog3.setTitle("Fault Family Selection");
					dialog3.setMessage("List of Fault Families"); 
					dialog3.setBlockOnOpen(true);
					dialog3.open();

					Object ffselected[] = dialog3.getResult();
					if ( ffselected == null )
						return;
					if(ffselected.length ==0){
						try {
							_categoryManager.addCategory(newCategory);
						} catch (IllegalOperationException e) {
							ErrorDialog error = new ErrorDialog(CategoriesView.this.getViewSite().getShell(),
									"Category already exist",
									"The Category "+dialog.getValue()+" already exists in the current configuration",
									new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.getMessage()),
									IStatus.ERROR);
							error.setBlockOnOpen(true);
							error.open();
							return;
						}
					}else{
						Alarms alarms = new Alarms();
						for(int i = 0 ; i < ffselected.length ; i++){
							try{
								alarms.addFaultFamily(_alarmManager.getFaultFamily((String)ffselected[i]).getName());
						  		//alarms.setFaultFamily(i, (String)ffselected[i]);					  		
						  	}catch(NullPointerException e){} 
						  	newCategory.setAlarms(alarms); 	
						}			
						try {
							_categoryManager.addCategory(newCategory);
						} catch (IllegalOperationException e) {
							ErrorDialog error = new ErrorDialog(CategoriesView.this.getViewSite().getShell(),
									"Category already exist",
									"The Category "+dialog.getValue()+" already exists in the current configuration",
									new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.getMessage()),
									IStatus.ERROR);
							error.setBlockOnOpen(true);
							error.open();
							return;
						}
					}
					String[] items = new String[1];
					items[0] = dialog.getValue();
					refreshContents();
					_categoriesList.setSelection(items);
					Event e = new Event();
					_categoriesList.notifyListeners(SWT.Selection, e);
					IWorkbenchWindow _window = getViewSite().getWorkbenchWindow();
					IViewReference[] views = _window.getActivePage().getViewReferences();
					IMyViewPart view = ((IMyViewPart)views[3].getView(false));
					//view.refreshContents();
					view.fillWidgets();
					if(_ffList.getItemCount() == 0)
						_errorMessageLabel.setText("You have to select at least one Fault Family");
				}
				else
					return; 
			}
		};
		_addCategoryButton.addListener(SWT.Selection, addCategory);
		Listener deleteCategory = new Listener() {
			public void handleEvent(Event event) {
				boolean choice = MessageDialog.openQuestion(
						CategoriesView.this.getViewSite().getShell(),
						"Confirmation",
						"Are you sure you want to delete this Category"
				);
		        if ( choice == true ){
		        	String tmp[] = _categoriesList.getSelection();
		        	if( tmp == null || tmp.length == 0) {
		        		ErrorDialog error = new ErrorDialog(CategoriesView.this.getViewSite().getShell(), 
							"Empty selection", "There are no Categories selected to be deleted", new Status(IStatus.ERROR,"cl.utfsm.acs.acg", ""),
							IStatus.ERROR
		        		);
		        		error.setBlockOnOpen(true);
		        		error.open();
		        		return;
		        	}
		        	String category = tmp[0];
		        	if(category.startsWith("*")) {
		        		ErrorDialog error = new ErrorDialog(CategoriesView.this.getViewSite().getShell(),
								"Cannot delete Category",
								"The Category cannot be deleted",
								new Status(IStatus.ERROR,"cl.utfsm.acs.acg","There must be one default category. Please select a different one before removing this category."),
								IStatus.ERROR);
			        		error.setBlockOnOpen(true);
			        		error.open();
			        		return;
		        	}
		        	try {
		        		_categoryManager.deleteCategory(_categoryManager.getCategoryByPath(category));
		        	} catch (IllegalOperationException e) {
		        		ErrorDialog error = new ErrorDialog(CategoriesView.this.getViewSite().getShell(),
							"Cannot delete Category",
							"The Category cannot be deleted",
							new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.getMessage()),
							IStatus.ERROR);
		        		error.setBlockOnOpen(true);
		        		error.open();
		        		return;
		        	}
		        	String[] items = null;
		        	if(_categoriesList.getSelection() != null && _categoriesList.getSelection().length != 0) {
						items = _categoriesList.getSelection();
					refreshContents();
					if(items == null)
						if(_categoriesList.getItemCount() > 0)
							_categoriesList.setSelection(0);
					} else
						_categoriesList.setSelection(items);
					Event e = new Event();
					_categoriesList.notifyListeners(SWT.Selection, e);
					IWorkbenchWindow _window = getViewSite().getWorkbenchWindow();
					IViewReference[] views = _window.getActivePage().getViewReferences();
					IMyViewPart view = ((IMyViewPart)views[3].getView(false));
					//view.refreshContents();
					view.fillWidgets();
		        }
			}
		};
		_deleteCategoryButton.addListener(SWT.Selection, deleteCategory);

		/* To delete a FF from a given Category */ 
		Listener deleteFaultFamily  = new Listener() {
			public void handleEvent(Event event) {
				Category c = _categoryManager.getCategoryByPath(_pathText.getText());
				try {
					String[] ff = c.getAlarms().getFaultFamily();			
					Alarms alarms = new Alarms();
					String[] temp = _ffList.getSelection();
					//int j = 0;
					for (int i = 0 ; i < ff.length ; i++){
						if( ff[i].compareTo(temp[0]) == 0 ){
							_ffList.remove(temp[0]);
							c.getAlarms().removeFaultFamily(ff[i]);
						}
						else{
							alarms.addFaultFamily(ff[i]);
							//alarms.setFaultFamily(j, ff[i]);
							//j++;
						}
					}
					c.setAlarms(alarms);
					_categoryManager.updateCategory(c,c);
					if(_ffList.getItemCount() == 0)
						_errorMessageLabel.setText("You have to select at least one Fault Family");
					IWorkbenchWindow _window = getViewSite().getWorkbenchWindow();
					IViewReference[] views = _window.getActivePage().getViewReferences();
					IMyViewPart view = ((IMyViewPart)views[3].getView(false));
					//view.refreshContents();
					view.fillWidgets();
					boolean inUse = false;
					boolean def = false;
					String defCat = "";
					for(Category cat: _categoryManager.getAllCategories()) {
						String[] ffs = cat.getAlarms().getFaultFamily();
						for(String tff : ffs) {
							if(tff.compareTo(temp[0]) == 0)
								inUse = true;
						}
						if(cat.getIsDefault()) {
							def = true;
							defCat = cat.getPath();
						}
					}
					if(!inUse) {
						String msg;
						if(def)
							msg = "Default category: "+defCat;
						else
							msg = "No default category";
						ErrorDialog error = new ErrorDialog(CategoriesView.this.getViewSite().getShell(),
								"Fault Family isn't in any Categoty",
								"The Fault Family is not part of any Category",
								new Status(IStatus.WARNING,"cl.utfsm.acs.acg","The Fault Family "+temp[0]+" is not part of any Category ("+msg+")"),
								IStatus.WARNING);
						error.setBlockOnOpen(true);
						error.open();
					}
				}catch(Exception e){
					e.printStackTrace();
				}
			}
	
		};
		
		/* To delete all FF from a given Category */ 
		Listener deleteAllFaultFamily  = new Listener() {
			public void handleEvent(Event event) {
				Category c = _categoryManager.getCategoryByPath(_pathText.getText());
				try {
					String[] ff = c.getAlarms().getFaultFamily();			
					Alarms alarms = new Alarms(); 
					for (int i = 0 ; i < ff.length ; i++){
						_ffList.remove(ff[i]);
						c.getAlarms().removeFaultFamily(ff[i]);
					}
					c.setAlarms(alarms);
					_categoryManager.updateCategory(c,c);
					IWorkbenchWindow _window = getViewSite().getWorkbenchWindow();
					IViewReference[] views = _window.getActivePage().getViewReferences();
					IMyViewPart view = ((IMyViewPart)views[3].getView(false));
					//view.refreshContents();
					view.fillWidgets();
					java.util.List<String> ffList = new ArrayList<String>();
					boolean def = false;
					String defCat = "";
					for(String cff: ff) {
						Boolean inUse = false;
						for(Category cat: _categoryManager.getAllCategories()) {
							String[] ffs = cat.getAlarms().getFaultFamily();
							for(String tff : ffs) {
								if(tff.compareTo(cff) == 0)
									inUse = true;
							}
							if(cat.getIsDefault()) {
								def = true;
								defCat = cat.getPath();
							}
						}
						if(!inUse)
							ffList.add(cff);
					}
					if(ffList.size() > 0) {
						String list = "";
						for(String ffel : ffList)
							list = list + ffel + ", ";
						list.substring(0,list.length()-2);
						String msg;
						if(def)
							msg = "Default category: "+defCat;
						else
							msg = "No default category";
						ErrorDialog error = new ErrorDialog(CategoriesView.this.getViewSite().getShell(),
								"Fault Family isn't in any Categoty",
								"The Fault Family is not part of any Category",
								new Status(IStatus.WARNING,"cl.utfsm.acs.acg","The Fault Family(ies) "+list+" is not part of any Category ("+msg+")"),
								IStatus.WARNING);
						error.setBlockOnOpen(true);
						error.open();
					}
				}catch(Exception e){
					e.printStackTrace();
				}
				if(_ffList.getItemCount() == 0)
					_errorMessageLabel.setText("You have to select at least one Fault Family");
			}
		};
		
		/* To add a new FF to a Category */ 
		Listener addFaultFamily  = new Listener() {
			public void handleEvent(Event event) {
				Category c = _categoryManager.getCategoryByPath(_pathText.getText());
				java.util.List<String> currentffs = new ArrayList<String>();
				try{
					String[] ffss = c.getAlarms().getFaultFamily();
					for (int i = 0 ; i < ffss.length ; i++){
						currentffs.add(ffss[i]);
					}
				}catch(NullPointerException e) {
					e.printStackTrace();
				}
				java.util.List<String> ffnames = sortFullFaultFamilyList();
				ListSelectionDialog dialog3 = new ListSelectionDialog(
						PlatformUI.getWorkbench().getDisplay().getActiveShell(),
				        ffnames,
				        new ArrayContentProvider(), 
				        new LabelProvider(), 
				        ""
				);

				dialog3.setTitle("Fault Family Selection");
				dialog3.setMessage("List of Fault Families"); 
				dialog3.setBlockOnOpen(true);
				if(currentffs != null)
					dialog3.setInitialElementSelections(currentffs);
				dialog3.open();
				if(dialog3.getReturnCode() == InputDialog.OK) {
					Object ffselected[] = dialog3.getResult();
					try{
						Alarms alarms = new Alarms();
						for(int i = 0 ; i < ffselected.length ; i++)
							alarms.addFaultFamily(_alarmManager.getFaultFamily((String)ffselected[i]).getName());  
						c.setAlarms(alarms);
						_categoryManager.updateCategory(c, c);
						sortCategoryFaultFamilyList(c.getPath());
					}catch(Exception e){
						e.printStackTrace();
					}
					String[] items = _categoriesList.getSelection();
					refreshContents();
					_categoriesList.setSelection(items);
					Event e = new Event();
					_categoriesList.notifyListeners(SWT.Selection, e);
					IWorkbenchWindow _window = getViewSite().getWorkbenchWindow();
					IViewReference[] views = _window.getActivePage().getViewReferences();
					IMyViewPart view = ((IMyViewPart)views[3].getView(false));
					//view.refreshContents();
					view.fillWidgets();
					if(_ffList.getItemCount() == 0)
						_errorMessageLabel.setText("You have to select at least one Fault Family");
				}
			}
		};

		/* Initial label when no categories are selected */
		_compInitial = new Composite(sash,SWT.NONE);
		_compInitial.setLayout(new GridLayout());
		new Label(_compInitial,SWT.NONE).setText("Select a category");

		/* Fill the right pane Group that will be shown when
		 * a category is selected in the left list */
		layout = new GridLayout();
		layout.numColumns = 2;
		GridData gridData = new GridData();
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		gridData.horizontalAlignment = SWT.FILL;
		gridData.verticalAlignment = SWT.FILL;
		_comp = new Group(_compInitial, SWT.SHADOW_ETCHED_IN);
		_comp.setText("Category information");
		_comp.setLayout(layout);
		_comp.setLayoutData(gridData);

		_pathLabel = new Label(_comp,SWT.NONE);
		_pathLabel.setText("Category name");
		_pathText  = new Text(_comp,SWT.SINGLE | SWT.BORDER);
		gridData = new GridData();
		gridData.horizontalAlignment = SWT.FILL;
		gridData.grabExcessHorizontalSpace = true;
		_pathText.setLayoutData(gridData);

		_descriptionLabel = new Label(_comp, SWT.NONE);
		_descriptionLabel.setText("Category description");
		_descriptionText  = new Text(_comp,SWT.SINGLE | SWT.BORDER);
		gridData = new GridData();
		gridData.horizontalAlignment = SWT.FILL;
		gridData.grabExcessHorizontalSpace = true;
		_descriptionText.setLayoutData(gridData);

		_isDefaultLabel = new Label(_comp, SWT.NONE);
		_isDefaultLabel.setText("Is default category?");
		_isDefaultCheck = new Button(_comp, SWT.CHECK);

		_ffLabel = new Label(_comp, SWT.NONE);
		_ffLabel.setText("Fault Families:");
		gridData = new GridData();
		gridData.verticalAlignment = SWT.TOP;
		gridData.horizontalSpan = 2;
		_ffLabel.setLayoutData(gridData);
		_ffList = new List(_comp,SWT.V_SCROLL | SWT.BORDER);
		gridData = new GridData();
		gridData.horizontalAlignment = SWT.FILL;
		gridData.grabExcessHorizontalSpace = true;
		gridData.verticalAlignment = SWT.FILL;
		gridData.grabExcessVerticalSpace = true;
		gridData.horizontalSpan = 2;
		_ffList.setLayoutData(gridData);
		
		_errorMessageLabel = new Label(_comp,SWT.NONE);
		_errorMessageLabel.setText("A");
		_errorMessageLabel.setForeground(getViewSite().getShell().getDisplay().getSystemColor(SWT.COLOR_RED));
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		gd.horizontalSpan = 2;
		_errorMessageLabel.setLayoutData(gd);

		/* Adding a click right menu to modify the FF of a given Category */
		Menu treePopUp1 = new Menu(parent);
		MenuItem treePopUpAddFF = new MenuItem(treePopUp1,SWT.PUSH);
		treePopUpAddFF.setText("Add a new Fault Family");
		treePopUpAddFF.addListener(SWT.Selection, addFaultFamily);
		MenuItem treePopUpDeleteFF = new MenuItem(treePopUp1,SWT.PUSH);
		treePopUpDeleteFF.setText("Delete this Fault Family");
		treePopUpDeleteFF.addListener(SWT.Selection, deleteFaultFamily);
		MenuItem treePopUpDeleteAllFF = new MenuItem(treePopUp1,SWT.PUSH);
		treePopUpDeleteAllFF.setText("Delete All Fault Families");
		treePopUpDeleteAllFF.addListener(SWT.Selection, deleteAllFaultFamily);
		_ffList.setMenu(treePopUp1); 
		
		/* Adding a click menu to add/delete Categories */
		Menu treePopUp2 = new Menu(parent);
		MenuItem treePopUpaddCategory = new MenuItem(treePopUp2,SWT.PUSH);
		treePopUpaddCategory.setText("Add a new Category");
		treePopUpaddCategory.addListener(SWT.Selection, addCategory);
		MenuItem treePopUpdeleteCategory = new MenuItem(treePopUp2,SWT.PUSH);
		treePopUpdeleteCategory.setText("Delete this Category");
		treePopUpdeleteCategory.addListener(SWT.Selection, deleteCategory);
		_categoriesList.setMenu(treePopUp2); 
		_comp.setVisible(false);

		/* Set a weight for each side of the view */
		sash.setWeights(new int[] {3, 5});
		Listener updateCategory = new Listener() {
			public void handleEvent(Event e) {
				updateName();
			}
		};
		_descriptionText.addListener(SWT.Modify , updateCategory);  
		_pathText.addListener(SWT.Modify , updateCategory);  
		
		_isDefaultCheck.addListener(SWT.Selection, new Listener(){
			public void handleEvent(Event e) {
				String category;
				if(_categoriesList.getSelection()[0].startsWith("*"))
					category = (String)_categoriesList.getData();
				else
					category = _categoriesList.getSelection()[0];
				if( _categoryManager.getCategoryByPath(category).getIsDefault() == true){
					_isDefaultCheck.setSelection(true);
					MessageBox messageBox = new MessageBox(PlatformUI.getWorkbench().getDisplay().getActiveShell(), SWT.ICON_ERROR);
					messageBox.setMessage("The Default Category always must exist, you can only change it");
					messageBox.open();
				}
				else{
					_categoryManager.updateDefaultCategory(_categoryManager.getCategoryByPath(_categoriesList.getSelection()[0]));
					String[] items = _categoriesList.getSelection();
					refreshContents();
					items[0] = "* "+items[0];
					_categoriesList.setSelection(items);
					Event e2 = new Event();
					_categoriesList.notifyListeners(SWT.Selection, e2);
					IWorkbenchWindow _window = getViewSite().getWorkbenchWindow();
					IViewReference[] views = _window.getActivePage().getViewReferences();
					IMyViewPart view = ((IMyViewPart)views[3].getView(false));
					//view.refreshContents();
					view.fillWidgets();
				}
			}
		});  
	}

	/* (non-Javadoc)
	 * @see cl.utfsm.acs.acg.gui.IMyViewPart#refreshContents()
	 */
	public void refreshContents() {
		_categoriesList.removeAll();
		_categoryManager = AlarmSystemManager.getInstance().getCategoryManager();
		_alarmManager = AlarmSystemManager.getInstance().getAlarmManager();
		sortCategoryList();
		_comp.setVisible(false);
	}
	
	
	public void updateName(){
		String[] lststr = _categoriesList.getSelection();
		int[] lstsel = _categoriesList.getSelectionIndices();
		if(lststr == null || lststr.length == 0)
			return;
		if(lstsel == null || lstsel.length == 0)
			return;
		String tmp = lststr[0];
		int sel = lstsel[0];
		if(tmp.startsWith("*"))
			tmp = (String) _categoriesList.getData();
		Category c = _categoryManager.getCategoryByPath(tmp);
		if(c == null)
			return;
		Category ci = new Category();
		if(_pathText.getText().isEmpty()) {
			_errorMessageLabel.setText("Category Path Missing!");
			return;
		}
		if(_pathText.getText().contains(" ")) {
			_errorMessageLabel.setText("Invalid Category Path. No spaces allowed.");
			return;
		}
		ci.setPath(_pathText.getText());
		if(_descriptionText.getText().isEmpty()) {
			_errorMessageLabel.setText("Category Description is Missing!");
			return;
		}
		ci.setDescription(_descriptionText.getText());
		ci.setIsDefault(c.getIsDefault());
		ci.setAlarms(c.getAlarms());
		_errorMessageLabel.setText("");
		try{
			_categoryManager.updateCategory(c, ci);
			if(ci.getIsDefault())
				_categoriesList.setItem(sel, "* "+_pathText.getText());
			else
				_categoriesList.setItem(sel, _pathText.getText());
			sortCategoryList();
			if(tmp.compareTo(ci.getPath()) != 0) {
				IWorkbenchWindow _window = getViewSite().getWorkbenchWindow();
				IViewReference[] views = _window.getActivePage().getViewReferences();
				IMyViewPart view = ((IMyViewPart)views[3].getView(false));
				//view.refreshContents();
				view.fillWidgets();
			}
		}catch(IllegalOperationException e){
			_errorMessageLabel.setText(e.getMessage());
		}catch(NullPointerException e){
			_errorMessageLabel.setText(e.getMessage());
		}
    }
	
	public java.util.List<String> sortFullFaultFamilyList() {
		java.util.List<FaultFamily> ffs = _alarmManager.getAllAlarms();
		java.util.List<String> sortedFFs = new ArrayList<String>();
		java.util.List<String> returnFFs = new ArrayList<String>();
		for(FaultFamily ff: ffs)
			sortedFFs.add(ff.getName().toLowerCase());
		Collections.sort(sortedFFs);
		
		for(String sff: sortedFFs)
			for(FaultFamily ff: ffs)
				if(sff.compareTo(ff.getName().toLowerCase()) == 0)
					returnFFs.add(ff.getName());

		return returnFFs;
	}
	
	public void sortCategoryFaultFamilyList(String string) {
		Category cat = _categoryManager.getCategoryByPath(string);
		_ffList.removeAll();
		if(cat == null)
			return;
		if(cat.getAlarms() == null)
			return;
		String[] ffs = cat.getAlarms().getFaultFamily();
		java.util.List<String> sortedFFs = new ArrayList<String>();
		for(String ff: ffs)
			sortedFFs.add(ff.toLowerCase());
		Collections.sort(sortedFFs);
		
		for(String sff: sortedFFs)
			for(String ff: ffs)
				if(sff.compareTo(ff.toLowerCase()) == 0)
					_ffList.add(ff);   
	}
	
	public void sortCategoryList() {
		String[] tmp = _categoriesList.getSelection();
		_categoriesList.removeAll();
		java.util.List<String> sortedCats = new ArrayList<String>();
		java.util.List<Category> cats = _categoryManager.getAllCategories();
		for(Category cat:cats)
			sortedCats.add(cat.getPath().toLowerCase());
		Collections.sort(sortedCats);
		for(String scat: sortedCats)
			for(Category cat: cats)
				if(cat.getPath().toLowerCase().compareTo(scat) == 0) {
					if(cat.getIsDefault()) {
						_categoriesList.add("* "+cat.getPath());
						_categoriesList.setData(cat.getPath());
					}
					else
						_categoriesList.add(cat.getPath());
				}
		_categoriesList.setSelection(tmp);
	}

	@Override
	public void setFocus() {
	}


	private void fillCategoryInfo(String string) {
		Category cat = _categoryManager.getCategoryByPath(string);
		if(cat == null)
			return;
		
		String descriptionText = "";
		String pathText = "";
		boolean isDefaultCheck = false;
		_ffList.removeAll();
		
		if(cat.getDescription() != null)
			descriptionText = cat.getDescription().trim();
		if(cat.getPath() != null)
			pathText = cat.getPath().trim();
		if(cat.hasIsDefault())
			isDefaultCheck = cat.getIsDefault();
		sortCategoryFaultFamilyList(string);
		
		if(_ffList.getItemCount() == 0)
			_errorMessageLabel.setText("You have to select at least one Fault Family");
		
		_descriptionText.setText(descriptionText);
		_pathText.setText(pathText);
		_isDefaultCheck.setSelection(isDefaultCheck);
		_compInitial.layout();
	}

	/* (non-Javadoc)
	 * @see cl.utfsm.acs.acg.gui.IMyViewPart#setEnabled(boolean)
	 */
	public void setEnabled(boolean v) {
		/* Left */
		_categoriesList.setEnabled(v);
		_addCategoryButton.setEnabled(v);
		_deleteCategoryButton.setEnabled(v);
		/* Right */
		_pathText.setEnabled(v);
		_descriptionText.setEnabled(v);
		_isDefaultCheck.setEnabled(v);
		_ffList.setEnabled(v);
	}

	@Override
	public void fillWidgets() {
		String[] tmp = _categoriesList.getSelection();
		if(tmp==null || tmp.length==0){
			return;
		}
		if(tmp[0].startsWith("*"))
			fillCategoryInfo((String)_categoriesList.getData());
		else
			fillCategoryInfo(tmp[0]);
	}
	
	public void setReadOnly(boolean v) {
		/*** Left **/
		if(v) {
			_categoriesList.setMenu(null);
			_ffList.setMenu(null);
		} else {
			_categoriesList.setMenu(null);
			_ffList.setMenu(null);
		}
		_addCategoryButton.setEnabled(!v);
		_deleteCategoryButton.setEnabled(!v);
		_pathText.setEnabled(!v);
		_descriptionText.setEnabled(!v);
		_isDefaultCheck.setEnabled(!v);
	}
}