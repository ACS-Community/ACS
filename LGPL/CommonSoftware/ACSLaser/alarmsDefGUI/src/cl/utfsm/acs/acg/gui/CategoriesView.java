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
		_categoriesList = new List(categoriesComp,SWT.BORDER | SWT.V_SCROLL);
		_categoriesList.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		_categoriesList.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}

			public void widgetSelected(SelectionEvent e) {
				Control c = _compInitial.getChildren()[0];

				if( c instanceof Label ){
					c.dispose();
					_comp.setVisible(true);
					_comp.layout();
				}

				/* Fill with the contents of the selected category */
				/* The default category is stored as the data of the _categoryList */
				/* and is shown with a "*" in the list */
				String categoryName = _categoriesList.getSelection()[0];
				if( categoryName.compareTo("* " + (String)_categoriesList.getData()) == 0 )
					fillCategoryInfo((String)_categoriesList.getData());
				else
					fillCategoryInfo(categoryName);
		
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
					
					if(description == null){
						
						return;
						
					}
					
					if (returnCode == InputDialog.OK) {
						newCategory.setDescription(description);
					}
					
					// sorting a list to show and select the Fault Families to the New Category
					_alarmManager = AlarmSystemManager.getInstance().getAlarmManager();
					java.util.List<FaultFamily> ffs = _alarmManager.getAllAlarms();
					java.util.List<String> ffnames = new ArrayList<String>();
					java.util.List<FaultFamily> sortedFFList = new ArrayList<FaultFamily>();
					
					java.util.List<String> tmp = new ArrayList<String>();
					for (Iterator<FaultFamily> iterator = ffs.iterator(); iterator.hasNext();) {
						tmp.add(((FaultFamily)iterator.next()).getName().toLowerCase());
					}
					Collections.sort(tmp);
					for (Iterator<String> iterator = tmp.iterator(); iterator.hasNext();) {
						String name = (String) iterator.next();
						for (Iterator<FaultFamily> iterator2 = ffs.iterator(); iterator2.hasNext();) {
							FaultFamily ff = (FaultFamily) iterator2.next();
							if( ff.getName().toLowerCase().compareTo(name) == 0 ) {
								sortedFFList.add(ff);
								break;
							}
						}
					}
					ffs = sortedFFList;
					
					for(int i=0 ; i < ffs.size() ; i++){
						ffnames.add(i, ffs.get(i).getName());
					}
					
					ListSelectionDialog dialog3 = new ListSelectionDialog(
					        PlatformUI.getWorkbench().getDisplay().getActiveShell(),
					        ffnames,
					        new ArrayContentProvider(), 
					        new LabelProvider(), 
					        "");

					dialog3.setTitle("Fault Family Selection");
					dialog3.setMessage("List of Fault Families"); 
					dialog3.setBlockOnOpen(true);
					dialog3.open();
					
					Object ffselected[] = dialog3.getResult();
					
					if ( ffselected == null ){
						
					return;	
					}
					
					if(ffselected.length ==0){
				
						
						try {
								_categoryManager.addCategory(newCategory);
							} catch (IllegalOperationException e) {
								ErrorDialog error = new ErrorDialog(CategoriesView.this.getViewSite().getShell(),
													"Category already exist",
													"The Category "+dialog.getValue()+" already exists in the current configuration",
													new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.toString()),
													IStatus.ERROR);
								error.setBlockOnOpen(true);
								error.open();
							} 
							refreshContents();
					}else{
						
						Alarms alarms = new Alarms();
						for(int i = 0 ; i < ffselected.length ; i++){
						  	try{
						  		alarms.addFaultFamily(_alarmManager.getFaultFamily(ffselected[i].toString()).toString());
						  		alarms.setFaultFamily(i, ffselected[i].toString());					  		
						  	}catch(NullPointerException e){} 
						  	
						  	newCategory.setAlarms(alarms); 	
						}			
						
						
					try {
							_categoryManager.addCategory(newCategory);
						} catch (IllegalOperationException e) {
							ErrorDialog error = new ErrorDialog(CategoriesView.this.getViewSite().getShell(),
												"Category already exist",
												"The Category "+dialog.getValue()+" already exists in the current configuration",
												new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.toString()),
												IStatus.ERROR);
							error.setBlockOnOpen(true);
							error.open();
						} 
						refreshContents();
					}
			
				}
				
				else
					return; 
								
			}
			
		};
		
		_addCategoryButton.addListener(SWT.Selection, addCategory);
		
		Listener deleteCategory = new Listener(){
			
			public void handleEvent(Event event) {
		        boolean choice = MessageDialog.openQuestion( 
		        		  CategoriesView.this.getViewSite().getShell(),
		            "Confirmation",
		            "Are you sure you want to delete this Category");
		        
		        if ( choice == true ){
		        
				String tmp[] = _categoriesList.getSelection();
				if( tmp == null ) {
					ErrorDialog error = new ErrorDialog(CategoriesView.this.getViewSite().getShell(), 
							"Empty selection", "There are no Categories selected to be deleted", new Status(IStatus.ERROR,"cl.utfsm.acs.acg", "asd"),
							IStatus.ERROR);
					error.setBlockOnOpen(true);
					error.open();
					return;
				}
				String category = tmp[0];    
				try {
					_categoryManager.deleteCategory(_categoryManager.getCategoryByPath(category));
				} catch (IllegalOperationException e) {
					ErrorDialog error = new ErrorDialog(CategoriesView.this.getViewSite().getShell(),
							"Cannot delete Category",
							"The Category cannot be deleted",
							new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.toString()),
							IStatus.ERROR);
					error.setBlockOnOpen(true);
					error.open();
					
				
				} catch(NullPointerException e){
					
				}
				
				refreshContents();
			}
		
			}
			
		};
		
		_deleteCategoryButton.addListener(SWT.Selection, deleteCategory);
			    

		/* To delete a FF from a given Category */ 
		Listener deleteFaultFamily  = new Listener() {
			public void handleEvent(Event event) {

				Category c = _categoryManager.getCategoryByPath(_pathText.getText());
				try{
				String[] ff = c.getAlarms().getFaultFamily();			
				Alarms alarms = new Alarms();
				String[] temp = _ffList.getSelection(); 
				for (int i = 0 ; i < ff.length ; i++){
					if( ff[i].toString().compareTo(temp[0]) == 0 ){
						_ffList.remove(temp[0]);
						c.getAlarms().removeFaultFamily(ff[i]);
					}
					else{
					alarms.addFaultFamily(ff[i].toString());
			  		alarms.setFaultFamily(i, ff[i].toString());	
					}
				}
				
				c.setAlarms(alarms);
				_categoryManager.updateCategory(c);
				
				IWorkbenchWindow _window = getViewSite().getWorkbenchWindow();
				IViewReference[] views = _window.getActivePage().getViewReferences();
				IMyViewPart view = ((IMyViewPart)views[3].getView(false));
				view.refreshContents();
			
			}catch(Exception e){}
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
				}catch(NullPointerException e){}
			
				
				_alarmManager = AlarmSystemManager.getInstance().getAlarmManager();
				java.util.List<FaultFamily> ffs = _alarmManager.getAllAlarms();
				java.util.List<String> ffnames = new ArrayList<String>();
				java.util.List<FaultFamily> sortedFFList = new ArrayList<FaultFamily>();
				
				java.util.List<String> tmp = new ArrayList<String>();
				for (Iterator<FaultFamily> iterator = ffs.iterator(); iterator.hasNext();) {
					tmp.add(((FaultFamily)iterator.next()).getName().toLowerCase());
				}
				Collections.sort(tmp);
				for (Iterator<String> iterator = tmp.iterator(); iterator.hasNext();) {
					String name = (String) iterator.next();
					for (Iterator<FaultFamily> iterator2 = ffs.iterator(); iterator2.hasNext();) {
						FaultFamily ff = (FaultFamily) iterator2.next();
						if( ff.getName().toLowerCase().compareTo(name) == 0 ) {
							sortedFFList.add(ff);
							break;
						}
					}
				}
				ffs = sortedFFList;
				
				for(int i=0 ; i < ffs.size() ; i++){
					ffnames.add(i, ffs.get(i).getName());
				}
				
				ListSelectionDialog dialog3 = new ListSelectionDialog(
				        PlatformUI.getWorkbench().getDisplay().getActiveShell(),
				        ffnames,
				        new ArrayContentProvider(), 
				        new LabelProvider(), 
				        "");

				dialog3.setTitle("Fault Family Selection");
				dialog3.setMessage("List of Fault Families"); 
				dialog3.setBlockOnOpen(true);
				if(currentffs != null){
				dialog3.setInitialElementSelections(currentffs);
				}
				dialog3.open();
				
				Object ffselected[] = dialog3.getResult();
				
				try{
					
					Alarms alarms = new Alarms();
					_ffList.removeAll();
					for(int i = 0 ; i < ffselected.length ; i++){
					  	
					  		alarms.addFaultFamily(_alarmManager.getFaultFamily(ffselected[i].toString()).toString());
					  		alarms.setFaultFamily(i, ffselected[i].toString());		
					  		c.setAlarms(alarms);
					  		_ffList.add(ffselected[i].toString());  
					  		
					  	}
				}catch(Exception e){} 
					 
		
				refreshContents();
				
				IWorkbenchWindow _window = getViewSite().getWorkbenchWindow();
				IViewReference[] views = _window.getActivePage().getViewReferences();
				IMyViewPart view = ((IMyViewPart)views[3].getView(false));
				view.refreshContents();

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
		gridData.horizontalAlignment = SWT.FILL;
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
		_ffLabel.setLayoutData(gridData);
		_ffList = new List(_comp,SWT.V_SCROLL | SWT.BORDER);
		gridData = new GridData();
		gridData.horizontalAlignment = SWT.FILL;
		gridData.grabExcessHorizontalSpace = true;
		_ffList.setLayoutData(gridData);
		
		
		/* Adding a click right menu to modify the FF of a given Category */
		Menu treePopUp1 = new Menu(parent);
		MenuItem treePopUpDeleteFF = new MenuItem(treePopUp1,SWT.PUSH);
		treePopUpDeleteFF.setText("Delete this Fault Family");
		treePopUpDeleteFF.addListener(SWT.Selection, deleteFaultFamily);
		MenuItem treePopUpAddFF = new MenuItem(treePopUp1,SWT.PUSH);
		treePopUpAddFF.setText("Add a new Fault Family");
		treePopUpAddFF.addListener(SWT.Selection, addFaultFamily);
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
		
		
		_descriptionText.addListener(SWT.Modify , new Listener() {
			public void handleEvent(Event e) {
				java.util.List<Category> categories = _categoryManager.getAllCategories();
				String tmp[] = _categoriesList.getSelection();
				
				for (int i = 0; i < categories.size() ;  i++) {
					if ( categories.get(i).getPath().compareTo(tmp[0].toString()) == 0 ){
						categories.get(i).setDescription(_descriptionText.getText());
					}
					}
			
			}
			});  
		
		_pathText.addListener(SWT.Modify , new Listener() {
			public void handleEvent(Event e) {
					updateName();
				}
			});  
		
		_isDefaultCheck.addListener(SWT.Selection, new Listener(){
			public void handleEvent(Event e) {
					if( _categoryManager.getCategoryByPath(_categoriesList.getSelection()[0]).getIsDefault() == true){
						_isDefaultCheck.setSelection(true);
						 MessageBox messageBox = new MessageBox(PlatformUI.getWorkbench().getDisplay().getActiveShell(), SWT.ICON_ERROR);
						 messageBox.setMessage("The Default Category always must exist, you can only change it");
						 messageBox.open();
					}
					else{
					_categoryManager.updateDefaultCategory(_categoryManager.getCategoryByPath(_categoriesList.getSelection()[0]));
					refreshContents();
					IWorkbenchWindow _window = getViewSite().getWorkbenchWindow();
					IViewReference[] views = _window.getActivePage().getViewReferences();
					IMyViewPart view = ((IMyViewPart)views[3].getView(false));
					view.refreshContents();
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
		java.util.List<Category> categs = _categoryManager.getAllCategories();
		java.util.List<String> temp = new ArrayList<String>();
		java.util.List<Category> sortedCategories = new ArrayList<Category>();
		
		for (Iterator<Category> iterator = categs.iterator(); iterator.hasNext();) {
			temp.add(((Category)iterator.next()).getPath().toLowerCase());
		}   
		
		/* Sorting the Categories by alphabetic name with a temporal list  */ 
		Collections.sort(temp);
		
		for (Iterator<String> iterator = temp.iterator(); iterator.hasNext();) {
			String name = (String) iterator.next();
			for (Iterator<Category> iterator2 = categs.iterator(); iterator2.hasNext();) {
				Category ct = (Category) iterator2.next();
				if( ct.getPath().toLowerCase().compareTo(name) == 0 ) {
					sortedCategories.add(ct);
					break;
				}
			}
		}
	
		_categoriesList.removeAll();
		for (Category cat : sortedCategories) {	
			if( cat.getIsDefault() == true ) {
				_categoriesList.add("* " + cat.getPath());
				_categoriesList.setData(cat.getPath());
			}
			else {
				_categoriesList.add(cat.getPath());
			}
	}
		_categoriesList.deselectAll();
		
	
	}
	
	
	public void updateName(){
		java.util.List<Category> categories = _categoryManager.getAllCategories();
		String tmp[] = _categoriesList.getSelection();
		
		for (int i = 0; i < categories.size() ;  i++) {
			if ( categories.get(i).getPath().compareTo(tmp[0].toString()) == 0 ){
				categories.get(i).setPath(_pathText.getText());
				categories.get(i).setDescription(_descriptionText.getText());
			}
			}

		
		for(int i = 0 ; i < categories.size() ; i++ ){
			if( _categoriesList.getItem(i).compareTo(tmp[0]) == 0 ){
				_categoriesList.setItem(i, _pathText.getText());
			}
		}
    }

	@Override
	public void setFocus() {
	}


	private void fillCategoryInfo(String string) {

		Category cat = _categoryManager.getCategoryByPath(string);

		_descriptionText.setText(cat.getDescription().trim());
		_pathText.setText(cat.getPath().trim());
		_isDefaultCheck.setSelection(cat.getIsDefault());

		_ffList.removeAll();
		
		try{
		String[] ffs = cat.getAlarms().getFaultFamily();
		if( ffs != null ){
			for (int i = 0; i < ffs.length; i++) {
				_ffList.add(ffs[i]);          
			}
			}
		
		}catch(NullPointerException e){}      

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
		fillCategoryInfo(tmp[0]);
		
	}

}