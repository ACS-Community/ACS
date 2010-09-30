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
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Button;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import cern.laser.business.data.Source;
import cl.utfsm.acs.acg.core.AlarmSystemManager;
import cl.utfsm.acs.acg.core.IllegalOperationException;
import cl.utfsm.acs.acg.core.SourceManager;

public class SourcesView extends ViewPart implements IMyViewPart {

	public static String ID = "cl.utfsm.acs.acg.gui.sourcesview";

	private SourceManager _sourceManager;

	/* High level widgets */
	private SashForm _sash;
	private Composite _sourcesComp;
	private Composite _compInitial;

	/* Left side widgets */
	private List _sourcesList;
	private Composite _sourcesButtonsComp;
	private Button _addSourceButton;
	private Button _deleteSourceButton;
	private Group _listGroup;

	/* Source information */
	private Label _sourceNameLabel;
	private Label _descriptionLabel;
	private Text _sourceNameText;
	private Text _descriptionText;
	private Label _errorMessageLabel;
	private Group _group;

	@Override
	public void createPartControl(Composite parent) {
		setTitleToolTip("Configuration of Sources.\nFault Families are published into Sources");
		setTitleImage(Activator.getDefault().getImageRegistry().get(Activator.IMG_SOURCE));
		createViewWidgets(parent);
		refreshContents();
	}

	/* (non-Javadoc)
	 * @see cl.utfsm.acs.acg.gui.IMyViewPart#refreshContents()
	 */
	public void refreshContents() {
		_sourcesList.removeAll();
		_sourceManager = AlarmSystemManager.getInstance().getSourceManager();
		Source[] sources = _sourceManager.getAllSources();
		java.util.List<String> sortedSources = new ArrayList<String>();
		for(Source src: sources) {
			sortedSources.add(src.getDescription());
		}
		Collections.sort(sortedSources);
		for (String src: sortedSources) {
			_sourcesList.add(src);
		}
	}

	private void createViewWidgets(final Composite parent) {
		_sash = new SashForm(parent, SWT.HORIZONTAL);
		_sash.setLayout(new FillLayout());

		/* Left pane */
		_sourcesComp = new Composite(_sash,SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.numColumns = 1;
		_sourcesComp.setLayout(layout);
		
		_listGroup = new Group(_sourcesComp,SWT.SHADOW_ETCHED_IN);
		GridData gd = new GridData();
		gd.horizontalAlignment = SWT.FILL;
		gd.verticalAlignment = SWT.FILL;
		gd.grabExcessHorizontalSpace = true;
		gd.grabExcessVerticalSpace = true;
		_listGroup.setLayoutData(gd);
		GridLayout gl = new GridLayout();
		gl.numColumns = 1;
		_listGroup.setLayout(gl);
		_listGroup.setText("Sources List");
		
		_sourcesList = new List(_listGroup,SWT.BORDER);
		gd = new GridData();
		gd.verticalAlignment = SWT.FILL;
		gd.horizontalAlignment = SWT.FILL;
		gd.grabExcessVerticalSpace = true;
		gd.grabExcessHorizontalSpace = true;
		_sourcesList.setLayoutData(gd);
		
		_sourcesButtonsComp = new Composite(_sourcesComp,SWT.NONE);
		layout = new GridLayout();
		layout.numColumns = 2;
		_sourcesButtonsComp.setLayout(layout);
		
		_addSourceButton = new Button(_sourcesButtonsComp, SWT.None);
		_addSourceButton.setText("Add");
		_addSourceButton.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_ADD));
		
		_deleteSourceButton = new Button(_sourcesButtonsComp, SWT.None);
		_deleteSourceButton.setText("Delete");
		_deleteSourceButton.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_ETOOL_DELETE));
		
		/* Please change this in the future when more SOURCES will be available */
		_addSourceButton.setEnabled(false);
		_deleteSourceButton.setEnabled(false);
		_sourcesList.addSelectionListener(new SelectionListener() {
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
			public void widgetSelected(SelectionEvent e) {
				String source = _sourcesList.getSelection()[0];
				Control c = _compInitial.getChildren()[0];
				if( c instanceof Label ){
					c.dispose();
					_group.setVisible(true);
					_group.layout();
				}
				fillSource(source);
				_compInitial.layout();
			}
		});
		
		_addSourceButton.addListener(SWT.Selection, new Listener() {
			public void handleEvent(Event event) {
				InputDialog dialog = new InputDialog(SourcesView.this.getViewSite().getShell(),
				                         "New source",
				                         "Enter the source name",
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
					Source newSource = new Source();
					newSource.setSourceId(dialog.getValue());
					try {
						_sourceManager.addSource(newSource);
					} catch (IllegalOperationException e) {
						ErrorDialog error = new ErrorDialog(SourcesView.this.getViewSite().getShell(),
											"Source already exist",
											"The source "+dialog.getValue()+" already exists in the current configuration",
											new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.getMessage()),
											IStatus.ERROR);
						error.setBlockOnOpen(true);
						error.open();
						return;
					}
					refreshContents();
					if ( _sourcesList.getItems().length != 0 ){
						int lenght = _sourcesList.getItems().length;
						lenght -= 1;
						_sourcesList.select(lenght);
						_descriptionText.setText(_sourcesList.getItem(lenght).toString());					
					}
				}
			}
		});

		_deleteSourceButton.addListener(SWT.Selection, new Listener() {
			public void handleEvent(Event event) {
				boolean choice = MessageDialog.openQuestion( 
		        		  SourcesView.this.getViewSite().getShell(),
		            "Confirmation",
		            "Are you sure you want to delete this Source");
		        if ( choice == true ){
				String tmp[] = _sourcesList.getSelection();
				if( tmp.length == 0 ) {
					MessageBox box = new MessageBox(getViewSite().getShell(),
							SWT.OK | SWT.ICON_ERROR | SWT.APPLICATION_MODAL);
					box.setText("Empty selection");
					box.setMessage("There are no sources selected to be deleted");
					box.open();
					return;
				}
				String source = tmp[0];
				try {
					_sourceManager.deleteSource(_sourceManager.getSource(source));
				} catch (IllegalOperationException e) {
					ErrorDialog error = new ErrorDialog(SourcesView.this.getViewSite().getShell(),
							"Cannot delete source",
							"The source cannot be deleted",
							new Status(IStatus.ERROR,"cl.utfsm.acs.acg",e.getMessage()),
							IStatus.ERROR);
					error.setBlockOnOpen(true);
					error.open();
				}
				refreshContents();
				if ( _sourcesList.getItems().length != 0 ){
					int lenght = _sourcesList.getItems().length;
					lenght -= 1;
					_sourcesList.select(lenght);
					_sourceNameText.setText(_sourcesList.getItem(lenght).toString());					
				}
			}
		}
		});

		/* Right pane */
		_compInitial = new Composite(_sash, SWT.NONE);
		_compInitial.setLayout(new GridLayout());
		new Label(_compInitial, SWT.NONE).setText("Select a source");

		layout = new GridLayout();
		layout.numColumns = 2;
		gd = new GridData();
		gd.horizontalAlignment = SWT.FILL;
		gd.verticalAlignment = SWT.FILL;
		gd.grabExcessHorizontalSpace = true;
		gd.grabExcessVerticalSpace = true;
		_group = new Group(_compInitial, SWT.SHADOW_ETCHED_IN);
		_group.setText("Source information");
		_group.setLayout(layout);
		_group.setLayoutData(gd);

		_sourceNameLabel = new Label(_group,SWT.NONE);
		_sourceNameLabel.setText("Source name");
		_sourceNameText  = new Text(_group,SWT.BORDER);

		_descriptionLabel = new Label(_group,SWT.NONE);
		_descriptionLabel.setText("Description");
		_descriptionText  = new Text(_group,SWT.BORDER);
				
		gd = new GridData();
		gd.horizontalAlignment = SWT.FILL;
		gd.grabExcessHorizontalSpace = true;
		
		_sourceNameText.setLayoutData(gd);
		_descriptionText.setLayoutData(gd);
		
		_group.setVisible(false);
		_sash.setWeights(new int[] {3, 5});

		/* TODO: This is temporal, since there is currently only
		 * one source defined in the AS, and it's hardcoded */
		//setEnabled(false);
		
		_descriptionText.addListener(SWT.Modify , new Listener() {
			public void handleEvent(Event e) {
					updateSource();
				}
			});
		
		_errorMessageLabel = new Label(_group,SWT.NONE);
		_errorMessageLabel.setText("");
		_errorMessageLabel.setForeground(getViewSite().getShell().getDisplay().getSystemColor(SWT.COLOR_RED));
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		gd.horizontalSpan = 2;
		_errorMessageLabel.setLayoutData(gd);
		
		/* Please change this in the future when more SOURCES will be available */
		_sourceNameText.setEnabled(false);
		_descriptionText.setEnabled(false);
	}
	
    public void updateSource(){
    	Source[] sources = _sourceManager.getAllSources();
		String tmp[] = _sourcesList.getSelection();
		
		for (int i = 0; i < sources.length; i++) {
			if (sources[i].getName().compareTo(tmp[0].toString()) == 0){
					sources[i].setSourceId(_sourceNameText.getText());
			}
		}
		
		refreshContents();
		for (int i = 0; i < sources.length ; i++) {
			if (sources[i].getName().compareTo(_sourceNameText.getText()) == 0){
				_sourcesList.setSelection(i);
			}
		}
		//_sourceManager.saveToCDB();
    }
	
	
	@Override
	public void setFocus() {
	}

	public void setEnabled(boolean v) {
		/* Left side widgets */
		_sourcesList.setEnabled(v);
		_sourcesButtonsComp.setEnabled(v);
		//Commented widgets should be uncommented when multiple Sources are available.
		//_addSourceButton.setEnabled(v);
		//_deleteSourceButton.setEnabled(v);

		/* Source information */
		//_sourceNameText.setEnabled(v);
	}

	private void fillSource(String name) {
		Source[] sources =_sourceManager.getAllSources();
		Source source = null;
		for (int i = 0; i < sources.length; i++) {
			if(sources[i].getDescription().compareTo(name) == 0)
				source = sources[i];
		}
		// This shouldn't happen anyways...
		if (source == null)
			return;
		// sourceName is the NC name TODO:
		_sourceNameText.setText(source.getName());
		// description is the name of the source
		_descriptionText.setText(source.getDescription());
	}

	public void fillWidgets() {
		// TODO Auto-generated method stub
		String[] tmp = _sourcesList.getSelection();
		if(tmp == null || tmp.length == 0)
			return;
		fillSource(tmp[0]);
	}
	
	
	public void setReadOnly(boolean v) {
		_sourcesButtonsComp.setEnabled(!v);
		_sourceNameText.setEnabled(!v);
		_descriptionText.setEnabled(!v);
	}
}
