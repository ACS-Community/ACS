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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;

import cl.utfsm.acs.acg.core.AlarmSystemManager;
import cl.utfsm.acs.acg.core.UserAuthenticator;
import cl.utfsm.acs.acg.core.UserAuthenticatorException;
import cl.utfsm.acs.acg.core.UserAuthenticator.Role;

public class ApplicationWorkbenchWindowAdvisor extends WorkbenchWindowAdvisor {

    public ApplicationWorkbenchWindowAdvisor(IWorkbenchWindowConfigurer configurer) {
        super(configurer);
    }

    public void preWindowOpen() {
        IWorkbenchWindowConfigurer configurer = getWindowConfigurer();
        configurer.setInitialSize(new Point(700, 800));
        configurer.setShowCoolBar(false);
        configurer.setShowStatusLine(true);
        configurer.setTitle("Alarms Configuration GUI");
    }

    public void postWindowOpen() {

    	final IStatusLineManager status = getWindowConfigurer().getActionBarConfigurer().getStatusLineManager();
    	status.setMessage("Application starting...");
		final Display display = getWindowConfigurer().getWindow().getShell().getDisplay();

		// Disables the initial view
		IViewReference[] views = getWindowConfigurer().getWindow().getWorkbench().getActiveWorkbenchWindow().getActivePage().getViewReferences();
		for (int i = 0; i < views.length; i++) {
			if( views[i].getId().compareTo(AlarmSystemView.ID) == 0 )
				((IMyViewPart)views[i].getView(false)).setEnabled(false);
		}

		boolean authenticated = false;

		AuthenticationDialog d = new AuthenticationDialog(ApplicationWorkbenchWindowAdvisor.this.getWindowConfigurer().getWindow().getShell());

		UserAuthenticator.Role role = null;
		while( !authenticated ) {

			d.open();
	    	UserAuthenticator userAuth = new UserAuthenticator();
	    	try {
	    		role = userAuth.authenticate(d.getUser(), d.getPassword());
	    	} catch (UserAuthenticatorException e) {
	    		d.setErrorMessage("Authentication unsuccessful");
	    		continue;
	    	} catch (IllegalArgumentException e) {
	    		d.setErrorMessage("Please authenticate yourselve");
	    		continue;
	    	} finally {
	    		status.setMessage("Authentication successful");
	    	}
	    	authenticated = true;
		}

		final UserAuthenticator.Role finalRole = role;

		new Thread( new Runnable() {

			@Override
			public void run() {
		    	AlarmSystemManager asm = AlarmSystemManager.getInstance(finalRole);
				try {
					display.asyncExec(new Runnable() {
						public void run() { status.setMessage("Connecting to Manager"); }
					});
					asm.connectToManager();
					display.asyncExec(new Runnable() {
						public void run() { status.setMessage("Connecting to CDB DAL"); }
					});
					asm.connectToDAL();
					display.asyncExec(new Runnable() {
						public void run() { status.setMessage("Loading contents from the CDB"); }
					});
					asm.loadFromCDB();
					final String error = asm.checkCDB();
					if(error.compareTo("") != 0) {
						display.asyncExec(new Runnable() {
							public void run() {
								ErrorDialog edialog = new ErrorDialog(getWindowConfigurer().getWindow().getShell(),
									"CDB Error",
									"Error while checking CDB integrity",
									new Status(IStatus.ERROR,"cl.utfsm.acs.acg",error),
									IStatus.ERROR);
								edialog.setBlockOnOpen(true);
								edialog.open();
							}
						});
					}
				} catch (Exception e) {
					e.printStackTrace();
					display.asyncExec(new Runnable() {
						public void run() { status.setErrorMessage("Couldn't successfully connect to AS configuation"); }
					});
					return;
				}

				/*  If everything went OK:
				     * Show the other views
				     *  Enable the widgets and inform the user */
				display.asyncExec(new Runnable() {
					public void run() {
						IWorkbenchPage page = getWindowConfigurer().getWindow().getActivePage();
						try {
							if( finalRole == Role.Administrator || finalRole == Role.Operator) {
								page.showView(SourcesView.ID,null,IWorkbenchPage.VIEW_VISIBLE);
								page.showView(CategoriesView.ID,null,IWorkbenchPage.VIEW_VISIBLE);
								page.showView(AlarmsView.ID,null,IWorkbenchPage.VIEW_VISIBLE);
								page.showView(ReductionsView.ID,null,IWorkbenchPage.VIEW_VISIBLE);
								page.showView("org.eclipse.pde.runtime.LogView",null,IWorkbenchPage.VIEW_VISIBLE);
							}
						} catch (PartInitException e) {
							status.setErrorMessage("Cannot open other views");
						}

						IViewReference[] views = page.getViewReferences();
						for (int i = 0; i < views.length; i++) {
							if( views[i].getId().compareTo(AlarmSystemView.ID) == 0 )
								((IMyViewPart)views[i].getView(false)).setEnabled(true);
							if(finalRole == Role.Operator)
								if(views[i].getView(false) instanceof IMyViewPart)
								((IMyViewPart)views[i].getView(false)).setReadOnly(true);
						}
						status.setMessage("Application started successfully");
					}
				});
				
			}
    		
		}).start();

    }

    private class AuthenticationDialog extends Dialog {

    	/* Input widgets */
    	private Text _userText;
    	private Text _passText;
    	private Label _errorLabel;
    	private IInputValidator _validator;

    	/* Saved values */
    	private String _user;
    	private String _password;
    	private String _error;

		protected AuthenticationDialog(Shell parentShell) {
			super(parentShell);
			setShellStyle( getShellStyle() | SWT.RESIZE );
		}

		public void setErrorMessage(String error) {
			_error = error;
		}

		protected Control createDialogArea(Composite parent) {
			Composite container = (Composite)super.createDialogArea(parent);
			GridLayout gl = new GridLayout();
			gl.numColumns = 3;
			container.setLayout(gl);

			Label infoLabel = new Label(container, SWT.NONE);
			infoLabel.setText("Please provide your username and password"+
					" for the ACS Alarm System");
			GridData gd = new GridData(GridData.FILL,GridData.CENTER,false,false,3,1);
			gd.horizontalIndent = 5;
			gd.verticalIndent = 5;
			infoLabel.setLayoutData(gd);
			
			Label userImageLabel = new Label(container, SWT.NONE);
			userImageLabel.setImage(Activator.getDefault().getImageRegistry().get(Activator.IMG_FAULTMEMBERS));
			gd = new GridData(GridData.END,GridData.CENTER,false,false);
			gd.horizontalIndent = 20;
			gd.verticalIndent = 5;
			userImageLabel.setLayoutData(gd);

			Label userLabel = new Label(container, SWT.NONE);
			userLabel.setText("Username");
			userLabel.setLayoutData(new GridData(GridData.END,GridData.CENTER,false,false));

			_userText = new Text(container, SWT.BORDER | SWT.SINGLE);
			if( _user != null )
				_userText.setText(_user);
			_userText.setLayoutData(new GridData(GridData.FILL,GridData.CENTER,true,false));
			_userText.addModifyListener(new ModifyListener() {
				public void modifyText(ModifyEvent e) {
					validateInput();
				}
			});

			Label passImageLabel = new Label(container, SWT.NONE);
			passImageLabel.setImage(Activator.getDefault().getImageRegistry().get(Activator.IMG_LINK));
			gd = new GridData(GridData.END,GridData.CENTER,false,false);
			gd.horizontalIndent = 20;
			passImageLabel.setLayoutData(gd);

			Label passLabel = new Label(container, SWT.NONE);
			passLabel.setText("Password");
			passLabel.setLayoutData(new GridData(GridData.END,GridData.CENTER,false,false));

			_passText = new Text(container, SWT.BORDER | SWT.SINGLE | SWT.PASSWORD );
			if( _password != null )
				_passText.setText(_password);
			_passText.setLayoutData(new GridData(GridData.FILL,GridData.CENTER,true,false));
			_passText.addModifyListener(new ModifyListener() {
				public void modifyText(ModifyEvent e) {
					validateInput();
				}
			});

			_errorLabel = new Label(container, SWT.NONE);
			if( _error != null ) {
				_errorLabel.setText(_error);
			}
			gd = new GridData(GridData.FILL,GridData.CENTER,false,false,3,1);
			gd.horizontalIndent = 5;
			gd.verticalIndent = 5;
			_errorLabel.setLayoutData(gd);

			_validator = new IInputValidator() {
				public String isValid(String newText) {
					if( newText.trim().compareTo("") == 0 )
						return "Field cannot be empty";
					return null;
				}
			};
			return container;
		}

		protected void createButtonsForButtonBar(Composite parent) {
			createButton(parent, IDialogConstants.OK_ID,
					"Authenticate", true);
			getButton(IDialogConstants.OK_ID).setEnabled(false);
		}

		protected void validateInput() {

			String errorMessage = null;

			if( _validator != null ) {
				errorMessage = _validator.isValid(_userText.getText());
				if( errorMessage == null )
					errorMessage = _validator.isValid(_passText.getText());
			}

			if( errorMessage != null )
				_errorLabel.setText(errorMessage);
			else
				_errorLabel.setText("");

			getButton(IDialogConstants.OK_ID).setEnabled(errorMessage == null);
		}

		protected void configureShell(Shell newShell) {
			super.configureShell(newShell);
			newShell.setText("Authentication");
		}

		public String getUser() {
			return _user;
		}

		public String getPassword() {
			return _password;
		}

		public void buttonPressed(int id) {

			// There is only one button, we don't have to check which it is
			_user = _userText.getText();
			_password = _passText.getText();

			super.buttonPressed(id);
		}
    }
}
