package si.ijs.acs.objectexplorer;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JFrame;

import si.ijs.acs.objectexplorer.engine.Introspectable;
import si.ijs.acs.objectexplorer.engine.NonStickyComponentReleased;
import alma.acs.logging.ClientLogManager;

import com.cosylab.gui.components.r2.SmartTextPane;
/**
 * This bean is used for notifying the user about events that occur
 * during runtime - debug, error, message, structural access,...
 *
 * Creation date: (2.11.2000 0:51:02)
 * @author: Miha Kadunc
 */
public class NotificationBean {
  SmartTextPane textArea=null;
  JFrame parent=null;
  boolean showError=true;
  AccessDestroyWindow accessDestroyWindow=null;
  ErrorDialog errorDialog=null;
  private static boolean debugToConsole = true;
  private static boolean confirmationDialog = false;
  private Logger consoleLogger;
 /**
 * NotificationBean constructor comment.
 */
public NotificationBean() {
	super();

	/* console only logger */
	consoleLogger = ClientLogManager.getAcsLogManager().getLoggerForApplication("objexp-console", false);
}
/**
 * Insert the method's description here.
 * Creation date: (11/13/00 7:19:17 PM)
 * @return javax.swing.JFrame
 */
public javax.swing.JFrame getParent() {
	return parent;
}
/**
 * Insert the method's description here.
 * Creation date: (11/10/00 6:55:59 PM)
 * @return javax.swing.JTextArea
 */
public SmartTextPane getTextArea() {
	return textArea;
}
/**
 * Insert the method's description here.
 * Creation date: (3/16/2001 10:34:10 AM)
 * @return boolean
 */
public boolean isDebugToConsole() {
	return debugToConsole;
}
public boolean isConfirmationDialog() {
	return confirmationDialog;
}
/**
 * Insert the method's description here.
 * Creation date: (2.11.2000 0:51:52)
 * @param message java.lang.String
 */
public void reportDebug(String location, String message) {
 if (debugToConsole) consoleLogger.info(message);
}
/**
 * Insert the method's description here.
 * Creation date: (2.11.2000 0:51:38)
 * @param error java.lang.String
 */
public void reportError(String error) {
  reportError(error,null);
}
/**
 * Insert the method's description here.
 * Creation date: (2.11.2000 1:13:45)
 * @param message java.lang.String
 * @param t java.lang.Throwable
 */
public void reportError(String message, Throwable t) {
  reportError(message, t, true);
}
/**
 * Insert the method's description here.
 * Creation date: (2.11.2000 1:13:45)
 * @param message java.lang.String
 * @param t java.lang.Throwable
 */
public void reportError(String message, Throwable t, boolean dialog) {
	reportError(message, t, dialog, true);
}
/**
 * Insert the method's description here.
 * Creation date: (2.11.2000 1:13:45)
 * @param message java.lang.String
 * @param t java.lang.Throwable
 */
public void reportError(String message, Throwable t, boolean dialog, boolean stackTrace) {
   textArea.append("Error: " + message);
   if (stackTrace) textArea.append(" (See console for exception stack trace.)");
   textArea.append("\n");

   if (t != null) {
	   if (!(t instanceof NonStickyComponentReleased))
	   {
	   		if (stackTrace)
	   			consoleLogger.log(Level.WARNING, message, t);
	   		else
	   			consoleLogger.warning(message);
	   }
	   message=message+"\n"+t;
   }
   else
   	consoleLogger.warning(message);
   
   if (dialog && showError) {
	   if (errorDialog==null) {
		   errorDialog=new ErrorDialog(parent,parent.getTitle()+" error",message);
		   errorDialog=null;
	   }
	   else {
			errorDialog.addError(message);
		}
   }
   else if (accessDestroyWindow!=null) accessDestroyWindow.addError();
}
/**
 * Insert the method's description here.
 * Creation date: (2.11.2000 1:04:53)
 * @param message java.lang.String
 */
public synchronized void reportMessage(String message) {
	textArea.append("Message: " + message + "\n");
}
/**
 * Insert the method's description here.
 * Creation date: (2.11.2000 0:55:16)
 */
public void reportStructuralAccess(short accessType, Introspectable target, String remark) {
	textArea.append("StructuralAccess: "+ target+ " - " + remark + " (type "+accessType+")\n");
	textArea.setCaretPosition(textArea.getText().length());
}
/**
 * Insert the method's description here.
 * Creation date: (30.6.2001 0:57:23)
 */
public void setAccessDestroyWindow(AccessDestroyWindow adw) {
  accessDestroyWindow=adw;	
}
/**
 * Insert the method's description here.
 * Creation date: (3/16/2001 10:34:10 AM)
 * @param newDebugToConsole boolean
 */
public void setDebugToConsole() {
	debugToConsole = !debugToConsole;
}
/**
 * Insert the method's description here.
 * Creation date: (3/16/2001 10:34:10 AM)
 * @param newDebugToConsole boolean
 */
public void setConfirmationDialog(boolean b) {
	confirmationDialog = b;
}
/**
 * Insert the method's description here.
 * Creation date: (11/13/00 7:19:17 PM)
 * @param newParent javax.swing.JFrame
 */
public void setParent(javax.swing.JFrame newParent) {
	parent = newParent;
}
/**
 * Insert the method's description here.
 * Creation date: (29.6.2001 23:31:31)
 * @param showError boolean
 */
public void setShowError(boolean showError) {
  this.showError=showError;
}
/**
 * Insert the method's description here.
 * Creation date: (11/10/00 6:55:59 PM)
 * @param newTextArea javax.swing.JTextArea
 */
public void setTextArea(SmartTextPane newTextArea) {
	textArea = newTextArea;
}
}
