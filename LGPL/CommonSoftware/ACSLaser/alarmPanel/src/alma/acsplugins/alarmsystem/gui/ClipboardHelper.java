package alma.acsplugins.alarmsystem.gui;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;

/**
 * The class to set the clipboard content 
 *  
 * @author acaproni
 *
 */
public final class ClipboardHelper implements ClipboardOwner {
	/**
   * Empty implementation of the ClipboardOwner interface.
   */
   public void lostOwnership( Clipboard aClipboard, Transferable aContents) {
     //do nothing
   }
   
   /**
    * Place a String on the clipboard, and make this class the
    * owner of the Clipboard's contents.
    */
    public void setClipboardContents( String str){
      StringSelection stringSelection = new StringSelection(str);
      Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
      try {
      	clipboard.setContents( stringSelection, stringSelection );
      } catch (IllegalStateException e) {
      	// This exception may be returned in some cases
      	// It is a temporary situation: we do nothing here
      	// and the user will retry again or most likely 
      	// submit an SPR ;-)
      	e.printStackTrace();
      }
    }
    
    /**
     * Get the String in the clipboard.
     *
     * @return any text found on the Clipboard; if none found, return an
     * empty String.
     */
     private String getClipboardContents() {
       String result = "";
       Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
       //odd: the Object param of getContents is not currently used
       Transferable contents = clipboard.getContents(null);
       boolean hasTransferableText = (contents != null) &&
	   		contents.isDataFlavorSupported(DataFlavor.stringFlavor);
       if (hasTransferableText) {
         try {
           result = (String)contents.getTransferData(DataFlavor.stringFlavor);
         }
         catch (UnsupportedFlavorException ex){
           //highly unlikely since we are using a standard DataFlavor
           System.out.println(ex);
         }
         catch (IOException ex) {
           System.out.println(ex);
         }
       }
       return result;
     }
}