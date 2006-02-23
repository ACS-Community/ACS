package si.ijs.acs.objectexplorer.engine.BACI;

import javax.swing.*;
import java.util.*;
import java.awt.event.*;

/**
 * Insert the type's description here.
 * Creation date: (6/29/2001 10:47:27 AM)
 * @author: 
 */
public class BACIMenu extends javax.swing.JMenu {
	private JMenuItem corbalocItem = null;
	private JCheckBoxMenuItem cacheItem = null;
	private BACIRemoteAccess ra = null;
	private Properties props = null;
	private String managerLoc = null;
	private String IRloc = null;
/**
 * BACIMenu constructor comment.
 */
public BACIMenu(BACIRemoteAccess ra) {
	super();
	if (ra == null) throw new NullPointerException("ra");
	this.ra = ra;
	setText("BACI Engine");
	
	corbalocItem = new JMenuItem("Manager & IR corbaloc");
	cacheItem = new JCheckBoxMenuItem("Cache IR descriptions");

	add(corbalocItem);
	add(cacheItem);
	
	props = System.getProperties();
	
	managerLoc = props.getProperty(BACIRemoteAccess.MANAGER_CORBALOC);
	IRloc = props.getProperty(BACIRemoteAccess.IR_CORBALOC);
	
	cacheItem.setSelected(this.ra.getCaching());
	
	corbalocItem.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent evt)
		{
			CorbalocDialog dialog = new CorbalocDialog();
			dialog.setLocationRelativeTo(BACIMenu.this);
			if (managerLoc != null) dialog.setManagerFieldText(managerLoc);
			if (IRloc != null) dialog.setRepositoryFieldText(IRloc);
			dialog.show();
			if (dialog.isOKed())
			{
				managerLoc = dialog.getManagerFieldText();
				IRloc = dialog.getRepositoryFieldText();
				props.setProperty(BACIRemoteAccess.MANAGER_CORBALOC, managerLoc);
				props.setProperty(BACIRemoteAccess.IR_CORBALOC, IRloc);
			}
		}
	});

	cacheItem.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent evt)
		{
			if (cacheItem.isSelected())
			{
				BACIMenu.this.ra.setCaching(true);
			}
			else
			{
				BACIMenu.this.ra.setCaching(false);
			}
		}
	});
}
}
