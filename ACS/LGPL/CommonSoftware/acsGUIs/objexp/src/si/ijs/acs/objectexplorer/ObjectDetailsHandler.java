package si.ijs.acs.objectexplorer;

import javax.swing.JPanel;
import si.ijs.acs.objectexplorer.engine.SimpleIntrospectable;

/**
 * Insert the type's description here.
 * Creation date: (3.2.2002 14:25:42)
 * @author: 
 */
public interface ObjectDetailsHandler {
	public JPanel getEditorPanel();
	public void setNodeRequestListener(NodeRequestListener listener);
	public void setNotifier(NotificationBean notifier);
	public void setObject(SimpleIntrospectable object);
	public void setReporter(ReporterBean reporter);
}
