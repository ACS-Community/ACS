package cern.gp.explorer.properties;

import org.openide.DialogDescriptor;
import org.openide.nodes.Node;

import cern.gp.nodes.GPNode;
import cern.gp.util.GPManager;

/**
 * A dialog Panel for editing a property 
 * 
 * @author Vito Baggiolini
 *
 * @version $Revision: 1.2 $, $Date: 2006/09/25 08:52:36 $, $Author: acaproni $
 */
public class PropertyDialog {
  private final DialogDescriptor dd;

  /**
   * 
   * @param node the node to which the property belongs
   * @param propName the name of the property
   * @param isModal whether the Dialog shall be modal
   */
  public PropertyDialog(GPNode node, String propName, boolean isModal) {
    Node.Property np = PropertyField.getNodeProperty(node.getPeerNode().getPropertySets(), propName);
    ReversiblePropertyField.ReversibleModel mod =
      new ReversiblePropertyField.ReversibleModel(np, new Object[] { node.getBean()});
    ReversiblePropertyField rpp =
      new ReversiblePropertyField(mod, node.getBean(), propName, PropertyField.PREF_INPUT_STATE);
    dd = new DialogDescriptor(rpp, "Change " + propName, isModal, mod);
  }
  
  public void show() {
    GPManager.createDialog(dd).show();
  }

}
