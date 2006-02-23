package cern.gp.explorer.properties;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.InvocationTargetException;

import org.openide.nodes.Node;

import cern.gp.nodes.GPNode;

/**
 * A PropertyField with a model that does not immediately propagate the changed property value
 * to the bean but allows the user to commit or revert the changes.
 * 
 * @see ReversibleModel, PropertyDialog
 */
public class ReversiblePropertyField extends PropertyField {
  // TODO handle property change updates   
  private final ReversibleModel model;
  /**
   * Constructor that creates a PropertyField from a node and a property Name, with preferences
   * @param model 
   * @param preferences one or more of {@link #PREF_CUSTOM_EDITOR}, {@link #PREF_INPUT_STATE}, 
   *         or {@link #PREF_READ_ONLY}
   */
  public ReversiblePropertyField(ReversibleModel model, Object bean, String propName, int preferences) {
    super(model, bean, propName, preferences);
    this.model = model;
  }

  public ReversiblePropertyField(GPNode node, String propName, int preferences) {
    this(
      new ReversibleModel(
        PropertyField.getNodeProperty(node.getPeerNode().getPropertySets(), propName),
        new Object[] { node.getBean() }),
        node.getBean(),
        propName,
        preferences);
  }
  public void commit() throws InvocationTargetException {
    model.commit();
  }

  public void revert() {
    model.revert();
  }

  /**
   * A model implementation that keeps a copy of the value set with {@link #setValue(Object)} 
   * locally and only sends it to the underlying bean when the an {@link ActionEvent} with "OK"
   * has been received or the {@link #commit()} method has been called directly.
   * If the ActionEvent is something else than "OK", or if the {@link #revert()} method is called,
   * the change is abandoned.
   */
  static class ReversibleModel extends PropertyField.SimpleModel implements ActionListener {
    private static final String OK_EVENT = "OK";
    private Object tmpValue;
    /**
     * @param property
     * @param beans
     */
    public ReversibleModel(Node.Property property, Object[] beans) {
      super(property, beans);
      // TODO Auto-generated constructor stub
    }

    /* (non-Javadoc)
     * @see org.openide.explorer.propertysheet.PropertyModel#getValue()
     */
    public Object getValue() throws InvocationTargetException {
      if (tmpValue != null) {
        return tmpValue;
      } else {
        // TODO Auto-generated method stub
        return super.getValue();
      }
    }

    /* (non-Javadoc)
     * @see org.openide.explorer.propertysheet.PropertyModel#setValue(java.lang.Object)
     */
    public void setValue(Object v) throws InvocationTargetException {
      tmpValue = v;
    }

    /* (non-Javadoc)
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent e) {
      if (OK_EVENT.equals(e.getActionCommand())) { 
        try {
          commit();
        } catch (InvocationTargetException e1) {
          e1.printStackTrace();
        }
      } else {
        revert();
      }
    }

    public boolean isModified() {
      return (tmpValue != null);
    }

    public void commit() throws InvocationTargetException {
      super.setValue(tmpValue);
      tmpValue = null;
    }

    public void revert() {
      tmpValue = null;
    }
  } // end of class ReversibleModel

}