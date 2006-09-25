package cern.gp.explorer.properties;
import java.beans.FeatureDescriptor;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.openide.explorer.propertysheet.ExPropertyModel;
import org.openide.explorer.propertysheet.PropertyModel;
import org.openide.explorer.propertysheet.PropertyPanel;
import org.openide.nodes.Node;
import org.openide.util.WeakListener;

import cern.gp.beans.BeanSupport;
import cern.gp.nodes.GPNode;
import cern.gp.util.Assertion;

/**
 * A GUI field useful to edit one property which can be embedded in another GUI
 * This is the same as used in the GP Explorers and in the Property Sheet opened by the
 * {@link cern.gp.actions.PropertiesAction}, and therefore has the same behavior and
 * representation as those property panels.
 * 
 * It is based on {@link org.openide.explorer.propertysheet.PropertyPanel}
 * 
 * @author Vito Baggiolini
 *
 * @version $Revision: 1.2 $, $Date: 2006/09/25 08:52:36 $, $Author: acaproni $
 */
public class PropertyField extends PropertyPanel {
  /**
   * Constant defining preferences in displaying of value.
   * Value should be displayed in read-only mode.
   */
  public static final int PREF_READ_ONLY = 0x0001;

  /**
   * Constant defining preferences in displaying of value.
   * Value should be displayed in custom editor.
   */
  public static final int PREF_CUSTOM_EDITOR = 0x0002;

  /**
   * Constant defining preferences in displaying of value.
   * Value should be displayed in editor only.
   */
  public static final int PREF_INPUT_STATE = 0x0004;

  /**
   * Constructor that creates a PropertyField from a node and a property Name, 
   * @param node the GPNode to which the property belongs
   * @param propName the property name of this field 
   */
  public PropertyField(GPNode node, String propName) {
    this(node, propName, PREF_INPUT_STATE);
  }

  /**
   * Constructor that creates a PropertyField from a node and a property Name, with preferences
   * @param node the GPNode to which the property belongs
   * @param propName the property name of this field 
   * @param preferences one or more of {@link #PREF_CUSTOM_EDITOR}, {@link #PREF_INPUT_STATE}, 
   *         or {@link #PREF_READ_ONLY}
   */
  public PropertyField(GPNode node, final String propName, int preferences) {
    this(
      (PropertyModel) new SimpleModel(getNodeProperty(node.getPeerNode().getPropertySets(), propName),
        new Object[] { node.getBean()}),
      node.getBean(),
      propName,
      preferences);
  }
  /**
   * @param model
   * @param preferences
   */
  protected PropertyField(PropertyModel model, Object bean, String propName, int preferences) {
    super(model, preferences);
    BeanPropertyChangeListener bpcl;
    if (bean instanceof BeanSupport) {
      BeanSupport beanPropertySource = (BeanSupport) bean;
      bpcl = new BeanPropertyChangeListener(beanPropertySource, propName);
    } else {
      bpcl = new BeanPropertyChangeListener(bean, propName);
    }
    bpcl.registerListener();
  }

  /**
   * helper method, finds the Node.Property for the indicated propName
   * @param psArr the Node.Property[] normally used to create a Property Sheet for a node
   * @param propName the name of the property requested
   * @return the Node.Property for the property propName
   * @throws IllegalArgumentException if the property is not found
   */
  protected static Node.Property getNodeProperty(Node.PropertySet[] psArr, String propName) {
    Node.Property[] npArr;
    for (int jx = 0; jx < psArr.length; jx++) {
      npArr = psArr[jx].getProperties();
      for (int ix = 0; ix < npArr.length; ix++) {
        if (npArr[ix].getName().equals(propName)) {
          return npArr[ix];
        }
      }
    }
    throw new IllegalArgumentException("unknown property: " + propName);
  }

  /**
   * helper class that updates the PropertyField when the value gets updated in the
   * Bean while the PropertyField is open
   * It can handle both Beans that do and those that don't inherit from BeanSupport. 
   */
  protected final class BeanPropertyChangeListener implements PropertyChangeListener {
    private final Method addPclMethod;
    private final Method remPclMethod;
    private final String propName;
    private BeanSupport source;
    /**
     * Constructor
     * @param source the bean to which this property belongs 
     * @param propName the name of the property
     */
    protected BeanPropertyChangeListener(BeanSupport source, String propName) {
      super();
      this.propName = propName;
      this.source = source;
      this.addPclMethod = null;
      this.remPclMethod = null;
    }

    protected BeanPropertyChangeListener(Object source, String propName) {
      this.propName = propName;
      // these temporary variables are needed to make *PclMethod final
      Method add = null;
      Method rem = null;

      try {
        add =
          source.getClass().getMethod(
            "addPropertyChangeListener",
            new Class[] { String.class, java.beans.PropertyChangeListener.class });
        rem =
          source.getClass().getMethod(
            "removePropertyChangeListener",
            new Class[] { String.class, java.beans.PropertyChangeListener.class });
      } catch (Exception e) {
        e.printStackTrace();
      }

      addPclMethod = add;
      remPclMethod = rem;
    }
    /**
     * add a property change listener to the Bean 
     */
    public void registerListener() {
      Assertion.assertTrue(source != null, "source != null. Maybe you called this twice?");
      PropertyChangeListener weak = WeakListener.propertyChange(this, source);
      if (addPclMethod != null) {
        try {
          addPclMethod.invoke(source, new Object[] { propName, weak });
        } catch (Exception e) {
          e.printStackTrace();
        }
      } else {
        source.addPropertyChangeListener(propName, this);
      }
      source = null;
    }

    /*
     * Handle a properyt change coming from the Bean
     *  (non-Javadoc)
     * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
     */
    public void propertyChange(PropertyChangeEvent evt) {
      String pn = evt.getPropertyName();
      if (!propName.equals(pn)) {
        System.err.println("unexpected propName: " + pn);
        return;
      }
      Object newValue = evt.getNewValue();
      if (newValue == null) {
        System.err.println("Null newValue not yet treated for prop " + pn);
      }
      try {
        PropertyField.super.getModel().setValue(newValue);
        PropertyField.super.repaint();
      } catch (InvocationTargetException e) {
        e.printStackTrace();
      }
    }
  }

  /** this is copied from NetBeans {@link org.openide.explorer.propertysheet.PropertyPanel} */
  /** Implementation of the <code>PropertyModel</code> interface keeping 
   * a <code>Node.Property</code>. */
  static class SimpleModel implements ExPropertyModel {
    /** Property to work with. */
    private Node.Property prop;
    /** Array of beans(nodes) to which belong the property. */
    private Object[] beans;
    /** Property change support. */
    private PropertyChangeSupport sup = new PropertyChangeSupport(this);

    /** Construct simple model instance.
     * @param property proeprty to work with
     * @param beans array of beans(nodes) to which belong the property */
    public SimpleModel(Node.Property property, Object[] beans) {
      this.prop = property;
      this.beans = beans;
    }

    /** Implements <code>PropertyModel</code> interface. */
    public Object getValue() throws InvocationTargetException {
      try {
        return prop.getValue();
      } catch (IllegalAccessException iae) {
        throw annotateException(iae);
      } catch (InvocationTargetException ite) {
        throw annotateException(ite);
      }
    }

    /** Implements <code>PropertyModel</code> interface. */
    public void setValue(Object v) throws InvocationTargetException {
      try {
        prop.setValue(v);
        sup.firePropertyChange(PropertyModel.PROP_VALUE, null, null);
      } catch (IllegalAccessException iae) {
        throw annotateException(iae);
      } catch (IllegalArgumentException iaae) {
        throw annotateException(iaae);
      } catch (InvocationTargetException ite) {
        throw annotateException(ite);
      }
    }

    /** Annotates specified exception. Helper method.
     * @param exception original exception to annotate
     * @return <code>IvocationTargetException</code> which annotates the
     *       original exception */
    private InvocationTargetException annotateException(Exception exception) {
      if (exception instanceof InvocationTargetException) {
        return (InvocationTargetException) exception;
      } else {
        return new InvocationTargetException(exception);
      }
    }

    /** Implements <code>PropertyModel</code> interface. */
    public Class getPropertyType() {
      return prop.getValueType();
    }

    /** Implements <code>PropertyModel</code> interface. */
    public Class getPropertyEditorClass() {
      Object ed = prop.getPropertyEditor();
      if (ed != null) {
        return ed.getClass();
      }
      return null;
    }

    /** Implements <code>PropertyModel</code> interface. */
    public void addPropertyChangeListener(PropertyChangeListener l) {
      Thread.dumpStack();
      sup.addPropertyChangeListener(l);
    }

    /** Implements <code>PropertyModel</code> interface. */
    public void removePropertyChangeListener(PropertyChangeListener l) {
      sup.removePropertyChangeListener(l);
    }

    /** Implements <code>ExPropertyModel</code> interface. */
    public Object[] getBeans() {
      return beans;
    }

    /** Implements <code>ExPropertyModel</code> interface. */
    public FeatureDescriptor getFeatureDescriptor() {
      return prop;
    }

    void fireValueChanged() {
      sup.firePropertyChange(PropertyModel.PROP_VALUE, null, null);
    }

  } // End of class SimpleModel.

}