package cern.gp.beans.editors;

import java.awt.Color;
import java.awt.Component;
import java.beans.FeatureDescriptor;
import java.beans.PropertyEditorSupport;
import java.lang.reflect.InvocationTargetException;

import javax.swing.JCheckBox;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.openide.explorer.propertysheet.ExPropertyEditor;
import org.openide.explorer.propertysheet.PropertyEnv;
import org.openide.explorer.propertysheet.editors.EnhancedPropertyEditor;
import org.openide.nodes.Node;
import org.openide.nodes.Node.Property;

/**
 * An editor to place a JCheckBox into a cell of an Explorer. This is to be asociated
 * with a boolean property.
 * 
 * @author Vito Baggiolini
 *
 * @version $Revision: 1.2 $, $Date: 2006/09/25 08:52:36 $, $Author: acaproni $
 */
public class CheckBoxEditor extends PropertyEditorSupport implements EnhancedPropertyEditor, ExPropertyEditor {
  private boolean editableMode;

  private PropertyEnv propertyEnv;

  private final JCheckBox renderer;

  private JCheckBox editor;

  /**
   * default constructor, opens the Editor already in Editable mode, i.e. the first
   *  click on the editor changes the value already
   */
  public CheckBoxEditor() {
    this(true);
  }

  /**
   * @param eitableMode if true, the editor is active on the first click, if not,
   * the user has to click first to be able to change the checkbox.
   */
  public CheckBoxEditor(boolean editableMode) {
    //TODO see if this can be done with PropertyPanel.PREF_INPUT_STATE
    this.editableMode = editableMode;
    renderer = new JCheckBox();
    renderer.setOpaque(false);
  }

  /*
   *  (non-Javadoc)
   * @see java.beans.PropertyEditor#isPaintable()
   */
  public boolean isPaintable() {
    return true;
  }

  /*
   *  (non-Javadoc)
   * @see java.beans.PropertyEditor#paintValue(java.awt.Graphics, java.awt.Rectangle)
   */
  public void paintValue(java.awt.Graphics gfx, java.awt.Rectangle box) {
    gfx.translate(box.x, box.y);
    boolean b = getBoolValue();
    renderer.setSelected(b);
    renderer.setSize(box.width, box.height);
    renderer.paint(gfx);
    gfx.translate(-box.x, -box.y);
  }

  /*
   *  (non-Javadoc)
   * @see org.openide.explorer.propertysheet.editors.EnhancedPropertyEditor#getInPlaceCustomEditor()
   */
  public Component getInPlaceCustomEditor() {
    if (editor == null) {
      editor = new JCheckBox();
      editor.setOpaque(false);
      editor.addChangeListener(new ChangeListener() {
        public void stateChanged(ChangeEvent e) {
          setBoolValue(editor.isSelected());
        }
      });
    }
    if (!editableMode) {

      editor.setSelected(getBoolValue());
    } else {
      // immediately invert the selection. This is what the user expects, because he does not want to click twice on the
      // checkbox. We want to avoid the normal behavior of the program, that requires you to click once to activate 
      // editing mode and once to change the value.
      boolean invBoolValue = !getBoolValue();
      editor.setSelected(invBoolValue);
      Boolean invBooleanObject = invBoolValue ? Boolean.TRUE : Boolean.FALSE;
      // we have to go directly to the Node.Property because this object (the PropertyEditor) 
      // does not propagate a setValue(invBoolValue). It gets blocked in the propertyChange() method of 
      // the private class PropertyPanel.EditorListener.
      // this is less of a Hack than it looks like, c.f. Javadoc of 
      // {@link org.openide.explorer.propertysheet.PropertyEnv.getFeatureDescriptor()}
      FeatureDescriptor fd = propertyEnv.getFeatureDescriptor();
      if (fd instanceof Node.Property) {
        try {
        setNodeProperty(invBoolValue, (Property) fd);
        } catch (IllegalArgumentException e) {
          // TODO Auto-generated catch block
          e.printStackTrace();
        } catch (IllegalAccessException e) {
          // TODO Auto-generated catch block
          e.printStackTrace();
        } catch (InvocationTargetException e) {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
      } else {
          System.out.println("fd not Node.Property, but " + fd.getClass()); 
      }
      // nevertheless call this.setValue() to be coherent
      setBoolValue(invBoolValue);
    }
    return editor;
  }

  protected void setNodeProperty(boolean newValue, Node.Property fd)
    throws IllegalAccessException, InvocationTargetException {
      fd.setValue(newValue ? Boolean.TRUE : Boolean.FALSE);
  }

  /*
   *  (non-Javadoc)
   * @see org.openide.explorer.propertysheet.editors.EnhancedPropertyEditor#hasInPlaceCustomEditor()
   */
  public boolean hasInPlaceCustomEditor() {
    return true;
  }
  
  /*
   *  (non-Javadoc)
   * @see org.openide.explorer.propertysheet.editors.EnhancedPropertyEditor#supportsEditingTaggedValues()
   */
  public boolean supportsEditingTaggedValues() {
    return false;
  }

  /**
   * Returns value of the property as boolean
   *
   * @return value of the property
   */
  protected boolean getBoolValue() {
    return ((Boolean) getValue()).booleanValue();
  }
  
  protected void setBoolValue(boolean newValue) {
      setValue(newValue? Boolean.TRUE : Boolean.FALSE);
  }

  /* (non-Javadoc)
   * @see org.openide.explorer.propertysheet.ExPropertyEditor#attachEnv(org.openide.explorer.propertysheet.PropertyEnv)
   */
  public void attachEnv(PropertyEnv env) {
    propertyEnv = env;
  }

  public void setBackground(Color color) {
    if (color != null) {
      renderer.setOpaque(true);
      renderer.setBackground(color);
    } else {
      renderer.setOpaque(false);
    }
  }
  
}
