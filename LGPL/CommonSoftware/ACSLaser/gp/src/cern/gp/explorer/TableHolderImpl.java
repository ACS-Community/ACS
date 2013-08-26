package cern.gp.explorer;

import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.InvocationTargetException;

import org.openide.explorer.view.NodeTableModel;
import org.openide.nodes.Node;
import org.openide.nodes.Node.Property;
import org.openide.nodes.Node.PropertySet;

import cern.gp.nodes.GPNode;
import cern.gp.nodes.NodeFactory;
import cern.gp.util.ArrayUtil;

import java.util.ArrayList;

/**
 * A class that contains common code related to the Tables contained in the
 * ListTableExplorer and TreeTableExplorer.
 * Both ListTableExplorer and TreeTableExplorer contain an instance of this class
 * and delegate to it.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Vito Baggiolini
 */
class TableHolderImpl implements TableHolder {
  private TablePropertyHolder tablePropertyHolder;

  TableHolderImpl(TablePropertyHolder tablePropertyHolder) {
    this.tablePropertyHolder = tablePropertyHolder;
  }

	/**
	 * @deprecated 
	 */
  public void setTableColumns(Class beanClass) throws IntrospectionException {
    setTableColumns(calcPropertyDescriptorsFor(beanClass, null));
  }

  /**
   * @deprecated 
   */
  public void setTableColumns(Class beanClass, String[] propNames) throws IntrospectionException {
    setTableColumns(TableHolderImpl.calcPropertyDescriptorsFor(beanClass, propNames));
  }

	 /* (non-Javadoc)
		* @see cern.gp.explorer.TableHolder#setTableColumns(java.lang.Class[], java.lang.String[])
		*/
	 public void setTableColumns(Class[] propTypes, String[] propNames) {
	 	setTableColumns(propTypes, propNames, null);
	 }

	/* non-javadoc
	 * Sets the colums of the Table. The colums are identified by their name and their type.
	 * Please make sure that the two arrays passed as arguments correspond.
	 * <Strong>Caution</strong>do not use this with beans that implement the 
	 * {@link cern.gp.beans.BeanSupport#getPropertyInfo} method to customize settings for
	 * properties. For such beans use {{#setTableColumns(Object, String[])} instead.
	 * 
	 * @param propNames the names of the properties to be displayed
	 * @param propTypes the types of the properties corresponding to the names
	 */
  public void setTableColumns(Class[] propTypes, String[] propNames, boolean[] sortable) {
    //if (NewAssert.assertTrue(propTypes.length == propNames.length, "not the same length for the two arrays") == false) { return; }
    Property[] propArr = new Property[propTypes.length];
    for (int ix = 0; ix < propArr.length; ix++) {
      propArr[ix] = new TableViewColumnProperty(propNames[ix], null, propTypes[ix]);
    }
    tablePropertyHolder.setProperties(propArr, sortable);
  }

  public void setTableColumns(Object bean) throws IntrospectionException {
    setTableColumns(bean, null);
  }

	public void setTableColumns(Object bean, String[] propNames) throws IntrospectionException {
		setTableColumns(bean, propNames, null);
	}
	
  public void setTableColumns(Object bean, String[] propNames, boolean[] sortable) throws IntrospectionException {
    // TODO check if there is a more efficient way to do this!
		setTableColumns(NodeFactory.createNode(bean), propNames, sortable);
  }
		
	/* (non-Javadoc)
	 * @see cern.gp.explorer.TableHolder#setTableColumns(cern.gp.nodes.GPNode, java.lang.String[])
	 */
	public void setTableColumns(GPNode node, String[] propNames) {
		setTableColumns(node, propNames, null);
	}
	public void setTableColumns(GPNode node, String[] propNames, boolean[] sortable) {
		// this is based on the org.openide.nodes.Node.getPropertySets() method, which is filled in by the
		// cern.gp.nodes.impl.BeanNode.createProperties();
		// get the normal and experts propertySets and concatenate them
		Node.Property[] allProperties;
    PropertySet[] propSets = node.getPeerNode().getPropertySets();
    Node.Property[] normalProperties = propSets[0].getProperties();
    if (propSets.length == 1) {
      allProperties = normalProperties;
    } else {
      Node.Property[] expertProperties = propSets[1].getProperties();
      allProperties = new Node.Property[normalProperties.length + expertProperties.length];
      System.arraycopy(normalProperties, 0, allProperties, 0, normalProperties.length);
      System.arraycopy(expertProperties, 0, allProperties, normalProperties.length, expertProperties.length);
    }

    final Node.Property[] tableProperties;
    if (propNames == null) {
      tableProperties = allProperties;
    } else {
      ArrayList list = new ArrayList(propNames.length);
      for (int n = 0; n < propNames.length; n++) {
        for (int p = 0; p < allProperties.length; p++) {
          if (propNames[n].equals(allProperties[p].getName())) {
            list.add(allProperties[p]);
          }
        }
      }
      tableProperties = (Node.Property[]) list.toArray(new Node.Property[list.size()]);
    }

    tablePropertyHolder.setProperties(tableProperties, sortable);
  }

  /**
   * helper method -- sets the table columns form an array of property descriptors
   * can be made public if needed
   * @deprecated -- not needed anymore, use setTableColumns(Object bean, String[] propNames)
   */
  private void setTableColumns(PropertyDescriptor[] propDescriptors) {
  	new UnsupportedOperationException("deprecated").printStackTrace();
    tablePropertyHolder.setProperties(calcNodeProperty(propDescriptors), null);
    //tablePropertyHolder.setProperties(propArr);
  }

  /** 
   * @deprecated -- not needed anymore, use setTableColumns(Object bean, String[] propNames)
   */
  public static Node.Property[] calcNodeProperty(PropertyDescriptor[] propDescriptors) {
    Property[] propArr = new Property[propDescriptors.length];
    for (int ix = 0; ix < propDescriptors.length; ix++) {
      propArr[ix] = new TableViewColumnProperty(propDescriptors[ix]);
    }
    return propArr;
  }

  /**
   * a helper method that determines the types of the properties by looking them up in the beanClass
   * @deprecated -- not needed anymore, use setTableColumns(Object bean, String[] propNames)
   */
  public static PropertyDescriptor[] calcPropertyDescriptorsFor(Class beanClass, String[] propNames)
    throws IntrospectionException {
    // [PENDING] we should not depend on BeanInfo here. Otheriwse NullPointerEx If someon wants to expose a column that is not declared inthe BI.
    // TODO BUG: Introspector does not find the DisplayName of the properties, that's why it's missing in the Table headers
    PropertyDescriptor[] propDescr = Introspector.getBeanInfo(beanClass).getPropertyDescriptors();
    java.util.ArrayList listNonHidden = new java.util.ArrayList(propDescr.length);
    for (int i = 0; i < propDescr.length; i++) {
      if (!propDescr[i].isHidden()) {
        listNonHidden.add(propDescr[i]);
      }
    }
    propDescr = (PropertyDescriptor[]) listNonHidden.toArray(new PropertyDescriptor[listNonHidden.size()]);

    if (propNames == null) {
      return propDescr;
    }

    if (propNames.length > propDescr.length) {
      System.err.println("Error: more propNames than beanClass properties!");
      System.err.println("PropNames = ");
      ArrayUtil.printArray(propNames, 8);
      System.err.println("PropDescr = ");
      ArrayUtil.printArray(propDescr, 8);
      return null;
    }

    //PropertyDescriptor[] resultDescr = new PropertyDescriptor[propNames.length];
    // find common ones:
    ArrayList resultList = new ArrayList();
    for (int ix = 0; ix < propNames.length; ix++) {
      for (int jx = 0; jx < propDescr.length; jx++) {
        if (propNames[ix].equals(propDescr[jx].getName())) {
          resultList.add(propDescr[jx]);
        }
      }
    }
    return (PropertyDescriptor[]) resultList.toArray(new PropertyDescriptor[resultList.size()]);
  }

  /**
   * Model that redirects the call to the super class if the Bean class is a subclass of
   * the one set with ListTableView.setProperties().
   * Otherwise it returns null. The goal is to display only the rows of the beans
   * instead of creating its getPropertySets. Not used in this implementation.
   * To use it, pass it as an argument to ListTableView()
   */
  static class SelectiveTableModel extends NodeTableModel {
    private final Class[] displayBeans;
    SelectiveTableModel(Class[] displayBeans) {
      this.displayBeans = displayBeans;
    }

    /**
     * Only return valid properties if the bean corresponding to this node shall be displayed in the
     * ListTable. Otherwise return null.
     */
    protected Property getPropertyFor(Node node, Property prop) {
      if (node instanceof GPNode) {
        Class beanClass = ((GPNode) node).getBean().getClass();
        for (int ix = 0; ix < displayBeans.length; ix++) {
          if (displayBeans[ix].equals(beanClass)) {
            return super.getPropertyFor(node, prop);
          }
        }
        return null;
      }
      return super.getPropertyFor(node, prop);
    }
  }

  /**
   * dummy implementation of a Node.Property, used to tell the ListTableView what columns it
   * shall display. It cannot be used to read or write properties.
   *
   * Instances of this class are used in the NodeTableModel.getPropertyFor() method
   * @see org.openide.explorer.view.ListTableView#setProperties()
   * @see org.openide.explorer.view.NodeTableModel#getPropertyFor()
   */
  private static class TableViewColumnProperty extends Property {
    public TableViewColumnProperty(String propertyName, String displayName, Class propertyType) {
      super(propertyType);
      setName(propertyName);
      if (displayName != null) {
        super.setDisplayName(displayName); 
      }
    }

    public TableViewColumnProperty(PropertyDescriptor prop) {
      super(prop.getPropertyType());
      setName(prop.getName());
      if (prop.getDisplayName() != null) {
        super.setDisplayName(prop.getDisplayName()); 
      }
    }

    public boolean canRead() {
      return false;
    }
    public boolean canWrite() {
      return false;
    }
    public Object getValue() throws IllegalAccessException, InvocationTargetException {
      return null;
    }
    public void setValue(Object obj)
      throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
    }
  }


}