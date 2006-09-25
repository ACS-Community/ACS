package cern.gp.explorer;

import java.beans.IntrospectionException;

import cern.gp.nodes.GPNode;


/**
 * A class that contains common code related to the Tables contained in the
 * ListTableExplorer and TreeTableExplorer.
 * Both ListTableExplorer and TreeTableExplorer contain an instance of this class
 * and delegate to it.
 *
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 * @author Vito Baggiolini
 */
interface TableHolder {
  /**
   * Sets the colums of the Table. The colums are identified by their name and their type.
   * Please make sure that the two arrays passed as arguments correspond.
   * <Strong>Caution</strong>do not use this with beans that implement the
   * {@link cern.gp.beans.BeanSupport#getPropertyInfo} method to customize settings for
   * properties. For such beans use {{#setTableColumns(Object, String[])} instead.
   *
   * @param propNames the names of the properties to be displayed
   * @param propTypes the types of the properties corresponding to the names
   */
  public void setTableColumns(Class[] propTypes, String[] propNames);
  
  /**
   * Sets the columns of the Table. This method determines the type of the properties by
   * doing reflection on the beanClass. It also takes into account info specified in
   * the method {@link cern.gp.beans.BeanSupport#getPropertyInfo}
   * All non-hidden properties of the bean will be displayed as columns in the Table
   *
   * @param the class from which the table colums are inferred
   */
  public void setTableColumns(Object bean) throws IntrospectionException;
  
  /**
   * Sets the columns of the Table. This method determines the type of the method by
   * doing reflection on the Bean. It also takes into account the additional info specified in
   * {@link cern.gp.beans.BeanSupport#getPropertyInfo}.<P>
   * Beware: all properties passed in the propNames argument must also be present in the beanClass.
   * If you already have created a GPNode for this bean, you should use 
   * {@link #setTableColumns(GPNode, String[])} because this is more efficient.<BR>
   * If you don't have such a bean, please use the {#setTableColumns(Class[], String[])} method.
   * 
   * @param bean the bean displayed in the table
   * @param propNames the properties to be displayed as columns in the table
   * @throws IntrospectionException if something goes wrong while introspecting the bean
   */
  public void setTableColumns(Object bean, String[] propNames) throws IntrospectionException;
  
  /**
   * Sets the columns of the Table. This method determines the type of the method by using the
   * information contained in the GPNode. It also takes into account the additional info specified
   * using the {@link cern.gp.beans.BeanSupport#getPropertyInfo} method.
   * Beware: all properties passed in the propNames argument must also be present in the beanClass
   * represented by this node.
   * If you don't have a suitable GPNode, please use the {#setTableColumns(Class[], String[])} method.
   *
   * @param bean the bean displayed in the table
   * @param propNames the properties to be displayed as columns in the table
   */
  public void setTableColumns(GPNode node, String[] propNames);

  /**
   * Sets the columns of the Table. This method determines the type of the properties by
   * doing reflection on the beanClass. As a result all properties of the bean will be
   * displayed as columns in the Table
   *
   * @deprecated use {@link #setTableColumns(Object)} instead
   * @param the class from which the table colums are inferred
   */
  public void setTableColumns(Class beanClass) throws IntrospectionException;
  
  /**
   * Sets the columns of the Table. This method determines the type of the properties by
   * doing reflection on the beanClass. Beware: all properties passed in the propNames argument
   * must also be present in the beanClass. If you don't have such a bean, please use the other
   * setTableColums() method.
   *
   * @deprecated use {@link #setTableColumns(Object, String[])} instead
   * @see #setTableColumns(Class[], String[])
   * @param propNames the name of the properties to be displayed
   * @param beanClass the class of one of the beans that shall be displayed. It must have all
   * properties mentioned in the propNames argument
   * @throws IntrospectionException if a property is not found in the beanClass
   */
  public void setTableColumns(Class beanClass, String[] propNames) throws IntrospectionException;
  
	public abstract void setTableColumns(Class[] propTypes, String[] propNames, boolean[] sortable);
	public abstract void setTableColumns(Object bean, String[] propNames, boolean[] sortable)
		throws IntrospectionException;
	public abstract void setTableColumns(GPNode node, String[] propNames, boolean[] sortable);
}