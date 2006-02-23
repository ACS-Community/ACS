
package cern.gp.beans.editors.support;

/**
 * Inteface that has to be implemented by JavaBeans that provide properties with 
 * dynamic sets of tags. <p>
 * Such dynamic tags can be visualized using a tagged PropertyEditor which itself should
 * implement the {@link BeanDependentPropertyEditor} interface.
 *
 * @author  Vito Baggiolini
 * @version $Revision: 1.1 $ $Date: 2005/06/07 03:26:13 $
 */
public interface TaggedBean {
  /**
   * get the tags for a given property name
   * @param propName the property name 
   * @return an array of tags 
   */
    String[] getTagsFor(String propName);
}
