/*
 * TablePropertyHolder.java
 *
 * Created on September 21, 2002, 1:06 PM
 */

package cern.gp.explorer;

import org.openide.nodes.Node;

/**
 * <i><font size="-1" color="#FF0000">**For internal use only** </font></i>
 * An interface that allows to set the properties displayed in the Table of a TreeTableExplorer and ListTableExplorer.
 * This interface was needed to factor common code into the TableHolder class. It is not intended for public use.
 *
 * @author  Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public interface TablePropertyHolder {
  public void setProperties(Node.Property[] props, boolean[] sortable);
}
