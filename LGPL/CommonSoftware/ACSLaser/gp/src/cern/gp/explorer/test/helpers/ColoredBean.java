/*
 * SimpleDemoBean.java
 *
 * Created on September 19, 2002, 3:42 PM
 */

package cern.gp.explorer.test.helpers;

/**
 * @author Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class ColoredBean {
  private static int counter = 0;
  
  private String name = "hello";
  
  private Integer myInt;
  
  public ColoredBean() {
    this("no name");
  }
  
  public ColoredBean(String name) {
    this.name = name;
    myInt = new Integer(counter++);
  }
  
  public String getName() {
    return name;
  }
  public void setName(String newName) {
    name = newName;
  }
  public double getValue() {
    return 0;
  }
  
  public Integer getInt() {
    return myInt;
  }
  
  public void setInt(Integer newInt) {
    myInt = newInt;
  }
}
