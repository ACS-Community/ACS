/*
 * SimpleDemoBean.java
 *
 * Created on September 19, 2002, 3:42 PM
 */

package cern.gp.explorer.test.helpers;

import cern.gp.beans.BeanSupport;

/**
 * A simple bean used for demo and testing purposes
 * @author Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class SimpleDemoBean extends BeanSupport {
    private static int counter;
    private final double value;
    private String name = "hello";
    
    /**
     * creates a bean with a default name which can be modified
     * with the {@link #setName(String)} method
     */
    public SimpleDemoBean() {
        this ("hello");
    }
    
    /**
     * constructs a bean with a given name
     * @param name the initial value of the name property
     */
    public SimpleDemoBean (String name) {
        this.name = name;
        value = counter++;
    }
    
    /**
     * getter method
     * @return the name property
     */
    public String getName() {
      return name;
    }
    /**
     * Setter method
     * @param name the new value of the name
     */
    public void setName(String newName) {
        name = newName;
    }
    /**
     * accessor methods
     * @returns the current value
     */
    public double getValue() {
        return value;
    }
}
