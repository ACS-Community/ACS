package cern.laser.test;


import cern.gp.beans.BeanSupport;
//import cern.laser.test.CopyNodesActionAction;


public class SimpleDemoBean extends BeanSupport {
    
    public static final String PROP_NAME = "name";
    public static final String PROP_VALUE = "value";
    private static int counter;
    private double value;
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
    public SimpleDemoBean(String name) {
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
    
    public void setValue(double newValue) {
      value = newValue;
    }
    
    
    public String[] getNodeActions() {
        return new String[] {
            ShowDetailsAction.class.getName(),
            //CopyNodesActionAction.class.getName(),
        };
    }
    
}

