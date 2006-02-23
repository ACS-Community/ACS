/*
 * LabelValueBean.java
 *
 * Created on June 30, 2003, 11:06 AM
 */

package cern.laser.guiplatform.util;

/**
 *
 * @author  pawlowsk
 */
public class LabelValueBean {
    
    private String label = null;
    
    private String value = null;
    
    /** Creates a new instance of LabelValueBean */
    public LabelValueBean() {
        this("", "");
    }
    public LabelValueBean(String label, String value) {
        this.label = label;
        this.value = value;
    }
    public String toString() {
        return label + " ==> " + value;
    }
    
    public String getLabel() {
        return label;
    }
    
    public void setLabel(String label) {
        this.label = label;
    }
    
    public String getValue() {
        return value;
    }
    
    public void setValue(String value) {
        this.value = value;
    }
    
}
