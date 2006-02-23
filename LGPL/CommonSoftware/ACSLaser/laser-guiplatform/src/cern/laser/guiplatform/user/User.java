/*
 * User.java
 *
 * Created on April 9, 2003, 3:28 PM
 */

package cern.laser.guiplatform.user;

/**
 * @deprecated replaced by cern.laser.console.User interface
 * @author  pawlowsk
 */
public class User {
    
    private String name = null;
    
    private String loginName = null;
    
    private String lastName = null;
    
    /** Creates a new instance of User */
    public User() {
        name = loginName = lastName = "";
    }
    public User(String loginName, String firstName, String lastName) {
        this.loginName = loginName;
        name = firstName;
        this.lastName = lastName;
    }
    
    public String getName() {
        return name;
    }
    
    public String getLoginName() {
        return loginName;
    }
    
    public String getLastName() { 
        return lastName;
    }
}
