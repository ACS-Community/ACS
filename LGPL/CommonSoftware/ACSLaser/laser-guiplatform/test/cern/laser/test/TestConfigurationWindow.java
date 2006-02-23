/*
 * TestConfigurationWindow.java
 *
 * Created on October 8, 2003, 11:16 AM
 */

package cern.laser.test;

import org.apache.log4j.Logger;
import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.windows.TopComponent;

import cern.laser.client.LaserException;
import cern.laser.console.Configuration;
import cern.laser.console.LaserConfigurationNotFoundException;
import cern.laser.console.LaserConsoleException;
import cern.laser.console.User;
import cern.laser.console.UserHandler;
import cern.laser.guiplatform.user.UserHandlerFactory;
import cern.laser.guiplatform.util.AppRegister;
import cern.laser.guiplatform.util.Constants;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windows.configuration.ConsoleConfigurationWindow;

/**
 *
 * @author  Bartlomiej Pawlowski <Bartlomiej.Pawlowski@cern.ch>
 */
public class TestConfigurationWindow {

    public static Logger logger = LogFactory.getLogger(TestConfigurationWindow.class.getName());
    
    static {
        // jndi.properties
        System.setProperty("java.naming.factory.initial", "com.evermind.server.ApplicationClientInitialContextFactory");
        System.setProperty("java.naming.provider.url", "ormi://abjas1/laser-core");
        System.setProperty("java.naming.security.principal", "admin");
        System.setProperty("java.naming.security.credentials", "password");
    }

    
    
    /** Creates a new instance of TestConfigurationWindow */
    public TestConfigurationWindow() {
    }
    
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) throws Exception, Error {
        testConsoleConfigurationWindow();
    }

    
    /**
     */
    public static void testConsoleConfigurationWindow() throws LaserException {
        
        // like login user
        UserHandler userHandler = UserHandlerFactory.getHandler();
        User user = null;
        try {
            user = userHandler.getUser("laser");
            AppRegister.getInstance().registerUser(user);
        } catch (LaserConsoleException lce) {
            //AcWindowManager.notifyError("Unable to get user, probably network problem");
            DialogDisplayer.getDefault().notify(
                new NotifyDescriptor.Message("Unable to get user, probably network problem"));
            logger.error(lce, lce.fillInStackTrace());
            return;
        }
    
        try {
            if ( user.getConfiguration(Constants.DEFAULT_CONFIGURATION_NAME) != null )
                user.removeConfiguration(Constants.DEFAULT_CONFIGURATION_NAME);
        } catch (LaserConfigurationNotFoundException lcnf) {
            logger.error(lcnf, lcnf.fillInStackTrace());
            logger.error(lcnf.getRootCause(), lcnf.getRootCause().fillInStackTrace());
        }
        Configuration loadedConfiguration = null;
        Configuration confTemp = null;
        if ( (confTemp = user.getDefaultConfiguration()) == null ) {
            System.out.println("user: " + user.getName() + " does not have default configuration");
            loadedConfiguration = user.createConfiguration(
                                            Constants.DEFAULT_CONFIGURATION_NAME);
        } else {
            loadedConfiguration = confTemp;
        }
        
        AppRegister.getInstance().registerLoadedConfiguration(loadedConfiguration);
        
        TopComponent tc = new ConsoleConfigurationWindow(user, loadedConfiguration);
                
        
        tc.open();
        
        
    }
    
    
}
