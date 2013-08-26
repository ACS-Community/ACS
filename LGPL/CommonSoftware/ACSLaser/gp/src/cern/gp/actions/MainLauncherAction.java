package cern.gp.actions;

import cern.gp.util.GPManager;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import org.openide.NotifyDescriptor;
import org.openide.util.HelpCtx;
import org.openide.util.actions.CallableSystemAction;

/**
 * An Action that launches the main method of the class indicated in the
 * related system property {@link #MAIN_CLASS_OPTION}.
 * With this action you can quickly bind a class with a main method
 * into a menu. This is used, e.g. when running the Platform inside another
 * IDE, e.g. for debugging purposes.
 * <p>
 * See the corresponding HowTo "Running the GP Platform in Eclipse"
 *
 * @author Vito Baggiolini
 * @version $Revision: 1.2 $, $Date: 2006/09/25 08:52:36 $
 */
public class MainLauncherAction extends CallableSystemAction {
  public final static String MAIN_CLASS_OPTION = "cern.gp.mainclass";
  public static String mainClassName = System.getProperty(MAIN_CLASS_OPTION);
  public void performAction() {
     if (mainClassName == null) {
       NotifyDescriptor usage = new NotifyDescriptor.Message(
          "Main Class not set, use the command line option -J-D" + MAIN_CLASS_OPTION + " to set it");
       GPManager.notify(usage);
       return;
     }
     try {
       Class mainClass = Class.forName(MAIN_CLASS_OPTION);
       Method mainMeth = mainClass.getDeclaredMethod("main", new Class[] { String[].class  } );
       mainMeth.invoke(null, null);
     } catch (ClassNotFoundException ex) {
       NotifyDescriptor err = new NotifyDescriptor.Message("class not found " + MAIN_CLASS_OPTION);
       GPManager.notify(err);
     } catch (NoSuchMethodException ex) {
       NotifyDescriptor err = new NotifyDescriptor.Message("class " + MAIN_CLASS_OPTION + " has no main(String[]) method");
       GPManager.notify(err);
     } catch (IllegalAccessException ex) {
       NotifyDescriptor err = new NotifyDescriptor.Message("error invoking main on class " + MAIN_CLASS_OPTION);
       GPManager.notify(err);
     } catch (InvocationTargetException ex) { // the main method has been executed and thrown an exception
       GPManager.notify(GPManager.EXCEPTION, ex);
     }
  }

  public String getName() {
    return mainClassName + ".main()";
  }

  public HelpCtx getHelpCtx() {
    return HelpCtx.DEFAULT_HELP;
    // If you will provide context help then use:
    // return new HelpCtx(gagaAction.class);
  }

}
