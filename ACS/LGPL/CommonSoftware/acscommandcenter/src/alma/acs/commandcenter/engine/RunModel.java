/*
 * Created on Dec 5, 2003 by mschilli
 */
package alma.acs.commandcenter.engine;


/**
 * Contains the data needed to run the various Acs workers.
 * <p>
 * Typically this is the Executors' view onto an CommandCenterProject model instance.
 * The implementing class provides the project's content as needed for the Acs workers.
 * For example, an implementation can, depending on a flag in the model, decide whether the
 * containerRemoteHost for a container C should be the globally declared one or one that
 * was specifically declared for C. CommandCenter does this in its implementation.</p>
 * <p>
 * Other ways of implementing this occur as well, e.g., if you want to run a
 * pure-java Acs in a virtual machine side-by-side with your application.</p>
 * <p>
 * See the AcsCommandCenter(Builtin)Tools.xml file to find out what data your
 * RunModel implementation needs to provide to the command(s) you want to run.</p>
 * 
 * @see alma.acs.commandcenter.app.ProjectRunModel
 * @see RunModelAdapter
 *  
 * @author mschilli
 */
public interface RunModel {

   //
   // local java services
   //
   public String getServicesLocalJavaPort();
   public String getServicesLocalJavaRoot();

   //
   // local java manager
   //
   public String getManagerLocalJavaPort();
   public String getManagerLocalJavaAgainstCDBHost();
   public String getManagerLocalJavaAgainstCDBPort();

   //
   // local java container
   //
   public String getContainerLocalJavaPort();
   
   //
   // global for manager and services 
   // (may also be used for a container - for this, the implementation of 
   // getContainerXXX() would forward to here)
   //
   public String getScriptBase();
   public String getRemoteHost();
   public String getRemoteAccount();
   public String getRemotePassword();

   // 
   // per tool (whether to use some global setting must be decided by model)
   // 
   public String getToolAgainstManagerHost();
   public String getToolAgainstManagerPort();
   public String getToolAgainstInterfaceRepository();
   public String getToolAgainstNameService();

   //
   // per container (whether to use some global setting must be decided by model)
   //
   public String getContainerAgainstManagerHost();
   public String getContainerAgainstManagerPort();
   public String getContainerAgainstCDB();
   public String getContainerAgainstInterfaceRepository();

   public String getContainerName();
   public String getContainerType();
   public String[] getContainerTypeModifiers();  // new in Acs 7.1
   public String getContainerHeapSize(); // new in Acs 8.1
   public String getContainerScriptBase();
   public String getContainerRemoteHost();
   public String getContainerRemoteAccount();
   public String getContainerRemotePassword();

}
