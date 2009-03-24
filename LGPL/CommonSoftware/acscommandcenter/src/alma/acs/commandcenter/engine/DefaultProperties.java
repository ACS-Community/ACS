/*
 * Created on Oct 21, 2003 by mschilli
 */
package alma.acs.commandcenter.engine;

import java.util.Properties;

import alma.acs.util.ACSPorts;

/**
 *
 * @author mschilli
 */
public class DefaultProperties extends Properties {

   public DefaultProperties() {
      super();

      this.setProperty("org.omg.CORBA.ORBClass", "org.jacorb.orb.ORB");
      this.setProperty("org.omg.CORBA.ORBSingletonClass", "org.jacorb.orb.ORBSingleton");
      this.setProperty("ACS.manager", "corbaloc::" + ACSPorts.getIP() +":3000/Manager");
      this.setProperty("DAL.defaultReference", "corbaloc::" + ACSPorts.getIP() +":3012/CDB");  // updated, was: 5001

      // msc 2005-07-22: if ACS.data is already defined keep it, otherwise set it
      this.setProperty("ACS.data", System.getProperty("ACS.data", ".")); //System.getProperty("java.io.tempdir");
      
      // msc 2004-07-08: if ACS.tmp is already defined keep it, otherwise set it
      this.setProperty("ACS.tmp", System.getProperty("ACS.tmp", ".")); //System.getProperty("java.io.tempdir");
      
      // taken from class "OrbEval2" as of 2004-02-16
      this.setProperty("jacorb.orb.objectKeyMap.Manager", "ORB/ManagerPOA/Manager");
      this.setProperty("jacorb.implname", "ORB");
      
      // 2004-04-08: added to deal with this frequent error:
      // Connecting to DAL 'corbaloc::134.171.27.202:3212/DAL'...
      // [ WARNING: Received a request with a non-jacorb object key ]
      this.setProperty("jacorb.orb.objectKeyMap.CDB", "ORB/dalPOA/CDB");

/*
 msc 2009-03: commented out all abeans-related stuff from Acs 8.0.1 on

      // 2004-05-05: 
      // this.setProperty("abeans.home", "JavaOnlyAcsConfig");      
      // - > i though setting abeans.home to "JavaOnlyAcsConfig"
      // ensures that config from JavaOnlyAcsConfig.jar is used and NOT the config from cosyframework.jar.
      // it turns out abeans.home only takes file-names and URLs, but nothing from the classpath.
      // -> instead, abeans.home must be unset, then abeans will search the classpath for resource 'abeansStartup.txt'
      // and use its folder as config dir. this resource will be found first in acsJavaOnlyConfig.jar and not in
      // cosyframework.jar (due to alphabetical sorting of the jars by acsStartJava).
      // this.setProperty("abeans.home", "");
      // -> it turns out this might cause incompatibilities with other abeans-applications, they need cosyframework.jar-config
      // if i set abeans.home to "AcsJavaOnlyConfig" on the command line, i can access a jar zzzConfig.jar with a root-folder named "AcsJavaOnlyConfig"
      //this.setProperty("abeans.home", "AcsJavaOnlyConfig");
      
      //this.setProperty("abeans.home", "JavaOnlyAcsConfig");
      //  nope, this doesn't work. totally weird!
      
      //this.setProperty("abeans.home", "MyMyMy");

      // finally, this is alphabetically after "Config" from cosyframework.jar and works.
      this.setProperty("abeans.home", "PureJavaCfg");
      
      //this.setProperty("ACS.manager", "corbaloc::" + ACSPorts.getIP() +":3000/StandardNS/Manager%2DPOA/_root");
      //this.setProperty("ORBInitRef.NameService", "corbaloc::" + ACSPorts.getIP() +":4000/StandardNS/NameServer%2DPOA/_root");
      //this.setProperty("ORBInitRef.NameService", "corbaloc::" + ACSPorts.getIP() +":4000/NameService");
      //this.setProperty("ACS.repository" , "corbaloc::" + ACSPorts.getIP() +":4001/InterfaceRepository"); 
*/
   }

}
