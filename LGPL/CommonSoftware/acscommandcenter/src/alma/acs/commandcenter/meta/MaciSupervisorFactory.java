/*
 * Created on Apr 29, 2005 by mschilli
 */
package alma.acs.commandcenter.meta;

import java.util.Iterator;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTree;

import org.omg.CORBA.ORB;

import alma.acs.util.AcsLocations;



public class MaciSupervisorFactory {

   // -------- FOR LOCAL TESTING ------
   public static void main(String[] args) {
   	try {
   	
   	Firestarter fs = new Firestarter("MaciSupervisorFactory-Tryout");
   	fs.prepareSupervisorFactory(Logger.global);
   	MaciSupervisorFactory f = fs.giveSupervisorFactory();
      IMaciSupervisor inst = f.giveMaciSupervisor(AcsLocations.convertToManagerLocation("te22", "3500"), Logger.global);
		
      JFrame frame = new JFrame();
      frame.getContentPane().add(new JScrollPane(new JTree(inst.getMaciInfo())));
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
      frame.pack();
      frame.setVisible(true);
      
   	}catch(Exception exc) {
   		exc.printStackTrace();
   	}
   } // ---------------------------------

   
   
	
   

	////////////////////////////////////////////////////////
	/// ----------------- Internal --------------------- ///
	////////////////////////////////////////////////////////

   private Map<String, IMaciSupervisor> managerLoc2instance = new WeakHashMap<String, IMaciSupervisor>();
   
   private String clientName = null;
   private ORB orb = null;
   private Logger factoryLogger = null;

   
   
	////////////////////////////////////////////////////////
	/// ------------------- API ------------------------ ///
	////////////////////////////////////////////////////////


   /**
	 * Creates a Firestarter for a client with the specified name (like "AcsCommandCenter"
	 * or "OMC"). The new factory will pass the given
    * ORB to new MaciSupervisors.
    */
	public MaciSupervisorFactory(String clientName, Logger factoryLogger, ORB orb) {
		this.clientName = clientName;
		this.orb = orb;
		this.factoryLogger = factoryLogger;
	}
	
   /**
    * Creates a factory. The new factory will pass the given
    * ORB to new MaciSupervisors.
    * @deprecated always use other constructor with clientName param
    */
	public MaciSupervisorFactory(Logger factoryLogger, ORB orb) {
		this.clientName = "";
		this.orb = orb;
		this.factoryLogger = factoryLogger;
	}

	
	/**
	 * Stops all MaciSupervisors that have been created through this factory.
	 */
   synchronized public void stop() {
   	Iterator<IMaciSupervisor> iter = managerLoc2instance.values().iterator();
   	while (iter.hasNext()) {
   		IMaciSupervisor s = iter.next(); 
   		try {
				s.stop();
			} catch (Exception exc) {
				factoryLogger.log(Level.INFO, "failed to stop supervisor "+s+": "+exc, exc);
			}
   	}
   }
   
	/**
	 * Returns a connected IMaciSupervisor.
	 * 
	 * @param managerHost 
	 * 				the manager host
	 * @param managerPort 
	 * 				the manager port
	 * @param logger 
	 * 				the logger (will only be used if the macisupervisor is newly created)
	 * 
	 * @return a connected IMaciSupervisor.
	 */
   synchronized public IMaciSupervisor giveMaciSupervisor(String managerHost, String managerPort, Logger logger) {
		return giveMaciSupervisor(AcsLocations.convertToManagerLocation(managerHost, managerPort), logger);
	}

	/**
	 * Returns a connected IMaciSupervisor.
	 * 
	 * @param managerLoc 
	 * 				the managerlocation
	 * @param logger 
	 * 				the logger (will only be used if the macisupervisor is newly created)
	 * 
	 * @return a connected IMaciSupervisor.
	 */
	synchronized public IMaciSupervisor giveMaciSupervisor(String managerLoc, Logger logger) {
		
      IMaciSupervisor ret = null;
      ret = managerLoc2instance.get(managerLoc);

      // see if instance needs to be created
      if (ret == null) {
			ret = new MaciSupervisor(clientName, managerLoc, orb, logger);
			managerLoc2instance.put(managerLoc, ret);
			
      } else {
      	// we have one already
      }
      
      // start is smart enough to connect if (and only if) necessary
   	ret.start(); 
      
      return ret;
	}
	
}




