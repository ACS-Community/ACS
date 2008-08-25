/**
 *      @author Julio Araya (jaray[at]alumnos.inf.utfsm.cl) &
 *      Nicolas Troncoso (ntroncos[at]alumnos.inf.utfsm.cl)
 **/

package cl.utfsm.samplingSystemUI.core;
import java.util.Hashtable;/*<K,V>;*/

import alma.ACSErrTypeCommon.CouldntAccessComponentEx;
import alma.ACSErrTypeCommon.TypeNotSupportedEx;



/**
 * The class is a base class for sampling tools. A tool must extend this class
 * and implement a static main. Usage of the sampling system must only be done
 * through the member provided by this class. 
 *
 */
public class SamplingManagerUITool {
	protected static AcsInformation info=null;
	private static Hashtable<SampDetail,PropertySamp> sampler = new Hashtable<SampDetail,PropertySamp>();
	/*container useage count*/

	/**
	 * Prepares the conection to the ACS infrastructure. This member creates
	 * two objects, an AcsInformation and a SamplingManager. This two are
	 * the links with the ACS infrastructure.
	 * 
	 * @param clientName string tha contains the name with which the tool
	 * will register with ACS.
	 * @param managerName string that contains the name of the sampling
	 * manager in the container. This manager is specified in the CDB.
	 *
	 * @see AcsInformation
	 * @see SamplingManager
	 */
	protected static void spinUp(String clientName, String managerName) throws AcsInformationException, SamplingManagerException {
		info = AcsInformation.getInstance(clientName);
		SamplingManager.getInstance(managerName);
	}

	/**
	 * Start the sampling of a property. This member will start a
	 * PropertySamp object, which will run in a separeted Thread.
	 * 
	 * @param sDetail a SampDetail object that contains the specification
	 * for the sampling.
	 * @throws CouldntAccessComponentEx 
	 * @throws TypeNotSupportedEx 
	 * @see SampDetail
	 * @see PropertySamp  
	 */
	protected static void startSample(SampDetail sDetail) throws CouldntAccessComponentEx, TypeNotSupportedEx{
		
		if(sampler.get(sDetail) == null)
			sampler.put(sDetail,new PropertySamp(sDetail));

		try {
			sampler.get(sDetail).run();
		} catch(CouldntAccessComponentEx e) {
			sampler.remove(sDetail);
			throw e;
		} catch(alma.ACSErrTypeCommon.TypeNotSupportedEx e) {
			sampler.remove(sDetail);
			throw e;
		}
	}

	/**
	 * Stop the sampling of a property identified by a SampDetail. 
	 *
	 * @see SampDetail
	 */
	protected static void stopSample(SampDetail sDetail){
		sampler.get(sDetail).stop();
	}

	/**
	 * Stop and delete from the internal structures the sampling identified
	 * by a SampDetail.
	 *
	 * @see SampDetail
	 */
	protected static void deleteSample(SampDetail sDetail){
		stopSample(sDetail);
		sampler.remove(sDetail);
	}

	/**
	 * Shutdown all the sampling operation. Stop every sampling, and
	 * disconnects from the ACS manager.
	 *
	 * @see AcsInformation
	 */
	protected static void tearDown() throws Exception{
		for (PropertySamp item : sampler.values()) {
			item.stop();
			info.shutDown();
		}
	}

	protected static void hashPrint(){
		for (PropertySamp item : sampler.values()){
			System.out.println(item);	
		}
	}

	/**
	 * Wraper that checks if a component exists.
	 *
	 * @param componetName the name of the component.
	 */
	protected static boolean componentExists(String componentName){
		return info.componentExists(componentName);

	}

	/**
	 * Wraper that checks if a property exists given a component.
	 *
	 * @param componetName the name of the component.
	 * @param propertyName the name of the property. 
	 */
	protected static boolean propertyExists(String componentName, String propertyName){
		return info.propertyExists(componentName,propertyName);
	}
}

