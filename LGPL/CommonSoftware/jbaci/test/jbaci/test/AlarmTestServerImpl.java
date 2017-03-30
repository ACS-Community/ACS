package jbaci.test;

import alma.ACS.ROdouble;
import alma.ACS.ROdoubleHelper;
import alma.ACS.ROdoublePOATie;
import alma.ACS.impl.CharacteristicComponentImpl;
import alma.ACS.impl.ROdoubleImpl;
import alma.ACS.jbaci.DataAccess;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.jbaci.AlarmTestServerOperations;
import alma.maciErrType.wrappers.AcsJComponentCleanUpEx;
import tmcdb.test.MCTestDataAccess;

public class AlarmTestServerImpl extends CharacteristicComponentImpl implements
		AlarmTestServerOperations {

	/**
	 * ROdouble property
	 */
	protected ROdouble doubleProp;
	
	/**
	 * Sine wave generator.
	 */
	private SineWaveGenerator generator;
	
	/**
	 * DataAccess for the sine wave generator.
	 */
	private SineWaveGeneratorDataAccess generatorDataAccess;
	
	/**
	 * @see alma.acs.component.ComponentLifecycle#initialize(alma.acs.container.ContainerServices)
	 */
	public void initialize(ContainerServices containerServices)
		throws ComponentLifecycleException {
		super.initialize(containerServices);
		
		try {
			generator = new SineWaveGenerator(1.0, 1.0, 10);
			generatorDataAccess = new SineWaveGeneratorDataAccess();
			generator.addValueChangedListener(generatorDataAccess);
			
			DataAccess<Double> doubleROPropDA = generatorDataAccess;
			ROdoubleImpl doublePropImpl =  new ROdoubleImpl("doubleProp", this, doubleROPropDA);
			ROdoublePOATie currentTie = new ROdoublePOATie(doublePropImpl);
			doubleProp = ROdoubleHelper.narrow(this.registerProperty(doublePropImpl, currentTie));
			
			generator.start();
		} catch (Throwable th) {
			throw new ComponentLifecycleException("Failed to create properties.", th);
		}
	}
	
	/**
	 * @see alma.acs.component.ComponentLifecycle#cleanUp()
	 */
	public void cleanUp() throws AcsJComponentCleanUpEx {
		generator.removeValueChangedListener(generatorDataAccess);
	}
	
	/**
	 * @see jbaci.test.AlarmTestServerOperations#doubleProp
	 */
	public ROdouble doubleProp() {
		return doubleProp;
	}
}
