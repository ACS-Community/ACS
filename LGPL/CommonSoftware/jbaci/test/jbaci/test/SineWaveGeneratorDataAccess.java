package jbaci.test;

import alma.ACS.jbaci.DataAccessSupport;
import alma.ACSErr.CompletionHolder;
import alma.acs.exceptions.AcsJException;

public class SineWaveGeneratorDataAccess extends DataAccessSupport<Double> implements SineWaveGenerator.ValueChangedListener {
	/**
	 * The voltage recently obtained.
	 */
	private double voltage; 
	
	public SineWaveGeneratorDataAccess() {
		voltage = 0.0;
	}
	
	@Override
	public Double get(CompletionHolder completionHolder) throws AcsJException {
		synchronized (this) {
			return Double.valueOf(voltage);
		}
	}

	@Override
	public boolean initializeValue() {
		// Does not allow overwriting the generated signal.
		return false;
	}

	@Override
	public void set(Double value, CompletionHolder completion) throws AcsJException {
		// The signal from the sine wave generator is read-only.
		// Nothing to do.
	}

	@Override
	public void valueChanged(double oldValue, double newValue) {
		this.notify(Double.valueOf(oldValue), Double.valueOf(newValue));
		synchronized (this) {
			voltage = newValue;
		}
	}
}
