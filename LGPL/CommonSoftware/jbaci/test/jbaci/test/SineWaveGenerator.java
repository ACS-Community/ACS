package jbaci.test;

import java.util.ArrayList;
import java.util.Vector;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.DataAccessSupport;
import alma.ACSErr.CompletionHolder;
import alma.acs.exceptions.AcsJException;

/**
 * This is a hardware simulator which generates sine wave for testing BACI properties.
 * 
 * TODO: use the thread pool managed by CharacteristicComponentImpl.
 * 
 * @author <a href="mailto:takashi.nakamotoATnao.ac.jp">Takashi Nakamoto</a>
 * @version $id$
 */
public class SineWaveGenerator implements Runnable {
	
	public interface ValueChangedListener {
		void valueChanged(double oldValue, double newValue);
	}
	
	/**
	 * Frequency of sine wave in Hz.
	 */
	private double frequency;
	
	/**
	 * Amplitude of sine wave in Volt.
	 */
	private double amplitude;
	
	/**
	 * Time resolution (or interval) in millisecond.
	 * This sine wave generator updates the signal value in the specified
	 * interval.  
	 */
	private long interval;
	
	/**
	 * Whether this signal generator is generating a signal or not.
	 */
	private boolean running;
	
	/**
	 * Logger
	 */
	private Logger logger;
		
	/**
	 * Current voltage of the signal in Volt.
	 */
	private double voltage;
	
	/**
	 * Thread of the loop of this sine wave generator. 
	 */
	private Thread thread;
	
	/**
	 * Listeners which are notified when the voltage is changed.
	 */
	private final ArrayList<ValueChangedListener> listeners = new ArrayList<ValueChangedListener>();
	
	/**
	 * 
	 * @param frequency Frequency of the sine wave to be generated in Hz.
	 * @param amplitude Amplitude of the sine wave to be generated in Volt.
	 * @param interval  Interval in which this sine wave generator updates
	 *        the signal value in millisecond.
	 */
	SineWaveGenerator(double frequency, double amplitude, long interval) {
		this.frequency   = frequency;
		this.amplitude   = amplitude;
		this.interval    = interval;
		this.running     = false;
		this.logger      = Logger.getLogger(this.getClass().getName());
		this.voltage     = 0.0;
		this.thread      = null;
		this.running     = false;
	}
	
	/**
	 * This method starts generating the sine wave signal.
	 * If it is already generating the signal, this method
	 * does nothing.
	 */
	public void start() {
		synchronized (this) {
			if (running) {
				return;
			}

			running = true;
			thread = new Thread(this);
			thread.start();
		}
	}
	
	/**
	 * This method stops generating the sine wave signal.
	 * If it is already stopped, this method does nothing.
	 * @throws InterruptedException 
	 */
	public void stop() throws InterruptedException {
		synchronized (this) {
			if (!running) {
				return;
			}
		}
			
		// Request stop to the main loop and wait until it stops.
		thread.interrupt();
		thread.join();
		
		synchronized (this) {
			running = false;
		}
	}
	
	/**
	 * Obtain the current voltage in Volt. This method returns
	 * 
	 * 
	 * @return the current voltage in Volt
	 */
	public double getVoltage() {
		synchronized (this) {
			return voltage;
		}
	}
	
	/**
	 * Add listener.
	 */
	public void addValueChangedListener(ValueChangedListener listener) {
		synchronized (listeners) {
			if (!listeners.contains(listener)) {
				listeners.add(listener);
			}
		}
	}
	
	/**
	 * Remove listener.
	 */
	public void removeValueChangedListener(ValueChangedListener listener) {
		synchronized (listeners) {
			if (listeners.contains(listener)) {
				listeners.remove(listener);
			}
		}
	}
	
	/**
	 * This is the main loop of the sine generator.
	 */
	@Override
	public void run() {
		try {
			// Base time for calculating sine wave;
			long t0 = System.nanoTime();
			long t;
			double prevVoltage, newVoltage;
			
			while (true) {
				t = System.nanoTime();

				prevVoltage = voltage;
				newVoltage = amplitude * Math.sin(2.0 * Math.PI * frequency * ((double)(t - t0)) * 1e-9);
				synchronized (this) {
					voltage = newVoltage;
				}
				
				synchronized (listeners) {
					for (ValueChangedListener listener : listeners) {
						listener.valueChanged(prevVoltage, newVoltage);
					}
				}
				
				Thread.sleep(interval);
			}
		} catch (InterruptedException ex) {
			// Interrupted. Terminate this thread.
			Thread.currentThread().interrupt();
		} catch (Throwable th) {
			// Catch any exception that occurs in this thread,
			// and terminate this thread.
			logger.log(Level.SEVERE, "The main loop of SineWaveGenerator is terminated illegally.", th);
			Thread.currentThread().interrupt();
		}
	}
	
	/**
	 * Main method to test SineWaveGenerator.
	 */
	public static void main(String args[]) throws Exception {
		SineWaveGenerator gen = new SineWaveGenerator(1.0, 5.5, 10);
		gen.addValueChangedListener(new ValueChangedListener() {
			public void valueChanged(double oldValue, double newValue) {
				System.out.println("voltage = " + newValue);
			}
		});
		
		gen.start();
		Thread.sleep(3000);
		gen.stop();
	}
}

