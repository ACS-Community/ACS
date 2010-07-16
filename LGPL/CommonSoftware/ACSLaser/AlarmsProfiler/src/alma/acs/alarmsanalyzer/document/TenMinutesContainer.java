/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.alarmsanalyzer.document;

import java.sql.Timestamp;
import java.util.Date;
import java.util.Vector;

import cern.laser.source.alarmsysteminterface.FaultState;
import alma.acs.alarmsanalyzer.view.ChartViewBase;
import alma.alarmsystem.clients.source.SourceListener;

/**
 * The container with the number of alarms received every 10 minutes for a maximum 
 * of one week.
 * <P>
 * If the tool listens for alarms for more then one week, the oldest number are
 * removed.
 * 
 * @author acaproni
 *
 */
public class TenMinutesContainer implements Runnable, SourceListener {
	
	/**
	 * One entry storing the number of alarms received in 10 minutes after the timestamp.
	 *  
	 * @author acaproni
	 *
	 */
	class TenMinutesNumber {
		
		// The timestamp when the count started
		public final Timestamp time;
		
		/**
		 * The number of alarms received in 10 minutes
		 */
		private int value=0;
		
		public TenMinutesNumber() {
			time=new Timestamp(System.currentTimeMillis());
			value=0;
		}
		
		/**
		 * Increment the counter when a new alarm has been received
		 */
		public void inc() {
			value++;
		}

		/**
		 * Getter
		 */
		public int getValue() {
			return value;
		}
	}
	
	/**
	 * The time interval (in minutes) for recording alarms
	 */
	public static final int MINUTESTIMEINTERVAL=10;
	
	/**
	 * <code>true</code> if the container has been shut down
	 */
	protected boolean shutdown=false;
	
	/**
	 * The number we are currently updating
	 */
	private TenMinutesNumber actualNumber=null;
	
	/**
	 * The view showing numbers from this container
	 */
	private ChartViewBase chartView=null;
	
	/**
	 * The vector of numbers.
	 * <P>
	 * The increment and initial capacity is enough for 1 hr.
	 * <P>
	 * The size of the vector is limited by <code>numbersMaxSize</code>.
	 * If the tool sample for more then one week, the oldest numbers are discarded.
	 * <P>
	 * The oldest sample are in the beginning of the vector while the newest are at the end.
	 */
	private Vector<TenMinutesNumber> numbers = new Vector<TenMinutesContainer.TenMinutesNumber>(6,6);
	
	/**
	 * The max number of items in the vector <code>numbers</code>: it is enough for one week
	 */
	private final int numbersMaxSize=6*24*7;
	
	/**
	 * The singleton
	 */
	private static TenMinutesContainer singleton=null;
	
	public static TenMinutesContainer getInstance() {
		if (singleton==null) {
			singleton = new TenMinutesContainer();
		}
		return singleton;
	}
	
	/**
	 * Constructor
	 */
	private TenMinutesContainer() {
		Thread t = new Thread(this,getClass().getName());
		t.setDaemon(true);
		t.start();
	}

	@Override
	public void faultStateReceived(FaultState faultState) {
		if (actualNumber==null) {
			// This can happen if a fault state is received before
			// starting the thread
			//
			// We can safely ignore it.
			return;
		}
		actualNumber.inc();
	}

	@Override
	public void sourceXMLMsgReceived(String asiMessage) {}

	/**
	 * The thread does nothing but waiting that the time elapses then 
	 * start a new sample.
	 */
	@Override
	public void run() {
		while (!shutdown) {
			synchronized (this) {
				actualNumber=startNewSample();
			}
			for (int min=0; min<MINUTESTIMEINTERVAL && !shutdown; min++) {
				for (int sec=0; sec<60 && !shutdown; sec++) {
					try {
						Thread.sleep(1000);
					} catch (InterruptedException ie) {
						continue;
					}
				}
			}
			refresh();
		}
	}
	
	/**
	 * A new sample period just started: we instantiated a new {@link TenMinutesNumber} to store the
	 * number.
	 * <P>
	 * The {@link TenMinutesNumber} object newly created is added to the vector, <code>numbers</code> taking care
	 * that its size remains less the <code>MINUTESTIMEINTERVAL</code>. 
	 *   
	 * @return The newly created <code>TenMinutesNumber</code> for storing the sample
	 */
	private TenMinutesNumber startNewSample() {
		TenMinutesNumber ret = new TenMinutesNumber();
		while (numbers.size()>numbersMaxSize) {
			numbers.remove(0);
		}
		numbers.add(ret);
		return ret;
	}
	
	/**
	 * Shut down the container and free the resources
	 */
	public synchronized void shutdownContainer() {
		shutdown=true;
		chartView=null;
	}
	
	/**
	 * Set the view showing numbers from this container
	 * @param view
	 */
	public void setChartViewer(ChartViewBase view) {
		chartView=view;
	}
	
	/**
	 * Ask the view to refresh the chart
	 */
	public synchronized void refresh() {
		if (chartView==null) {
			return;
		}
		Date[] dates=new Date[numbers.size()];
		double[] values=new double[numbers.size()];
		for (int t=0; t<numbers.size(); t++) {
			dates[t]=numbers.get(t).time;
			values[t]=numbers.get(t).getValue();
		}
		chartView.refreshChart(values,dates);
	}
	
}
