/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2011
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
package alma.acs.alarm.gui.senderpanel.table;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.table.AbstractTableModel;

import alma.acs.alarm.gui.senderpanel.SenderPanelUtils.Triplet;

/**
 * The model for the table.
 * <P>
 * The table is update once every 2 seconds by the swing timer thread.
 * 
 * @author acaproni
 *
 */
public class AlarmsSentTableModel extends AbstractTableModel implements ActionListener {
	
	private static class TripletSent {
		public final Triplet triplet;
		public final boolean activation;
		
		public TripletSent(Triplet triplet, boolean activation) {
			this.triplet = triplet;
			this.activation = activation;
		}
	}
	
	/**
	 * The swing timer
	 */
	private final Timer timer;
	
	/**
	 * The alarms shown in the table
	 */
	private final Vector<Triplet> triplets=new Vector<Triplet>();
	
	/**
	 * The triplets to flush in the table by the thread
	 */
	private final Map<String, TripletSent> tripletsToFlush= Collections.synchronizedMap(new HashMap<String, TripletSent>());
	
	/**
	 * Constructor
	 */
	public AlarmsSentTableModel() {
		timer = new Timer(2000, this);
	}

	@Override
	public int getRowCount() {
		return triplets.size();
	}

	@Override
	public int getColumnCount() {
		return 1;
	}

	@Override
	public Object getValueAt(int rowIndex, int columnIndex) {
		return triplets.get(rowIndex);
	}
	
	/**
	 * Put the received triplets in the queue waiting to be flushed by the next timer event
	 * 
	 * @param triplet The triplet of the alarm
	 * @param active The state of the alarm
	 */
	public void alarmSent(Triplet triplet, boolean active) {
		tripletsToFlush.put(triplet.id, new TripletSent(triplet, active));
		
	}
	
	/**
	 * @return The alarms in the table
	 */
	public Collection<Triplet> getAlarms() {
		return new Vector<Triplet>(triplets);
	}
	
	/**
	 * Start the thread
	 */
	public void start() {
		timer.setRepeats(true);
		timer.setInitialDelay(1000);
		timer.start();
	}
	
	/**
	 * Stop the thread
	 */
	public void stop() {
		timer.stop();
	}

	/**
	 * Refresh the content of the table i.e. Timer events
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		 // Active alarms must be added to the table;
		 // terminate alarm must be removed.
		Collection<TripletSent> tripletsList;
		synchronized (tripletsToFlush) {
			tripletsList = new Vector<TripletSent>(tripletsToFlush.values());
			tripletsToFlush.clear();
		}
		for (TripletSent triplet: tripletsList) {
			if (!triplet.activation && triplets.contains(triplet.triplet)) {
				triplets.remove(triplet.triplet);
			} else if (triplet.activation && !triplets.contains(triplet.triplet)) {
				triplets.add(triplet.triplet);
			}
			Collections.sort(triplets);
			fireTableDataChanged();
		}
	}

}
