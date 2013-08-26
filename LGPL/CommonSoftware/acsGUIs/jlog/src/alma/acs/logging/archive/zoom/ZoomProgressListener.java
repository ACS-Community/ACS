/*
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2008 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.acs.logging.archive.zoom;

/**
 * An interface for the progress of the zoom.
 * 
 * @author acaproni
 *
 */
public interface ZoomProgressListener {
	
	/**
	 * Notifies the listener about the number of files currently read by the zoom engine.
	 * <P>
	 * This method of the listener is called when the loading of logs starts
	 * with a value of <code>0</code> while the zoom engine scans the folder
	 * looking for the files to read.
	 * <BR>
	 * When the list is ready, the zoom engine executes this method passing the number
	 * of the file it has started to read. 
	 * 
	 * @param num The number of files currently read.
	 */
	public void zoomReadingFile(int num);
	
	/**
	 * Inform the listener about the total number of files that
	 * the zoom engine needs to read.
	 * <P>
	 * This method of the listener is executed only once by the engine,
	 * when it has generated the list of the files to read.
	 *  
	 * @param num
	 */
	public void zoomTotalFileToRead(int num);
	
}
