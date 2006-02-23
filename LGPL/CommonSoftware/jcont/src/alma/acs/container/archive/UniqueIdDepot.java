/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.container.archive;

/**
 * Interface that allows ACS framework users to implement their own 
 * mechanism to store ranges of unique IDs.
 * This is only useful if an ACS application runs in isolation,
 * e.g. on a laptop computer.
 * <p>
 * Later there should always be an archive available, and the local archive
 * should synchronize internally with the main archive.
 * Since this is not worked out yet, the container offers this hook
 * to locally persist (and use up one ID after the other) 
 * a range of unique IDs. 
 * <p>
 * This range can either be initially retrieved by the containe from the archive
 * and then stored through this interface across container sessions, 
 * or can be provided manually from the beginning.
 * The container always tries first to pull a new range of IDs from the archive,
 * and if this fails, it will try to use this interface.   
 * <p>
 * The java property <b>ACS.Container.UniqueIdDepotImpl</b> must be set to 
 * a class that implements the <code>UniqueIdDepot</code> interface
 * and has a public constructor without parameters.
 * For example, with 
 * <code>-DACS.Container.UniqueIdDepotImpl=alma.obsprep.util.TempDirUniqueIdDepot</code>
 * added to the VM start script, the container will attempt to instantiate that
 * <code>TempDirUniqueIdDepot</code> class and use it for ID range persistence.
 * 
 * @author hsommer
 * created Oct 31, 2003 2:19:45 PM
 */
public interface UniqueIdDepot
{
	/**
	 * The Java property that defines the class which implements this interface.
	 * The container will attempt to load the impl class through its default ctor.
	 */
	public static final String IMPL_CLASS_PROPERTY = "ACS.Container.UniqueIdDepotImpl";
	   
	/**
	 * Gets the base for unique IDs in the format defined by {@link Uid}.
	 * The container will use this ID itself, and will increment the local identifier part
	 * to generate more IDs. 
	 */
	public String getUniqueIdBase();
	
	/**
	 * Stores the base for unique IDs. 
	 * The container will call this method shortly after having gotten an ID range
	 * through <code>getUniqueIdBase</code>, having "taken out" a certain number of IDs.
	 * This ensures thrifty usage of IDs, because only the local identifier part has
	 * to be incremented, lasting many container sessions, even with container crashes.
	 */
	public void storeUniqueIdBase(String newUniqueIdBase);
}
