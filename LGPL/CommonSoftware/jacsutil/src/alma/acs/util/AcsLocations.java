/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2002 Copyright
 * by ESO (in the framework of the ALMA collaboration), All rights reserved
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms
 * of the GNU Lesser General Public License as published by the Free Software Foundation;
 * either version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with
 * this library; if not, write to the Free Software Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 */
/*
 * Created on Mar 17, 2004 by mschilli
 */
package alma.acs.util;


/**
 * Provides some support for dealing with manager references and corbalocs.
 * 
 * @author mschilli
 */
public class AcsLocations {


	// //////////////////////////////////////////////////////
	// / ------------------- API ------------------------ ///
	// //////////////////////////////////////////////////////



	/**
	 * Returns the stringified IP or "localhost" if an error were to occur.
	 * This method forwards to {@link ACSPorts#getIP()}.
	 * 
	 * @return the stringified IP or "localhost" if an error were to occur.
	 */
	public static String getLocalIP () {
		return ACSPorts.getIP();
	}


	/**
	 * Figures out the manager location by reading the related system properties, otherwise
	 * falls back to a default value.
	 * 
	 * The strategy uses the following order:
	 * <ol>
	 * <li>if both exist, use properties <code>ACS.managerhost</code> and
	 * <code>ACS.baseport</code>
	 * <li>if it exists, use property <code>ACS.manager</code>
	 * <li>if it exists, use <code>ACS.baseport</code> together with
	 * <code>local IP</code>
	 * <li>use port <code>3000</code> together with <code>local IP</code>
	 * </ol>
	 * 
	 * @return our best guess for a valid manager location (something like
	 *         <code>corbaloc::134.171.27.234:3000/Manager</code>)
	 */
	static public String figureOutManagerLocation () {

		String baseport = System.getProperty("ACS.baseport", "").trim();

		// --- first of all, try to convert the baseport property to a manager port

		String managerPort = "";

		if (!baseport.equals("")) {
			try {
				int acsInstance = -1;
				// instance must be specified and positive
				acsInstance = Integer.parseInt(baseport);
				if (acsInstance > -1) {
					// convert instance to actual port
					managerPort = ACSPorts.globalInstance(acsInstance).giveManagerPort();
				}
			} catch (NumberFormatException exc) {}
		}

		// --- try to read new two-property definition

		String managerHost = System.getProperty("ACS.managerhost", "").trim();

		if (!managerHost.equals("") && !managerPort.equals("")) {
			return convertToManagerLocation(managerHost, managerPort);
		}

		// --- try to read old single-property definition

		String managerLoc = System.getProperty("ACS.manager", "").trim();

		if (!managerLoc.equals(""))
			return managerLoc;


		// --- try to put together the localhost with the baseport

		if (!managerPort.equals(""))
			return convertToManagerLocation(ACSPorts.getIP(), managerPort);


		// --- fallback to localhost:3000

		return convertToManagerLocation(ACSPorts.getIP(), "3000");
	}

	/**
	 * Extracts host and port from a location string (either a corbaloc or an IOR).
	 * <p>
	 * This method is the counterpart to all the <code>convertTo...</code> methods
	 * defined in this class.
	 * <p>
	 * The location string's format is recognized by the prefix (case insensitive).
	 * If the format is unknown, an exception will be thrown.
	 * 
	 * @param loc a CORBALOC or an IOR
	 * @return an array {host, port}
	 * @throws IllegalArgumentException If the location has an unknown format or is otherwise invalid
	 */
	static public String[] convert (String loc) throws IllegalArgumentException {

		String host = null;
		String port = null;

		if (loc.regionMatches(true, 0, "ior", 0, "ior".length())) {

			// have ior parsed
			
			String[] parseResult = IorParser.parse(loc);
			
			host = parseResult[0];
			port = parseResult[1];
			
		} else if (loc.regionMatches(true, 0, "corbaloc", 0, "corbaloc".length())) {

			// split corbaloc into parts

			int a = loc.indexOf(":", 0) + 1;
			int b = loc.indexOf(":", a) + 1;
			int c = loc.indexOf(":", b);
			int d = c + 1;
			int e = loc.indexOf("/", d);

			if (b == -1 || c == -1 || d == -1 || e == -1) {
				throw new IllegalArgumentException("not a valid location: \"" + loc + "\"");
			}
			
			host = loc.substring(b, c);
			port = loc.substring(d, e);

		} else {
			throw new IllegalArgumentException("not a valid location: \"" + loc + "\"");
		}

		return new String[]{host, port};

	}


// Manager	
	
	/**
	 * The suffix for a Manager location.
	 * Note: Not a constant, to allow for changes at runtime in case this might be needed.
	 */
	static public String MANAGER_SYMBOL = "Manager";


	/**
	 * Composes a Manager corbaloc for the specified host and port.
	 */
	static public String convertToManagerLocation (String host, String port) {
		return convertToManagerLocation(host, port, "");
	}

	/**
	 * Composes a Manager corbaloc for the specified host, port, and protocol.
	 */
	static public String convertToManagerLocation (String host, String port, String protocol) {
		return "corbaloc:" + protocol + ":" + host.trim() + ":" + port.trim() + "/" + MANAGER_SYMBOL;
	}

	
// NameService

	/**
	 * The suffix for a NameService location.
	 * Note: Not a constant, to allow for changes at runtime in case this might be needed.
	 */
	static public String NAMESERVICE_SYMBOL = "NameService";


	/**
	 * Composes a NameService corbaloc for the specified host and port.
	 */
	static public String convertToNameServiceLocation (String host, String port) {
		return convertToNameServiceLocation(host, port, "");
	}

	/**
	 * Composes a NameService corbaloc for the specified host, port, and protocol.
	 */
	static public String convertToNameServiceLocation (String host, String port, String protocol) {
		return "corbaloc:" + protocol + ":" + host.trim() + ":" + port.trim() + "/" + NAMESERVICE_SYMBOL;
	}


// InterfaceRepository
	
	/**
	 * The suffix for a InterfaceRepository location.
	 * Note: Not a constant, to allow for changes at runtime in case this might be needed.
	 */
	static public String INTERFACEREPOSITORY_SYMBOL = "InterfaceRepository";


	/**
	 * Composes a InterfaceRepository corbaloc for the specified host and port.
	 */
	static public String convertToInterfaceRepositoryLocation (String host, String port) {
		return convertToInterfaceRepositoryLocation(host, port, "");
	}

	/**
	 * Composes a InterfaceRepository corbaloc for the specified host, port, and protocol.
	 */
	static public String convertToInterfaceRepositoryLocation (String host, String port, String protocol) {
		return "corbaloc:" + protocol + ":" + host.trim() + ":" + port.trim() + "/" + INTERFACEREPOSITORY_SYMBOL;
	}

	
// CDB

	/**
	 * The suffix for a cdb-dal location.
	 * Note: Not a constant, to allow for changes at runtime in case this might be needed.
	 */
	static public String CDB_SYMBOL = "CDB";


	/**
	 * Composes a CDB corbaloc for the specified host and port.
	 */
	static public String convertToCdbLocation (String host, String port) {
		return convertToCdbLocation(host, port, "");
	}

	/**
	 * Composes a CDB corbaloc for the specified host, port, and protocol.
	 */
	static public String convertToCdbLocation (String host, String port, String protocol) {
		return "corbaloc:" + protocol + ":" + host.trim() + ":" + port.trim() + "/" + CDB_SYMBOL;
	}


// ACSServicesDaemon
	
	/**
	 * The suffix for a ServicesDaemon location.
	 * <p>
	 * Note that this string constant is also defined in acsdaemon.idl 
	 * and would be accessible as "alma.acsdaemon.servicesDaemonServiceName.value"
	 * if module acsdaemonidl would not come after jacsutil.
	 * @TODO clean up the module dependencies.
	 */
	static public final String SERVICESDAEMON_SYMBOL = "ACSServicesDaemon";

	/**
	 * Composes a ServicesDaemon corbaloc for the specified host.
	 */
	static public String convertToServicesDaemonLocation (String host) {
		return "corbaloc::" + host.trim() + ":" + ACSPorts.getServicesDaemonPort() + "/" + SERVICESDAEMON_SYMBOL;
	}

	/**
	 * Composes a ServicesDaemon corbaloc for the specified host and port.
	 * @deprecated  because the daemon port is fixed.
	 */
	static public String convertToServicesDaemonLocation (String host, String port) {
		return convertToServicesDaemonLocation(host, port, "");
	}

	/**
	 * Composes a ServicesDaemon corbaloc for the specified host, port, and protocol.
	 * @deprecated  because the daemon port is fixed.
	 */
	static public String convertToServicesDaemonLocation (String host, String port, String protocol) {
		return "corbaloc:" + protocol + ":" + host.trim() + ":" + port.trim() + "/" + SERVICESDAEMON_SYMBOL;
	}

	
// ACSContainerDaemon

	/**
	 * The suffix for a ContainerDaemon location.
	 * <p>
	 * Note that this string constant is also defined in acsdaemon.idl 
	 * and would be accessible as "alma.acsdaemon.containerDaemonServiceName.value"
	 * if module acsdaemonidl would not come after jacsutil.
	 * @TODO clean up the module dependencies.
	 */
	static public final String CONTAINERDAEMON_SYMBOL = "ACSContainerDaemon";

	/**
	 * Composes a ContainerDaemon corbaloc for the specified host.
	 */
	static public String convertToContainerDaemonLocation (String host) {
		return "corbaloc::" + host.trim() + ":" + ACSPorts.getContainerDaemonPort() + "/" + CONTAINERDAEMON_SYMBOL;
	}

	/**
	 * Composes a ContainerDaemon corbaloc for the specified host and port.
	 * @deprecated  because the daemon port is fixed.
	 */
	static public String convertToContainerDaemonLocation (String host, String port) {
		return convertToContainerDaemonLocation(host, port, "");
	}

	/**
	 * Composes a ContainerDaemon corbaloc for the specified host, port, and protocol.
	 * @deprecated  because the daemon port is fixed.
	 */
	static public String convertToContainerDaemonLocation (String host, String port, String protocol) {
		return "corbaloc:" + protocol + ":" + host.trim() + ":" + port.trim() + "/" + CONTAINERDAEMON_SYMBOL;
	}
	
}

