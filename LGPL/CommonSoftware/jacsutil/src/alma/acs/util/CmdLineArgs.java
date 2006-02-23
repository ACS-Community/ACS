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
package alma.acs.util;

import java.util.*;

/**
 * Deals with commandline options (<code>String[] args</code>), 
 * for example to merge several sets of option specifications.
 * 
 * An option consists of a key arg and 0...many value arguments.
 * The method <code>parseArgs</code> will attempt to guess which arguments
 * are keys and which are values, e.g. looking for a "-" prefix.
 * This class can therefore be used for unknown sets of options. 
 * <p>
 * To avoid fooling the algorithm with weird values (like "-273"), the expected
 * option keys with their minimum number of values can be specified using the
 * <code>registerOption</code> methods. 
 * This mechanism can later be extended to allow validation of required arguments.
 * <p>
 * TODO: logging and error handling
 * 
 * @author hsommer
 */
public class CmdLineArgs 
{
	private Map m_registeredOptions;
	
	// Map({CmdLineOption or String} key, String[] values)
	private LinkedHashMap m_options;

	public CmdLineArgs()
	{
		reset();
	}
	
	public void reset()
	{
		m_registeredOptions = new HashMap();
		m_options = new LinkedHashMap();
	}
	
	///////// configuration ///////////////
	
	public void registerOption(String name, int minValuesCount)
	{
		CmdLineRegisteredOption opt = new CmdLineRegisteredOption(name, minValuesCount);
		registerOption(opt);
	}
	
	public void registerOption(CmdLineRegisteredOption opt)
	{
		if (opt == null) {
			// todo 
		}
		else {
			m_registeredOptions.put(opt.getName(), opt);
		}
		
		if (opt.getAlternativeName() != null) {
			m_registeredOptions.put(opt.getAlternativeName(), opt);
		}
	}

	
	///////// execution ///////////////////

	/**
	 * If called more than once without calling <code>reset()</code> in between,
	 * the options are merged in the style of a <code>Map</code>.
	 * @param args
	 */
	public void parseArgs(String[] args)
	{
		if (args == null) {
			return;
		}
		
		int i = 0;
		while (i < args.length)
		{
			if (!isKey(args, i))
			{
				System.out.println("stray option value " + args[i] + " found outside of any key.");
				// cope with a hererogeneous map for the sake of not losing any arg
				m_options.put(args[i], args[i]);
				i++;
			}
			else
			{
				CmdLineOption opt = (CmdLineOption) m_registeredOptions.get(args[i]);
				if (opt == null)
				{
					opt = new CmdLineOption(args[i]);
				}
				
				ArrayList values = new ArrayList();
				
				int lastValIndex = i; 
				if (opt instanceof CmdLineRegisteredOption)
				{
					CmdLineRegisteredOption regOpt = (CmdLineRegisteredOption) opt;
					lastValIndex = i + regOpt.getMinValueCount();
					if (lastValIndex >= args.length)
					{
						System.out.println("warning: less than the required " + regOpt.getMinValueCount() +
													" values given for option " + regOpt.getName());
					}
				}
				// collect the values for the current option key
				i++;
				while (i < args.length && (i <= lastValIndex || !isKey(args, i)))
				{
					values.add(args[i]);
					i++;
				}
				
				String[] valueArray = (String[]) values.toArray(new String[0]);
				
				m_options.put(opt, valueArray);
			}
		}
	}
	
	
	/**
	 * Returns all arguments (both options and values). 
	 * If arguments were parsed more than once, the values from a later call overwrite earlier ones.
	 * Options and values that could not be distinguished during parsing appear in the original order.
	 * @return
	 */
	public String[] getAllArgs()
	{
		ArrayList args = new ArrayList();
		if (m_options != null)
		{
			for (Iterator iter = m_options.keySet().iterator(); iter.hasNext();)
			{
				Object element = iter.next();
				if (element instanceof String)
				{
					args.add(element);
				}
				else
				{
					CmdLineOption clo = (CmdLineOption) element;
					args.add(clo.getName());
					String[] values = (String[]) m_options.get(clo);
					args.addAll(Arrays.asList(values));
				}
			}
		}
		String[] argArray = (String[]) args.toArray(new String[0]);
		return argArray;
	}
	
	/**
	 * Gets all arguments that were recognized by the parser.
	 * This includes arguments registered before parsing, as well as arguments recognized automatically.
	 * The values for these args can be obtained from {@link #getValues(CmdLineOption)}.
	 * @return
	 */
	public CmdLineOption[] getRecognizedArgs() {
		ArrayList args = new ArrayList();
		if (m_options != null) {
			for (Iterator iter = m_options.keySet().iterator(); iter.hasNext();) {
				Object element = iter.next();
				if (element instanceof CmdLineOption) {
					CmdLineOption clo = (CmdLineOption) element;
					args.add(clo);
				}
			}
		}
		CmdLineOption[] argArray = (CmdLineOption[]) args.toArray(new CmdLineOption[0]);
		return argArray;
	}
	
	
	/**
	 * Returns <code>true</code> if the given option <code>clo</code> appeared in the argument lists that were 
	 * passed to <code>parseArgs</code>; <code>false</code> otherwise.
	 * 
	 * @param clo
	 * @return boolean
	 */
	public boolean isSpecified(CmdLineRegisteredOption clo)
	{
		return (m_options.get(clo) != null );
	}
	
	public String[] getValues(CmdLineOption clo)
	{
		return ( (String[]) m_options.get(clo) );
	}
	

	/**
	 * Decides whether args[index] is a key or a value.
	 * Future implementations might need to look at args[other indices], 
	 * that's why the full array is supplied.
	 * @param args
	 * @param index
	 * @return boolean
	 */
	private boolean isKey(String[] args, int index)
	{
		// first check the registered options
		if (m_registeredOptions.containsKey(args[index]))
		{
			return true;
		}
		// then apply some heuristics that tells options (keys) from values
		else if (args[index].startsWith("-"))
		{
			return true;
		}
		// did not pass for a key
		else
		{
			return false;
		}
	}
	
}

