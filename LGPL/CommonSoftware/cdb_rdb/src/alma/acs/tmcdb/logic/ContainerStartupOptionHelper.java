/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.tmcdb.logic;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.acs.tmcdb.ContStartOptType;
import alma.acs.tmcdb.Container;
import alma.acs.tmcdb.ContainerStartupOption;

/**
 * Encapsulates the translation of Container parameters between 
 * <ol>
 *   <li> The one-string version used by the CDB and container daemon 
 *        (where different levels of options are expressed via wrapper options like --passthroughProcessStart)
 *   <li> and the more atomic storage in the TMCDB as a set of {@link ContainerStartupOption} objects.
 * </ol>
 * In the future this class may also be used to provide lists of available options to the TMCDB Explorer.
 * <p>
 * @TODO Perhaps move this class to module acsstartup if we refactor this class's API to not use TMCDB pojos.
 * 
 * @author hsommer
 */
public class ContainerStartupOptionHelper {
	
	private final Logger logger;

	/**
	 * Wrapper for options targeted at the container application (passed to the container's main method).
	 */
	public static final String CONT_ARG_WRAPPER_OPTION = "--passthrough";
	
	/**
	 * Wrapper for options targeted at the language-specific container start script.
	 */
	public static final String EXEC_ARG_LANG_WRAPPER_OPTION = "--passthroughProcessStart";
	
	/**
	 * Default name used when storing a string of concatenated options, 
	 * typically from importing options that were specified before the TMCDB table ContainerStartupOption
	 * allowed for finer granularity and meaningful option names.
	 */
	public static final String OPTION_NAME_LEGACY_CONCATENATED = "LegacyOptionsConcat";
	
	
	/**
	 * Ctor that takes a logger. 
	 */
	public ContainerStartupOptionHelper(Logger logger) {
		this.logger = logger;
	}
	
	/**
	 * Converts a string of options to one or more ContainerStartupOption objects.
	 * <p>
	 * Parses out the wrapper options and creates specialized ContainerStartupOption instances for their contents.
	 * @TODO: Parse out known options such as '-maxHeapSize' into separate ContainerStartupOptions,
	 *        so that we can use nicer OptionNames than OPTION_NAME_LEGACY_CONCATENATED.
	 * 
	 * @param container  The container whose flags we convert (will be set on the created <code>ContainerStartupOption</code>.
	 * @param flags  The flags string, as it comes from the CDB (Container.DeployInfo.Flags) or from the WDAL interface. May be null.
	 * @return  
	 */
	public Collection<ContainerStartupOption> convertFlagsString(Container container, String flags) {
		List<ContainerStartupOption> ret = new ArrayList<ContainerStartupOption>();
		
		if (flags != null) {
			flags = flags.trim();
			if (flags.length() > 0) {
				WrapperOptionParser wop = new WrapperOptionParser();
				String verbatimOptions = "";
				try {
					wop.parseAll(flags);
					if (!wop.getWrappedOptionsContainerExecutable().isEmpty()) {
						ContainerStartupOption containerStartupOption = new ContainerStartupOption();
						ret.add(containerStartupOption);
						containerStartupOption.setContainer(container);
						containerStartupOption.setOptionType(ContStartOptType.EXEC_ARG_LANG);
						containerStartupOption.setOptionName(OPTION_NAME_LEGACY_CONCATENATED);
						containerStartupOption.setOptionValue(wop.getWrappedOptionsContainerExecutable());
					}
					if (!wop.getWrappedOptionsContainerArgs().isEmpty()) {
						ContainerStartupOption containerStartupOption = new ContainerStartupOption();
						ret.add(containerStartupOption);
						containerStartupOption.setContainer(container);
						containerStartupOption.setOptionType(ContStartOptType.CONT_ARG);
						containerStartupOption.setOptionName(OPTION_NAME_LEGACY_CONCATENATED);
						containerStartupOption.setOptionValue(wop.getWrappedOptionsContainerArgs());
					}
					verbatimOptions = wop.getRemainingOptions();
				} catch (IllegalArgumentException ex) {
					logger.log(Level.WARNING, "Failed to parse container options '" + flags + "'. Will leave them as verbatim string.");
					verbatimOptions = flags;
				}
				if (!verbatimOptions.isEmpty()) {
					ContainerStartupOption containerStartupOption = new ContainerStartupOption();
					ret.add(containerStartupOption);
					containerStartupOption.setContainer(container);
					containerStartupOption.setOptionType(ContStartOptType.EXEC_ARG);
					containerStartupOption.setOptionName(OPTION_NAME_LEGACY_CONCATENATED);
					containerStartupOption.setOptionValue(verbatimOptions);
				}
			}
		}
		return ret;
	}
	
	/**
	 * Parses out the options wrapped by <code>--passthroughProcessStart</code>
	 * and <code>--passthrough</code> from an option string, as well as the remaining unwrapped options.
	 */
	static class WrapperOptionParser {
		private String wrappedOptionsContainerExecutable;
		private String wrappedOptionsContainerArgs;
		
		private String remainingOptions;

		WrapperOptionParser() {
			wrappedOptionsContainerExecutable = "";
			wrappedOptionsContainerArgs = "";
			remainingOptions = "";
		}

		/**
		 * Parses all wrapper options and makes results available through subsequent calls to
		 * {@link #getWrappedOptionsContainerExecutable()}, {@link #getWrappedOptionsContainerArgs()},
		 * and {@link #getRemainingOptions()}.
		 * @param flags
		 * @throws IllegalArgumentException if wrapper options do not wrap the underlying options in a pair of single or double quotes.
		 */
		void parseAll(String flags) {
			// first parse out "passthroughProcessStart" and remove that wrapper from the option string, 
			// because parsing for "passthrough" first would get confused by "passthroughProcessStart".
			wrappedOptionsContainerExecutable = parse(flags, EXEC_ARG_LANG_WRAPPER_OPTION);
			wrappedOptionsContainerArgs = parse(remainingOptions, CONT_ARG_WRAPPER_OPTION);
		}
		
		/**
		 * Tries to extract options even from multiple occurrences of wrapper options,
		 * although the container daemon does not (verify this!) support this,
		 * so that it should not happen in practice. 
		 * <p>
		 * Returns the wrapped options, and stores the remaining option string in 
		 * {@link #remainingOptions}.
		 * 
		 * @param flags  The option string that may contain wrapper options.
		 * @param wrapperOptionName  Should be {@link #EXEC_ARG_LANG_WRAPPER_OPTION} or {@link #CONT_ARG_WRAPPER_OPTION}.
		 * @return the wrapped options, or empty string.
		 * @throws IllegalArgumentException if wrapper options do not wrap the underlying options in a pair of single or double quotes.
		 */
		private String parse(String flags, String wrapperOptionName) {
			String wrappedOptions = "";
			remainingOptions = "";
			if (flags != null) {
				int indexRemainingOptionsBegin = 0;
				int indexWrapperOption = flags.indexOf(wrapperOptionName);
				while (indexWrapperOption >= 0) {
					char wrapperQuoteChar = '"';
					// find opening quotes, and assert that only space and '=' lie between the wrapper option and the quotes
					int indexOptionBeginQuote = -1;
					for (int i = indexWrapperOption + wrapperOptionName.length(); i < flags.length(); i++) {
						char c = flags.charAt(i);
						if (c == '\"' || c == '\'') {
							wrapperQuoteChar = c;
							indexOptionBeginQuote = i;
							break;
						}
						if (c != ' ' && c != '=') {
							// bad, will lead to IllegalArgumentException (indexOptionBeginQuote == -1)
							break;
						}
					}
					if (indexOptionBeginQuote < 0) {
						throw new IllegalArgumentException("Wrapper option at pos." + indexWrapperOption + " must be followed by '=' and single or double quotes.");
					}
					int indexOptionEndQuote = flags.indexOf(wrapperQuoteChar, indexOptionBeginQuote + 1);
					if (indexOptionEndQuote < 0) {
						throw new IllegalArgumentException("Wrapper option at pos." + indexWrapperOption + 
								" must be followed by '=' and a pair of '" + wrapperQuoteChar + "' chars around the wrapped options.");
					}
					String option = flags.substring(indexOptionBeginQuote + 1, indexOptionEndQuote).trim();
	
					wrappedOptions += option + " ";
					remainingOptions += flags.substring(indexRemainingOptionsBegin, indexWrapperOption);
					indexRemainingOptionsBegin = indexOptionEndQuote + 1;
					indexWrapperOption = flags.indexOf(wrapperOptionName, indexOptionEndQuote);
				}
				if (indexRemainingOptionsBegin < flags.length()) {
					remainingOptions += flags.substring(indexRemainingOptionsBegin).trim();
				}
				remainingOptions = remainingOptions.trim();
			}
			return wrappedOptions.trim();
		}
		
		/**
		 * Returns the parsed options found inside <code>--passthroughProcessStart</code> wrapper option.
		 * Call {@link #parseAll(String)} first.
		 */
		String getWrappedOptionsContainerExecutable() {
			return wrappedOptionsContainerExecutable;
		}

		/**
		 * Returns the parsed options found inside <code>--passthrough</code> wrapper options.
		 * Call {@link #parseAll(String)} first.
		 */
		String getWrappedOptionsContainerArgs() {
			return wrappedOptionsContainerArgs;
		}

		/**
		 * Returns the options that were not wrapped.
		 * Call {@link #parseAll(String)} first.
		 */
		String getRemainingOptions() {
			return remainingOptions;
		}
	}


	/**
	 * Converts a list of ContainerStartupOption to a flat option string
	 * that can be passed to the container daemon or used to satisfy DAL calls.
	 * @param options
	 * @return  Options in one string, wrapped as needed. Possibly empty string, never null.
	 * @throws IllegalArgumentException if an option references a different container than the other options 
	 *                                  (all refs null is OK though)
	 */
	public String convertContainerStartupOptions(Collection<ContainerStartupOption> options) {
		if (options == null || options.isEmpty()) {
			logger.finer("convertContainerStartupOptions called without options.");
			return "";
		}
		
		String execArgs = "";
		String execArgsLang = "";
		String contArgs = "";
		Container commonContainer = null;
		
		for (ContainerStartupOption option : options) {
			// validate container ref
			if (commonContainer == null) {
				commonContainer = option.getContainer();
			}
			else if (option.getContainer() != commonContainer) {
				throw new IllegalArgumentException();
			}
			
			// gather by option type
			if (option.getOptionType().equals(ContStartOptType.ENV_VAR)) {
				logger.warning("Ignoring option of type " + ContStartOptType.ENV_VAR);
			}
			else if (option.getOptionType().equals(ContStartOptType.EXEC_ARG)) {
				execArgs += " " + option.getOptionValue();
			}
			else if (option.getOptionType().equals(ContStartOptType.EXEC_ARG_LANG)) {
				execArgsLang += " " + option.getOptionValue();
			}
			else if (option.getOptionType().equals(ContStartOptType.CONT_ARG)) {
				contArgs += " " + option.getOptionValue();
			}
		}
		// wrap and concatenate the options
		String ret = execArgs.trim();
		if (execArgsLang.length() > 0) {
			ret += " " + EXEC_ARG_LANG_WRAPPER_OPTION + "=\"" + execArgsLang.trim() + "\"";
		}
		if (contArgs.length() > 0) {
			ret += " " + CONT_ARG_WRAPPER_OPTION + "=\"" + contArgs.trim() + "\"";
		}
		
		if (commonContainer != null && logger.isLoggable(Level.FINER)) {
			logger.finer(options.size() + " options for container " + commonContainer + " flattened: " + ret.trim());
		}
		return ret.trim();
	}
}
