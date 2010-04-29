package alma.acs.tmcdb.logic;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Logger;

import com.cosylab.cdb.jdal.logging.AcsLoggerHelper;

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
	 * These values correspond to the allowed content of attribute 'OptionType' in table ContainerStartupOption.
	 */
	public static enum OptionType {
		ENV_VAR,
		EXEC_ARG,
		EXEC_ARG_LANG,
		CONT_ARG
	}
	
	public static final String CONT_ARG_WRAPPER_OPTION = "--passthrough";
	public static final String EXEC_ARG_LANG_WRAPPER_OPTION = "--passthroughProcessStart";
	
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
	 * TODO: Parse out at least the wrapper options and create separate ContainerStartupOption instances for them.
	 *       Or even parse other options into separate ContainerStartupOptions.
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
				// stuff all options into one ContainerStartupOption of type EXEC_ARG.
				ContainerStartupOption containerStartupOption = new ContainerStartupOption();
				ret.add(containerStartupOption);
				containerStartupOption.setContainer(container);
				containerStartupOption.setOptionType(OptionType.EXEC_ARG.toString());
				containerStartupOption.setOptionName(OPTION_NAME_LEGACY_CONCATENATED);
				containerStartupOption.setOptionValue(flags);
			}
		}
		return ret;
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
			if (option.getOptionType().equals(OptionType.ENV_VAR.toString())) {
				logger.warning("Ignoring option of type " + OptionType.ENV_VAR);
			}
			else if (option.getOptionType().equals(OptionType.EXEC_ARG.toString())) {
				execArgs += " " + option.getOptionValue();
			}
			else if (option.getOptionType().equals(OptionType.EXEC_ARG_LANG.toString())) {
				execArgsLang += " " + option.getOptionValue();
			}
			else if (option.getOptionType().equals(OptionType.CONT_ARG.toString())) {
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
		
		return ret.trim();
	}
}
