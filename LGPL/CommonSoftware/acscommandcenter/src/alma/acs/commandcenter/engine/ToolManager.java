/*
 * Created on Apr 7, 2004 by mschilli
 */
package alma.acs.commandcenter.engine;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import alma.acs.commandcenter.util.MiscUtils;
import alma.acs.commandcenter.util.PreparedString;
import alma.entity.xmlbinding.acscommandcentertools.AcsCommandCenterTools;
import alma.entity.xmlbinding.acscommandcentertools.Insertion;
import alma.entity.xmlbinding.acscommandcentertools.Tool;
import alma.entity.xmlbinding.acscommandcentertools.types.InsertionSourceType;

/**
 * @author mschilli
 */
public class ToolManager {


	static private Logger log = MiscUtils.getPackageLogger(ToolManager.class);


	// ======================================================
	// API 
	// ======================================================


	//
	// ====== "Extra" Tools =======
	//	

	/**
	 * Reads the tool definitions from the specified reader. The tool definitions that were
	 * in place before will be REPLACED by this.
	 */
	static public void readExtraTools (Reader f) throws Exception {

		// --- read in xml file
		BufferedReader r;
		r = new BufferedReader(f);
		extraTools = AcsCommandCenterTools.unmarshalAcsCommandCenterTools(r);
	}

	static public AcsCommandCenterTools getExtraTools () throws Exception {

		// attempt lazy reading of definition if no-one has called
		// readExtraTools() or readDefaultExtraTools() before
		if (extraTools.getToolCount() == 0) {
			ToolManager.readDefaultExtraTools();
		}

		return extraTools;
	}

	static public String getDefaultExtraToolsName () {
		return "AcsCommandCenterTools.xml";
	}

	static public void readDefaultExtraTools () throws Exception {
		URL url = ToolManager.class.getClassLoader().getResource(getDefaultExtraToolsName());
		InputStream is = url.openStream();
		InputStreamReader r = new InputStreamReader(is);
		readExtraTools(r);
	}


	//
	// ====== "Built-in" Tools =======
	//	


	/**
	 * Reads the tool definition from the specified reader. They are ADDED to an internal
	 * map that uses toolnames as it keys. This implies that if a read-in tool has a name
	 * that - after trim() - doesn't exactly match the required name , it will have no
	 * effect.
	 */
	static public void readBuiltinTools (Reader f) throws Exception {

		// --- read in xml file
		BufferedReader r;
		r = new BufferedReader(f);
		AcsCommandCenterTools tools = AcsCommandCenterTools.unmarshalAcsCommandCenterTools(r);

		// --- put all into map
		// (some of the previous definitions are likely to be overwritten
		// through this)
		for (int i = 0; i < tools.getToolCount(); i++) {
			Tool t = tools.getTool(i);
			builtinTools.put(t.getCaption().trim(), t);
		}
	}


	/**
	 * Returns the builtin tool with the specified name.
	 * <p>
	 * If no builtin-tool definition has yet been read in, this will try to read the
	 * default tools definition file (see <code>getDefaultBuiltinToolsName()</code>). If
	 * an exception occurs during that, it will be passed to the caller. To avoid this lazy
	 * attempy to read the default definitions, invoke one of
	 * <ul>
	 * <li><code>readDefaultBuiltinTools()</code>
	 * <li><code>readBuiltinTools(Reader)</code>
	 * </ul>
	 * before using this method.
	 * </p>
	 * 
	 * @param caption the name of a tool
	 * @return a tool descriptor as defined in the tools definition
	 * @throws IllegalArgumentException if non-defined tool is requested
	 * @throws Exception if no tool defined yet, and loading of default definition fails
	 */
	static public Tool getBuiltinTool (String caption) throws IllegalArgumentException, Exception {

		// attempt lazy reading of definition if no-one has called
		// readBuiltinTools() or readDefaultBuiltinTools() before
		if (builtinTools.size() == 0) {
			ToolManager.readDefaultBuiltinTools();
		}

		Tool ret;

		ret = (Tool) builtinTools.get(caption);
		if (ret == null) {
			throw new IllegalArgumentException("no such built-in tool: '" + caption + "'; currently defined built-in tools are: "
					+ builtinTools.keySet());
		}

		return ret;
	}

	static public String getDefaultBuiltinToolsName () {
		return "AcsCommandCenterBuiltinTools.xml";
	}



	static public void readDefaultBuiltinTools () throws Exception {
		URL url = ToolManager.class.getClassLoader().getResource(getDefaultBuiltinToolsName());
		InputStream is = url.openStream();
		InputStreamReader r = new InputStreamReader(is);
		readBuiltinTools(r);
	}


	
	//
	// ====== Command Generation =======
	//	

	
	static public String generateCommand (Tool tool, RunModel runModel) throws Exception {
		return generateCommand(tool, runModel, emptyMap);
	}

	static private HashMap<String,Object> emptyMap = new HashMap<String,Object>();

	static public String generateCommand (Tool tool, RunModel runModel, Map<String,Object> input) throws Exception {
		String ret;

		String[] pieces = collectInsertions(tool, runModel, input);
		PreparedString prep = new PreparedString(tool.getCommand());
		String command = prep.toString(pieces);

//		VariableString vari = new VariableString(command, false);
//		ret = vari.toString(getVariables());
		ret = command;
		
		return ret;
	}

	
	
	// ======================================================
	// Internal
	// ======================================================

	
	/**
	 * Initializes the builtinTools map with hardcoded definitions
	 */
	static {

		extraTools = new AcsCommandCenterTools();
		builtinTools = new HashMap<String, Tool>();

	}


	static protected AcsCommandCenterTools extraTools;

	static protected HashMap<String, Tool> builtinTools;


	static protected String[] collectInsertions (Tool tool, RunModel runModel, Map<String,Object> input) throws Exception {

		String[] ret = new String[tool.getInsertionCount()];
		for (int i = 0; i < ret.length; i++) {
			Insertion ins = tool.getInsertion(i);
			String name = (ins.getContent() != null) ? ins.getContent().trim() : "";
			String fallback = (ins.getDefault() != null) ? ins.getDefault().trim() : "";
			if (ins.getSource().equals(InsertionSourceType.MODEL))
				ret[i] = readFromModel(runModel, name, fallback);
			else
				ret[i] = readFromInput(input, name, fallback);
		}
		return ret;
	}

	static protected String readFromModel (RunModel runModel, String name, String fallback) throws Exception {
		try {

			Object ret = null;

			// --- normalization: strip "()", prepend "get"
			if (name.endsWith("()"))
				name = name.substring(0, name.length() - 2);
			if (!name.startsWith("get"))
				name = "get" + name;


			// --- find method and invoke
			Method m = RunModel.class.getDeclaredMethod(name, new Class[]{});
			ret = m.invoke(runModel, new Object[]{});

			if (ret == null || ret.equals("")) {
				ret = fallback;
			}

			log.finer("readFromModel('" + name + "','" + fallback + "') returns '" + ret + "'");
			return ret.toString();

		} catch (InvocationTargetException exc) {
			log.fine("readFromModel(" + name + "," + fallback + ") failed: " + exc);
			// in general, the root cause should be interesting to our callers.
			// this is particularly true if reading from model failed because of an
			// unresolvable variable. thus, we unwrap and throw the cause
			Throwable cause = exc.getCause(); 
			if (cause != null && cause instanceof Exception) {
				throw (Exception)cause;
			} else {
				throw exc;
			}
			
		} catch (Exception exc) {
			log.fine("readFromModel(" + name + "," + fallback + ") failed: " + exc);
			throw exc;
		}
	}

	static protected String readFromInput (Map<String, Object> input, String name, String fallback) {
		Object ret = null;
		ret = input.get(name);
		if (ret == null || ret.equals("")) {
			ret = fallback;
		}
		return ret.toString();
	}



}

