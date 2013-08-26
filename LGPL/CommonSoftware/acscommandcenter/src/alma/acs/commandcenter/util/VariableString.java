/*
 * Created on Oct 25, 2005 by mschilli
 */
package alma.acs.commandcenter.util;

import java.util.Map;



/**
 * Purpose of this class is to resolve embedded variables
 * in a source string like "${user.home}/config/${policydir}".
 * <p>
 * Invocation of the {@link #toString()} method will resolve the embedded
 * variables using the <em>Java system properties</em>. 
 * By using the {@link #toString(Map)} method you can specify which
 * dictionary to use. 
 * 
 * In both cases, the following holds: If a non-existing (or null-value)
 * variable is encountered, it will either a) be resolved to the empty string,
 * or b) an exception will be thrown. This depends on the boolean flag {@link #lenient}.
 * </p>
 * 
 * <p><strong>Note: </strong>
 * Relationship between {@link alma.acs.commandcenter.util.PreparedString}
 * and {@link alma.acs.commandcenter.util.VariableString}:
 * 
 * {@link alma.acs.commandcenter.util.PreparedString} uses <em>positional</em>
 * parameters to replace placeholders with the elements of a string array.
 * {@link alma.acs.commandcenter.util.VariableString} uses <em>named</em>
 * parameters to replace placeholders with values from a map.</p>
 */
public class VariableString {

	protected boolean lenient;
	protected String source;
	
	/**
	 * Constructs an instance with the given source, and a lenient way
	 * of dealing with unresolvable variables.
	 * 
	 * @param source string, e.g. "${user.home}/config/${policydir}".
	 */
	public VariableString(String source) {
		this(source, true);
	}

	/**
	 * Constructs an instance with the given source.
	 * 
	 * @param source string, e.g. "${user.home}/config/${policydir}".
	 * @param lenient whether unresolvable variables will provoke an exception
	 */
	public VariableString(String source, boolean lenient) {
		this.source = source;
		this.lenient = lenient;
	}


	// ================================================
	// API
	// ================================================


	/**
	 * Returns the <strong>non-</strong>resolved (source) version of this instance.
	 * @return the <strong>non-</strong>resolved (source) version of this instance.
	 */
	public String getSource() {
		return source;
	}
	
	/**
	 * Returns a <strong>resolved</strong> version of this instance,
	 * resolving is done using the <em>Java system properties</em>.
	 * 
	 * @return the <strong>resolved</strong> version of this instance.
	 * @throws UnresolvableException if variables cannot be resolved and we are non-{@link #lenient}
	 */
	@Override
	public String toString() throws UnresolvableException {
		return toString(System.getProperties());
	}
	
	/**
	 * Returns a <strong>resolved</strong> version of this instance,
	 * resolving is done using the specified map.
	 * 
	 * @return the <strong>resolved</strong> version of this instance.
	 * @throws UnresolvableException if variables cannot be resolved and we are non-{@link #lenient}
	 */
	public String toString (Map<?,Object> map) throws UnresolvableException {
		// wrap the given map with an ad-hoc IResolver
		final Map<?,Object> m = map;
		IResolver res = new IResolver() {
			public String resolve (String name) {
				Object value = m.get(name);
				return (value instanceof String)? (String)value : null;
			}
		};
		return resolveVariables(res, source);
	}

	/**
	 * Returns a <strong>resolved</strong> version of this instance,
	 * resolving is done using the specified IResolver.
	 * 
	 * @return the <strong>resolved</strong> version of this instance.
	 * @throws UnresolvableException if variables cannot be resolved and we are non-{@link #lenient}
	 */
	public String toString (IResolver map) throws UnresolvableException {
		return resolveVariables(map, source);
	}
	
	
	// ================================================
	// Internal
	// ================================================

	/** 
	 * Performs the replacement of embedded variable names, recursively.
	 * If a non-existing (or null-value) variable is encountered, this
	 * will either a) resolve to the empty string, or b) throw an exception,
	 * depending on the boolean flag {@link #lenient}.
	 */
	protected String resolveVariables (IResolver res, String value) throws UnresolvableException {
		
		if (value == null)
			return null;
		
		int markerStart = value.indexOf("${");
		
		if (markerStart != -1) {
			int markerEnd = value.indexOf("}", markerStart);
			if (markerEnd != -1) {

				// split into 3 parts
				String preVarName = value.substring(0, markerStart);
				String embeddedVarName = value.substring(markerStart + 2, markerEnd);
				String postVarName = value.substring(markerEnd + 1);
				
				// resolve middle part
				Object embeddedVarValue = res.resolve(embeddedVarName);
				if (embeddedVarValue == null) {
					
					// cannot resolve, how should we react?
					if (lenient) {
						embeddedVarValue = "";
					} else {
						throw new UnresolvableException(embeddedVarName);
					}
					
				}
				
				// concatenate again
				value = preVarName + embeddedVarValue + postVarName;
				
				// continue with next variable
				return (resolveVariables(res, value));
			}
		}
		return value;
	}
	
	

	//
	// =========== Inner Types ===========
	// 
	
	static public interface IResolver {
	
		public String resolve(String name);
		
	}

	
	static public class UnresolvableException extends RuntimeException {

		protected String variableName;
		
		protected UnresolvableException (String varname) {
			this.variableName = varname;
		}

		public String getVariableName() {
			return variableName;
		}
	}
	
}



