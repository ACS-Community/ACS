package alma.acs.nc;

import java.util.logging.Logger;

import org.omg.CosNotification.EventType;
import org.omg.CosNotifyFilter.ConstraintInfo;
import org.omg.CosNotifyFilter.Filter;
import org.omg.CosNotifyFilter.FilterAdminOperations;
import org.omg.CosNotifyFilter.FilterNotFound;

/**
 * Gets info about NC filters of a selected proxy supplier or other FilterAdmin object. 
 * To be used for debug logging, eventGUI etc.
 */
public class NcFilterInspector
{
	private final Logger logger;
	private final String serverObjectName;
	private final FilterAdminOperations serverObject;

	
	/**
	 * @param serverObject The proxy supplier (or possibly other NC-satellite object) 
	 * @param serverObjectName A descriptive name for the serverObject.
	 * @param logger Used for internal logging.
	 */
	public NcFilterInspector(FilterAdminOperations serverObject, String serverObjectName, Logger logger) {
		this.serverObject = serverObject;
		this.serverObjectName = serverObjectName;
		this.logger = logger;
	}
	
	public String getFilterInfo() {
		
		String ret = "NC filter info for '" + serverObjectName + "' ";
		
		int[] filterIds = serverObject.get_all_filters();
		ret += "(" + filterIds.length + " filters total): ";
		
		for (int filterId : filterIds) {
			ret += "{ filterId=" + filterId +"; ";
			try {
				Filter f = serverObject.get_filter(filterId);
				String grammarName = f.constraint_grammar();
				ret += "grammar=" + grammarName +"; ";
				ret += "constraints: { ";
				for (ConstraintInfo ci : f.get_all_constraints()) {
					String cExp = ci.constraint_expression.constraint_expr;
					ret += "expr=" + cExp + "; ";
					for (EventType eT : ci.constraint_expression.event_types) {
						String typeName = eT.type_name;
						ret += "type=" + typeName + "; ";
						String domainName = eT.domain_name;
						ret += "domain=" + domainName + "; ";
					}
				}
				ret += "}; ";
			} catch (FilterNotFound ex) {
				ret += "FilterNotFound!";
			}
			ret += "}; ";
		}
		
		return ret;
	}


}
