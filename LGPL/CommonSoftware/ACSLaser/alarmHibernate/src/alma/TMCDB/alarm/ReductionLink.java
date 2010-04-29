/**
 * 
 */
package alma.TMCDB.alarm;

import java.util.Map;

import com.cosylab.cdb.jdal.hibernate.InternalElementsMap;
import com.cosylab.cdb.jdal.hibernate.NameOverrideFeature;

/**
 * @author msekoranja
 *
 */
public class ReductionLink implements NameOverrideFeature {
	
	private String type;
	
    // hierarchical support
    // must be public to be accessible, but should not have getter to be come visible as node
    // since childred can be properties and subcomponent Object is used
	public Map<String, Object> _ = new InternalElementsMap<String, Object>();

	/* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.NameOverrideFeature#getNameOverride()
	 */
	public String getNameOverride() {
		return "reduction-link";
	}
	
	public class HierarchyElement
	{
		public Map<String, Object> _ = new InternalElementsMap<String, Object>();
		
		public HierarchyElement(String name, Object element)
		{
			_.put(name, element);
		}
	}
	
	public ReductionLink(String type, AlarmDefinition parent, AlarmDefinition child)
	{
		this.type = type;
		_.put("parent", new HierarchyElement("alarm-definition", parent));
		_.put("child", new HierarchyElement("alarm-definition", child));
	}

	/**
	 * @return the type
	 */
	public String getType() {
		return type;
	}

}
