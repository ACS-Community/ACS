/**
 * 
 */
package alma.TMCDB.alarm;

import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.cosylab.cdb.jdal.hibernate.ElementValue;
import com.cosylab.cdb.jdal.hibernate.ExtraDataFeature;
import com.cosylab.cdb.jdal.hibernate.InternalElementsMap;
import com.cosylab.cdb.jdal.hibernate.NameOverrideFeature;

/**
 * @author msekoranja
 *
 */
public class Category implements ExtraDataFeature, NameOverrideFeature {
	
	private String path;
	
    // hierarchical support
    // must be public to be accessible, but should not have getter to be come visible as node
    // since childred can be properties and subcomponent Object is used
	public Map<String, Object> _ = new InternalElementsMap<String, Object>();

    // extra data support
    private Element _extraData;
    
    /* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.ExtraDataFeature#getExtraData()
	 */
	public Element getExtraData() {
		return _extraData;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.NameOverrideFeature#getNameOverride()
	 */
	public String getNameOverride() {
		return "category";
	}
	
	public Category(String path, String description, boolean isDefault, String[] faultFamilies)
	{
		this.path = path;
		_.put("description", new ElementValue(description));

		// add is-element attribute (cannot be done directly because it is an invalid Java name)
		try
		{
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			DocumentBuilder builder = factory.newDocumentBuilder();
			DOMImplementation impl = builder.getDOMImplementation();
			Document xmldoc = impl.createDocument(null, "data", null);
			_extraData = xmldoc.getDocumentElement();
			_extraData.setAttribute("is-default", String.valueOf(isDefault));
		}
		catch (Throwable th)
		{
			th.printStackTrace();
		}
		
		// alarms
		StringBuffer alarms = new StringBuffer();
		for (String faultFamily : faultFamilies)
			alarms.append("<FaultFamily>").append(faultFamily).append("</FaultFamily>");
		_.put("alarms", new ElementValue(alarms.toString()));
	}

	/**
	 * @return the path
	 */
	public String getPath() {
		return path;
	}

}
