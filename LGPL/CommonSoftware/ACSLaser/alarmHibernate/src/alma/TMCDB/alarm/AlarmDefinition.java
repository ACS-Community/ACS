/**
 * 
 */
package alma.TMCDB.alarm;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.cosylab.cdb.jdal.hibernate.ExtraDataFeature;
import com.cosylab.cdb.jdal.hibernate.NameOverrideFeature;

/**
 * @author msekoranja
 *
 */
public class AlarmDefinition implements ExtraDataFeature, NameOverrideFeature {

    // extra data support
    private Element _extraData;
    
    /* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.ExtraDataFeature#getExtraData()
	 */
	public Element getExtraData() {
		return _extraData;
	}
	
	public AlarmDefinition(String faultFamilyName, String faultMemberName, String faultCode) {
		// add xmlns attributes
		try
		{
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			DocumentBuilder builder = factory.newDocumentBuilder();
			DOMImplementation impl = builder.getDOMImplementation();
			Document xmldoc = impl.createDocument(null, "data", null);
			_extraData = xmldoc.getDocumentElement();
			_extraData.setAttribute("fault-family", faultFamilyName);
			_extraData.setAttribute("fault-member", faultMemberName);
			_extraData.setAttribute("fault-code", faultCode);
		}
		catch (Throwable th)
		{
			th.printStackTrace();
		}
	}

	/* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.NameOverrideFeature#getNameOverride()
	 */
	public String getNameOverride() {
		return "alarm-definition";
	}

}
