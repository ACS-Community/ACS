/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: Location.java,v 1.2 2005/06/14 15:21:12 mslenc Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * FS location
 * 
 * @version $Revision: 1.2 $ $Date: 2005/06/14 15:21:12 $
 */
public class Location extends LocationType implements java.io.Serializable
{

	// ----------------/
	// - Constructors -/
	// ----------------/

	public Location()
	{
		super();
	} // -- com.cosylab.acs.laser.dao.xml.Location()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Method isValid
	 * 
	 * 
	 * 
	 * @return boolean
	 */
	public boolean isValid()
	{
		try {
			validate();
		} catch (org.exolab.castor.xml.ValidationException vex) {
			return false;
		}
		return true;
	} // -- boolean isValid()

	/**
	 * Method marshal
	 * 
	 * 
	 * 
	 * @param out
	 */
	public void marshal(java.io.Writer out)
			throws org.exolab.castor.xml.MarshalException,
			org.exolab.castor.xml.ValidationException
	{

		Marshaller.marshal(this, out);
	} // -- void marshal(java.io.Writer)

	/**
	 * Method marshal
	 * 
	 * 
	 * 
	 * @param handler
	 */
	public void marshal(org.xml.sax.ContentHandler handler)
			throws java.io.IOException, org.exolab.castor.xml.MarshalException,
			org.exolab.castor.xml.ValidationException
	{

		Marshaller.marshal(this, handler);
	} // -- void marshal(org.xml.sax.ContentHandler)

	/**
	 * Method unmarshal
	 * 
	 * 
	 * 
	 * @param reader
	 * @return Object
	 */
	public static java.lang.Object unmarshal(java.io.Reader reader)
			throws org.exolab.castor.xml.MarshalException,
			org.exolab.castor.xml.ValidationException
	{
		return (com.cosylab.acs.laser.dao.xml.Location) Unmarshaller.unmarshal(
				com.cosylab.acs.laser.dao.xml.Location.class, reader);
	} // -- java.lang.Object unmarshal(java.io.Reader)

	/**
	 * Method validate
	 * 
	 */
	public void validate() throws org.exolab.castor.xml.ValidationException
	{
		org.exolab.castor.xml.Validator validator = new org.exolab.castor.xml.Validator();
		validator.validate(this);
	} // -- void validate()

}
