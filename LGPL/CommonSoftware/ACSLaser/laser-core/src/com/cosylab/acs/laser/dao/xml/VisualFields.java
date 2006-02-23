/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: VisualFields.java,v 1.2 2005/06/14 15:21:12 mslenc Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * FS visual fields
 * 
 * @version $Revision: 1.2 $ $Date: 2005/06/14 15:21:12 $
 */
public class VisualFields implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * The system type name
	 */
	private java.lang.String _systemName;

	/**
	 * The system identification
	 */
	private java.lang.String _identifier;

	/**
	 * The system fault description
	 */
	private java.lang.String _problemDescription;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public VisualFields()
	{
		super();
	} // -- com.cosylab.acs.laser.dao.xml.VisualFields()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Returns the value of field 'identifier'. The field 'identifier' has the
	 * following description: The system identification
	 * 
	 * @return String
	 * @return the value of field 'identifier'.
	 */
	public java.lang.String getIdentifier()
	{
		return this._identifier;
	} // -- java.lang.String getIdentifier()

	/**
	 * Returns the value of field 'problemDescription'. The field
	 * 'problemDescription' has the following description: The system fault
	 * description
	 * 
	 * @return String
	 * @return the value of field 'problemDescription'.
	 */
	public java.lang.String getProblemDescription()
	{
		return this._problemDescription;
	} // -- java.lang.String getProblemDescription()

	/**
	 * Returns the value of field 'systemName'. The field 'systemName' has the
	 * following description: The system type name
	 * 
	 * @return String
	 * @return the value of field 'systemName'.
	 */
	public java.lang.String getSystemName()
	{
		return this._systemName;
	} // -- java.lang.String getSystemName()

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
	 * Sets the value of field 'identifier'. The field 'identifier' has the
	 * following description: The system identification
	 * 
	 * @param identifier
	 *            the value of field 'identifier'.
	 */
	public void setIdentifier(java.lang.String identifier)
	{
		this._identifier = identifier;
	} // -- void setIdentifier(java.lang.String)

	/**
	 * Sets the value of field 'problemDescription'. The field
	 * 'problemDescription' has the following description: The system fault
	 * description
	 * 
	 * @param problemDescription
	 *            the value of field 'problemDescription'.
	 */
	public void setProblemDescription(java.lang.String problemDescription)
	{
		this._problemDescription = problemDescription;
	} // -- void setProblemDescription(java.lang.String)

	/**
	 * Sets the value of field 'systemName'. The field 'systemName' has the
	 * following description: The system type name
	 * 
	 * @param systemName
	 *            the value of field 'systemName'.
	 */
	public void setSystemName(java.lang.String systemName)
	{
		this._systemName = systemName;
	} // -- void setSystemName(java.lang.String)

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
		return (com.cosylab.acs.laser.dao.xml.VisualFields) Unmarshaller
				.unmarshal(com.cosylab.acs.laser.dao.xml.VisualFields.class,
						reader);
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
