/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: SourceDefinitionType.java,v 1.2 2005/06/14 15:21:12 mslenc Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * Class SourceDefinitionType.
 * 
 * @version $Revision: 1.2 $ $Date: 2005/06/14 15:21:12 $
 */
public class SourceDefinitionType implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * source description
	 */
	private java.lang.String _description;

	/**
	 * source backup timeout
	 */
	private int _connectionTimeout;

	/**
	 * keeps track of state for field: _connectionTimeout
	 */
	private boolean _has_connectionTimeout;

	/**
	 * source responsible person CERN id
	 */
	private int _responsibleId;

	/**
	 * keeps track of state for field: _responsibleId
	 */
	private boolean _has_responsibleId;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public SourceDefinitionType()
	{
		super();
	} // -- com.cosylab.acs.laser.dao.xml.SourceDefinitionType()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Method deleteConnectionTimeout
	 * 
	 */
	public void deleteConnectionTimeout()
	{
		this._has_connectionTimeout = false;
	} // -- void deleteConnectionTimeout()

	/**
	 * Method deleteResponsibleId
	 * 
	 */
	public void deleteResponsibleId()
	{
		this._has_responsibleId = false;
	} // -- void deleteResponsibleId()

	/**
	 * Returns the value of field 'connectionTimeout'. The field
	 * 'connectionTimeout' has the following description: source backup timeout
	 * 
	 * @return int
	 * @return the value of field 'connectionTimeout'.
	 */
	public int getConnectionTimeout()
	{
		return this._connectionTimeout;
	} // -- int getConnectionTimeout()

	/**
	 * Returns the value of field 'description'. The field 'description' has the
	 * following description: source description
	 * 
	 * @return String
	 * @return the value of field 'description'.
	 */
	public java.lang.String getDescription()
	{
		return this._description;
	} // -- java.lang.String getDescription()

	/**
	 * Returns the value of field 'responsibleId'. The field 'responsibleId' has
	 * the following description: source responsible person CERN id
	 * 
	 * @return int
	 * @return the value of field 'responsibleId'.
	 */
	public int getResponsibleId()
	{
		return this._responsibleId;
	} // -- int getResponsibleId()

	/**
	 * Method hasConnectionTimeout
	 * 
	 * 
	 * 
	 * @return boolean
	 */
	public boolean hasConnectionTimeout()
	{
		return this._has_connectionTimeout;
	} // -- boolean hasConnectionTimeout()

	/**
	 * Method hasResponsibleId
	 * 
	 * 
	 * 
	 * @return boolean
	 */
	public boolean hasResponsibleId()
	{
		return this._has_responsibleId;
	} // -- boolean hasResponsibleId()

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
	 * Sets the value of field 'connectionTimeout'. The field
	 * 'connectionTimeout' has the following description: source backup timeout
	 * 
	 * @param connectionTimeout
	 *            the value of field 'connectionTimeout'.
	 */
	public void setConnectionTimeout(int connectionTimeout)
	{
		this._connectionTimeout = connectionTimeout;
		this._has_connectionTimeout = true;
	} // -- void setConnectionTimeout(int)

	/**
	 * Sets the value of field 'description'. The field 'description' has the
	 * following description: source description
	 * 
	 * @param description
	 *            the value of field 'description'.
	 */
	public void setDescription(java.lang.String description)
	{
		this._description = description;
	} // -- void setDescription(java.lang.String)

	/**
	 * Sets the value of field 'responsibleId'. The field 'responsibleId' has
	 * the following description: source responsible person CERN id
	 * 
	 * @param responsibleId
	 *            the value of field 'responsibleId'.
	 */
	public void setResponsibleId(int responsibleId)
	{
		this._responsibleId = responsibleId;
		this._has_responsibleId = true;
	} // -- void setResponsibleId(int)

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
		return (com.cosylab.acs.laser.dao.xml.SourceDefinitionType) Unmarshaller
				.unmarshal(
						com.cosylab.acs.laser.dao.xml.SourceDefinitionType.class,
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
