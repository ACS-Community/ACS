/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: ReductionDefinitions.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * LASER reduction relationship definitions
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 */
public class ReductionDefinitions implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * Field _linksToCreate
	 */
	private com.cosylab.acs.laser.dao.xml.LinksToCreate _linksToCreate;

	/**
	 * Field _linksToRemove
	 */
	private com.cosylab.acs.laser.dao.xml.LinksToRemove _linksToRemove;

	/**
	 * Field _thresholds
	 */
	private com.cosylab.acs.laser.dao.xml.Thresholds _thresholds;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public ReductionDefinitions()
	{
		super();
	} // -- com.cosylab.acs.laser.dao.xml.ReductionDefinitions()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Returns the value of field 'linksToCreate'.
	 * 
	 * @return LinksToCreate
	 * @return the value of field 'linksToCreate'.
	 */
	public com.cosylab.acs.laser.dao.xml.LinksToCreate getLinksToCreate()
	{
		return this._linksToCreate;
	} // -- com.cosylab.acs.laser.dao.xml.LinksToCreate getLinksToCreate()

	/**
	 * Returns the value of field 'linksToRemove'.
	 * 
	 * @return LinksToRemove
	 * @return the value of field 'linksToRemove'.
	 */
	public com.cosylab.acs.laser.dao.xml.LinksToRemove getLinksToRemove()
	{
		return this._linksToRemove;
	} // -- com.cosylab.acs.laser.dao.xml.LinksToRemove getLinksToRemove()

	/**
	 * Returns the value of field 'thresholds'.
	 * 
	 * @return Thresholds
	 * @return the value of field 'thresholds'.
	 */
	public com.cosylab.acs.laser.dao.xml.Thresholds getThresholds()
	{
		return this._thresholds;
	} // -- com.cosylab.acs.laser.dao.xml.Thresholds getThresholds()

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
	 * Sets the value of field 'linksToCreate'.
	 * 
	 * @param linksToCreate
	 *            the value of field 'linksToCreate'.
	 */
	public void setLinksToCreate(
			com.cosylab.acs.laser.dao.xml.LinksToCreate linksToCreate)
	{
		this._linksToCreate = linksToCreate;
	} // -- void setLinksToCreate(com.cosylab.acs.laser.dao.xml.LinksToCreate)

	/**
	 * Sets the value of field 'linksToRemove'.
	 * 
	 * @param linksToRemove
	 *            the value of field 'linksToRemove'.
	 */
	public void setLinksToRemove(
			com.cosylab.acs.laser.dao.xml.LinksToRemove linksToRemove)
	{
		this._linksToRemove = linksToRemove;
	} // -- void setLinksToRemove(com.cosylab.acs.laser.dao.xml.LinksToRemove)

	/**
	 * Sets the value of field 'thresholds'.
	 * 
	 * @param thresholds
	 *            the value of field 'thresholds'.
	 */
	public void setThresholds(
			com.cosylab.acs.laser.dao.xml.Thresholds thresholds)
	{
		this._thresholds = thresholds;
	} // -- void setThresholds(com.cosylab.acs.laser.dao.xml.Thresholds)

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
		return (com.cosylab.acs.laser.dao.xml.ReductionDefinitions) Unmarshaller
				.unmarshal(
						com.cosylab.acs.laser.dao.xml.ReductionDefinitions.class,
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
