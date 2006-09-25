/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: ReductionLinkType.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * Class ReductionLinkType.
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 */
public class ReductionLinkType implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * Field _type
	 */
	private java.lang.String _type;

	/**
	 * parent FS
	 */
	private com.cosylab.acs.laser.dao.xml.Parent _parent;

	/**
	 * child FS
	 */
	private com.cosylab.acs.laser.dao.xml.Child _child;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public ReductionLinkType()
	{
		super();
	} // -- com.cosylab.acs.laser.dao.xml.ReductionLinkType()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Returns the value of field 'child'. The field 'child' has the following
	 * description: child FS
	 * 
	 * @return Child
	 * @return the value of field 'child'.
	 */
	public com.cosylab.acs.laser.dao.xml.Child getChild()
	{
		return this._child;
	} // -- com.cosylab.acs.laser.dao.xml.Child getChild()

	/**
	 * Returns the value of field 'parent'. The field 'parent' has the following
	 * description: parent FS
	 * 
	 * @return Parent
	 * @return the value of field 'parent'.
	 */
	public com.cosylab.acs.laser.dao.xml.Parent getParent()
	{
		return this._parent;
	} // -- com.cosylab.acs.laser.dao.xml.Parent getParent()

	/**
	 * Returns the value of field 'type'.
	 * 
	 * @return String
	 * @return the value of field 'type'.
	 */
	public java.lang.String getType()
	{
		return this._type;
	} // -- java.lang.String getType()

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
	 * Sets the value of field 'child'. The field 'child' has the following
	 * description: child FS
	 * 
	 * @param child
	 *            the value of field 'child'.
	 */
	public void setChild(com.cosylab.acs.laser.dao.xml.Child child)
	{
		this._child = child;
	} // -- void setChild(com.cosylab.acs.laser.dao.xml.Child)

	/**
	 * Sets the value of field 'parent'. The field 'parent' has the following
	 * description: parent FS
	 * 
	 * @param parent
	 *            the value of field 'parent'.
	 */
	public void setParent(com.cosylab.acs.laser.dao.xml.Parent parent)
	{
		this._parent = parent;
	} // -- void setParent(com.cosylab.acs.laser.dao.xml.Parent)

	/**
	 * Sets the value of field 'type'.
	 * 
	 * @param type
	 *            the value of field 'type'.
	 */
	public void setType(java.lang.String type)
	{
		this._type = type;
	} // -- void setType(java.lang.String)

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
		return (com.cosylab.acs.laser.dao.xml.ReductionLinkType) Unmarshaller
				.unmarshal(
						com.cosylab.acs.laser.dao.xml.ReductionLinkType.class,
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
