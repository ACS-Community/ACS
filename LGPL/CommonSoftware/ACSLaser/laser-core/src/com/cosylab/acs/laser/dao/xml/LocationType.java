/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: LocationType.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * Class LocationType.
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 */
public class LocationType implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * building number
	 */
	private java.lang.String _building;

	/**
	 * floor number
	 */
	private java.lang.String _floor;

	/**
	 * room number
	 */
	private java.lang.String _room;

	/**
	 * building mnemonic
	 */
	private java.lang.String _mnemonic;

	/**
	 * FS position
	 */
	private java.lang.String _position;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public LocationType()
	{
		super();
	} // -- com.cosylab.acs.laser.dao.xml.LocationType()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Returns the value of field 'building'. The field 'building' has the
	 * following description: building number
	 * 
	 * @return String
	 * @return the value of field 'building'.
	 */
	public java.lang.String getBuilding()
	{
		return this._building;
	} // -- java.lang.String getBuilding()

	/**
	 * Returns the value of field 'floor'. The field 'floor' has the following
	 * description: floor number
	 * 
	 * @return String
	 * @return the value of field 'floor'.
	 */
	public java.lang.String getFloor()
	{
		return this._floor;
	} // -- java.lang.String getFloor()

	/**
	 * Returns the value of field 'mnemonic'. The field 'mnemonic' has the
	 * following description: building mnemonic
	 * 
	 * @return String
	 * @return the value of field 'mnemonic'.
	 */
	public java.lang.String getMnemonic()
	{
		return this._mnemonic;
	} // -- java.lang.String getMnemonic()

	/**
	 * Returns the value of field 'position'. The field 'position' has the
	 * following description: FS position
	 * 
	 * @return String
	 * @return the value of field 'position'.
	 */
	public java.lang.String getPosition()
	{
		return this._position;
	} // -- java.lang.String getPosition()

	/**
	 * Returns the value of field 'room'. The field 'room' has the following
	 * description: room number
	 * 
	 * @return String
	 * @return the value of field 'room'.
	 */
	public java.lang.String getRoom()
	{
		return this._room;
	} // -- java.lang.String getRoom()

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
	 * Sets the value of field 'building'. The field 'building' has the
	 * following description: building number
	 * 
	 * @param building
	 *            the value of field 'building'.
	 */
	public void setBuilding(java.lang.String building)
	{
		this._building = building;
	} // -- void setBuilding(java.lang.String)

	/**
	 * Sets the value of field 'floor'. The field 'floor' has the following
	 * description: floor number
	 * 
	 * @param floor
	 *            the value of field 'floor'.
	 */
	public void setFloor(java.lang.String floor)
	{
		this._floor = floor;
	} // -- void setFloor(java.lang.String)

	/**
	 * Sets the value of field 'mnemonic'. The field 'mnemonic' has the
	 * following description: building mnemonic
	 * 
	 * @param mnemonic
	 *            the value of field 'mnemonic'.
	 */
	public void setMnemonic(java.lang.String mnemonic)
	{
		this._mnemonic = mnemonic;
	} // -- void setMnemonic(java.lang.String)

	/**
	 * Sets the value of field 'position'. The field 'position' has the
	 * following description: FS position
	 * 
	 * @param position
	 *            the value of field 'position'.
	 */
	public void setPosition(java.lang.String position)
	{
		this._position = position;
	} // -- void setPosition(java.lang.String)

	/**
	 * Sets the value of field 'room'. The field 'room' has the following
	 * description: room number
	 * 
	 * @param room
	 *            the value of field 'room'.
	 */
	public void setRoom(java.lang.String room)
	{
		this._room = room;
	} // -- void setRoom(java.lang.String)

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
		return (com.cosylab.acs.laser.dao.xml.LocationType) Unmarshaller
				.unmarshal(com.cosylab.acs.laser.dao.xml.LocationType.class,
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
