/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: MaskDefinitionListType.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import java.util.ArrayList;

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * Class MaskDefinitionListType.
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 */
public class MaskDefinitionListType implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * LASER alarm maintenance mask definition
	 */
	private java.util.ArrayList _maintenanceMaskList;

	/**
	 * LASER alarm machine mode mask definition
	 */
	private java.util.ArrayList _modeMaskList;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public MaskDefinitionListType()
	{
		super();
		_maintenanceMaskList = new ArrayList();
		_modeMaskList = new ArrayList();
	} // -- com.cosylab.acs.laser.dao.xml.MaskDefinitionListType()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Method addMaintenanceMask
	 * 
	 * 
	 * 
	 * @param vMaintenanceMask
	 */
	public void addMaintenanceMask(
			com.cosylab.acs.laser.dao.xml.MaintenanceMask vMaintenanceMask)
			throws java.lang.IndexOutOfBoundsException
	{
		_maintenanceMaskList.add(vMaintenanceMask);
	} // -- void
		// addMaintenanceMask(com.cosylab.acs.laser.dao.xml.MaintenanceMask)

	/**
	 * Method addMaintenanceMask
	 * 
	 * 
	 * 
	 * @param index
	 * @param vMaintenanceMask
	 */
	public void addMaintenanceMask(int index,
			com.cosylab.acs.laser.dao.xml.MaintenanceMask vMaintenanceMask)
			throws java.lang.IndexOutOfBoundsException
	{
		_maintenanceMaskList.add(index, vMaintenanceMask);
	} // -- void addMaintenanceMask(int,
		// com.cosylab.acs.laser.dao.xml.MaintenanceMask)

	/**
	 * Method addModeMask
	 * 
	 * 
	 * 
	 * @param vModeMask
	 */
	public void addModeMask(com.cosylab.acs.laser.dao.xml.ModeMask vModeMask)
			throws java.lang.IndexOutOfBoundsException
	{
		_modeMaskList.add(vModeMask);
	} // -- void addModeMask(com.cosylab.acs.laser.dao.xml.ModeMask)

	/**
	 * Method addModeMask
	 * 
	 * 
	 * 
	 * @param index
	 * @param vModeMask
	 */
	public void addModeMask(int index,
			com.cosylab.acs.laser.dao.xml.ModeMask vModeMask)
			throws java.lang.IndexOutOfBoundsException
	{
		_modeMaskList.add(index, vModeMask);
	} // -- void addModeMask(int, com.cosylab.acs.laser.dao.xml.ModeMask)

	/**
	 * Method clearMaintenanceMask
	 * 
	 */
	public void clearMaintenanceMask()
	{
		_maintenanceMaskList.clear();
	} // -- void clearMaintenanceMask()

	/**
	 * Method clearModeMask
	 * 
	 */
	public void clearModeMask()
	{
		_modeMaskList.clear();
	} // -- void clearModeMask()

	/**
	 * Method enumerateMaintenanceMask
	 * 
	 * 
	 * 
	 * @return Enumeration
	 */
	public java.util.Enumeration enumerateMaintenanceMask()
	{
		return new org.exolab.castor.util.IteratorEnumeration(
				_maintenanceMaskList.iterator());
	} // -- java.util.Enumeration enumerateMaintenanceMask()

	/**
	 * Method enumerateModeMask
	 * 
	 * 
	 * 
	 * @return Enumeration
	 */
	public java.util.Enumeration enumerateModeMask()
	{
		return new org.exolab.castor.util.IteratorEnumeration(_modeMaskList
				.iterator());
	} // -- java.util.Enumeration enumerateModeMask()

	/**
	 * Method getMaintenanceMask
	 * 
	 * 
	 * 
	 * @param index
	 * @return MaintenanceMask
	 */
	public com.cosylab.acs.laser.dao.xml.MaintenanceMask getMaintenanceMask(
			int index) throws java.lang.IndexOutOfBoundsException
	{
		// -- check bounds for index
		if ((index < 0) || (index > _maintenanceMaskList.size())) {
			throw new IndexOutOfBoundsException();
		}

		return (com.cosylab.acs.laser.dao.xml.MaintenanceMask) _maintenanceMaskList
				.get(index);
	} // -- com.cosylab.acs.laser.dao.xml.MaintenanceMask
		// getMaintenanceMask(int)

	/**
	 * Method getMaintenanceMask
	 * 
	 * 
	 * 
	 * @return MaintenanceMask
	 */
	public com.cosylab.acs.laser.dao.xml.MaintenanceMask[] getMaintenanceMask()
	{
		int size = _maintenanceMaskList.size();
		com.cosylab.acs.laser.dao.xml.MaintenanceMask[] mArray = new com.cosylab.acs.laser.dao.xml.MaintenanceMask[size];
		for (int index = 0; index < size; index++) {
			mArray[index] = (com.cosylab.acs.laser.dao.xml.MaintenanceMask) _maintenanceMaskList
					.get(index);
		}
		return mArray;
	} // -- com.cosylab.acs.laser.dao.xml.MaintenanceMask[]
		// getMaintenanceMask()

	/**
	 * Method getMaintenanceMaskCount
	 * 
	 * 
	 * 
	 * @return int
	 */
	public int getMaintenanceMaskCount()
	{
		return _maintenanceMaskList.size();
	} // -- int getMaintenanceMaskCount()

	/**
	 * Method getModeMask
	 * 
	 * 
	 * 
	 * @param index
	 * @return ModeMask
	 */
	public com.cosylab.acs.laser.dao.xml.ModeMask getModeMask(int index)
			throws java.lang.IndexOutOfBoundsException
	{
		// -- check bounds for index
		if ((index < 0) || (index > _modeMaskList.size())) {
			throw new IndexOutOfBoundsException();
		}

		return (com.cosylab.acs.laser.dao.xml.ModeMask) _modeMaskList
				.get(index);
	} // -- com.cosylab.acs.laser.dao.xml.ModeMask getModeMask(int)

	/**
	 * Method getModeMask
	 * 
	 * 
	 * 
	 * @return ModeMask
	 */
	public com.cosylab.acs.laser.dao.xml.ModeMask[] getModeMask()
	{
		int size = _modeMaskList.size();
		com.cosylab.acs.laser.dao.xml.ModeMask[] mArray = new com.cosylab.acs.laser.dao.xml.ModeMask[size];
		for (int index = 0; index < size; index++) {
			mArray[index] = (com.cosylab.acs.laser.dao.xml.ModeMask) _modeMaskList
					.get(index);
		}
		return mArray;
	} // -- com.cosylab.acs.laser.dao.xml.ModeMask[] getModeMask()

	/**
	 * Method getModeMaskCount
	 * 
	 * 
	 * 
	 * @return int
	 */
	public int getModeMaskCount()
	{
		return _modeMaskList.size();
	} // -- int getModeMaskCount()

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
	 * Method removeMaintenanceMask
	 * 
	 * 
	 * 
	 * @param vMaintenanceMask
	 * @return boolean
	 */
	public boolean removeMaintenanceMask(
			com.cosylab.acs.laser.dao.xml.MaintenanceMask vMaintenanceMask)
	{
		boolean removed = _maintenanceMaskList.remove(vMaintenanceMask);
		return removed;
	} // -- boolean
		// removeMaintenanceMask(com.cosylab.acs.laser.dao.xml.MaintenanceMask)

	/**
	 * Method removeModeMask
	 * 
	 * 
	 * 
	 * @param vModeMask
	 * @return boolean
	 */
	public boolean removeModeMask(
			com.cosylab.acs.laser.dao.xml.ModeMask vModeMask)
	{
		boolean removed = _modeMaskList.remove(vModeMask);
		return removed;
	} // -- boolean removeModeMask(com.cosylab.acs.laser.dao.xml.ModeMask)

	/**
	 * Method setMaintenanceMask
	 * 
	 * 
	 * 
	 * @param index
	 * @param vMaintenanceMask
	 */
	public void setMaintenanceMask(int index,
			com.cosylab.acs.laser.dao.xml.MaintenanceMask vMaintenanceMask)
			throws java.lang.IndexOutOfBoundsException
	{
		// -- check bounds for index
		if ((index < 0) || (index > _maintenanceMaskList.size())) {
			throw new IndexOutOfBoundsException();
		}
		_maintenanceMaskList.set(index, vMaintenanceMask);
	} // -- void setMaintenanceMask(int,
		// com.cosylab.acs.laser.dao.xml.MaintenanceMask)

	/**
	 * Method setMaintenanceMask
	 * 
	 * 
	 * 
	 * @param maintenanceMaskArray
	 */
	public void setMaintenanceMask(
			com.cosylab.acs.laser.dao.xml.MaintenanceMask[] maintenanceMaskArray)
	{
		// -- copy array
		_maintenanceMaskList.clear();
		for (int i = 0; i < maintenanceMaskArray.length; i++) {
			_maintenanceMaskList.add(maintenanceMaskArray[i]);
		}
	} // -- void
		// setMaintenanceMask(com.cosylab.acs.laser.dao.xml.MaintenanceMask)

	/**
	 * Method setModeMask
	 * 
	 * 
	 * 
	 * @param index
	 * @param vModeMask
	 */
	public void setModeMask(int index,
			com.cosylab.acs.laser.dao.xml.ModeMask vModeMask)
			throws java.lang.IndexOutOfBoundsException
	{
		// -- check bounds for index
		if ((index < 0) || (index > _modeMaskList.size())) {
			throw new IndexOutOfBoundsException();
		}
		_modeMaskList.set(index, vModeMask);
	} // -- void setModeMask(int, com.cosylab.acs.laser.dao.xml.ModeMask)

	/**
	 * Method setModeMask
	 * 
	 * 
	 * 
	 * @param modeMaskArray
	 */
	public void setModeMask(
			com.cosylab.acs.laser.dao.xml.ModeMask[] modeMaskArray)
	{
		// -- copy array
		_modeMaskList.clear();
		for (int i = 0; i < modeMaskArray.length; i++) {
			_modeMaskList.add(modeMaskArray[i]);
		}
	} // -- void setModeMask(com.cosylab.acs.laser.dao.xml.ModeMask)

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
		return (com.cosylab.acs.laser.dao.xml.MaskDefinitionListType) Unmarshaller
				.unmarshal(
						com.cosylab.acs.laser.dao.xml.MaskDefinitionListType.class,
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
