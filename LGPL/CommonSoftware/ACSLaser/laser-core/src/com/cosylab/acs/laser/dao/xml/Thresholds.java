/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: Thresholds.java,v 1.2 2005/06/14 15:21:12 mslenc Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import java.util.ArrayList;

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * Class Thresholds.
 * 
 * @version $Revision: 1.2 $ $Date: 2005/06/14 15:21:12 $
 */
public class Thresholds implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * Field _thresholdList
	 */
	private java.util.ArrayList _thresholdList;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public Thresholds()
	{
		super();
		_thresholdList = new ArrayList();
	} // -- com.cosylab.acs.laser.dao.xml.Thresholds()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Method addThreshold
	 * 
	 * 
	 * 
	 * @param vThreshold
	 */
	public void addThreshold(com.cosylab.acs.laser.dao.xml.Threshold vThreshold)
			throws java.lang.IndexOutOfBoundsException
	{
		_thresholdList.add(vThreshold);
	} // -- void addThreshold(com.cosylab.acs.laser.dao.xml.Threshold)

	/**
	 * Method addThreshold
	 * 
	 * 
	 * 
	 * @param index
	 * @param vThreshold
	 */
	public void addThreshold(int index,
			com.cosylab.acs.laser.dao.xml.Threshold vThreshold)
			throws java.lang.IndexOutOfBoundsException
	{
		_thresholdList.add(index, vThreshold);
	} // -- void addThreshold(int, com.cosylab.acs.laser.dao.xml.Threshold)

	/**
	 * Method clearThreshold
	 * 
	 */
	public void clearThreshold()
	{
		_thresholdList.clear();
	} // -- void clearThreshold()

	/**
	 * Method enumerateThreshold
	 * 
	 * 
	 * 
	 * @return Enumeration
	 */
	public java.util.Enumeration enumerateThreshold()
	{
		return new org.exolab.castor.util.IteratorEnumeration(_thresholdList
				.iterator());
	} // -- java.util.Enumeration enumerateThreshold()

	/**
	 * Method getThreshold
	 * 
	 * 
	 * 
	 * @param index
	 * @return Threshold
	 */
	public com.cosylab.acs.laser.dao.xml.Threshold getThreshold(int index)
			throws java.lang.IndexOutOfBoundsException
	{
		// -- check bounds for index
		if ((index < 0) || (index > _thresholdList.size())) {
			throw new IndexOutOfBoundsException();
		}

		return (com.cosylab.acs.laser.dao.xml.Threshold) _thresholdList
				.get(index);
	} // -- com.cosylab.acs.laser.dao.xml.Threshold getThreshold(int)

	/**
	 * Method getThreshold
	 * 
	 * 
	 * 
	 * @return Threshold
	 */
	public com.cosylab.acs.laser.dao.xml.Threshold[] getThreshold()
	{
		int size = _thresholdList.size();
		com.cosylab.acs.laser.dao.xml.Threshold[] mArray = new com.cosylab.acs.laser.dao.xml.Threshold[size];
		for (int index = 0; index < size; index++) {
			mArray[index] = (com.cosylab.acs.laser.dao.xml.Threshold) _thresholdList
					.get(index);
		}
		return mArray;
	} // -- com.cosylab.acs.laser.dao.xml.Threshold[] getThreshold()

	/**
	 * Method getThresholdCount
	 * 
	 * 
	 * 
	 * @return int
	 */
	public int getThresholdCount()
	{
		return _thresholdList.size();
	} // -- int getThresholdCount()

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
	 * Method removeThreshold
	 * 
	 * 
	 * 
	 * @param vThreshold
	 * @return boolean
	 */
	public boolean removeThreshold(
			com.cosylab.acs.laser.dao.xml.Threshold vThreshold)
	{
		boolean removed = _thresholdList.remove(vThreshold);
		return removed;
	} // -- boolean removeThreshold(com.cosylab.acs.laser.dao.xml.Threshold)

	/**
	 * Method setThreshold
	 * 
	 * 
	 * 
	 * @param index
	 * @param vThreshold
	 */
	public void setThreshold(int index,
			com.cosylab.acs.laser.dao.xml.Threshold vThreshold)
			throws java.lang.IndexOutOfBoundsException
	{
		// -- check bounds for index
		if ((index < 0) || (index > _thresholdList.size())) {
			throw new IndexOutOfBoundsException();
		}
		_thresholdList.set(index, vThreshold);
	} // -- void setThreshold(int, com.cosylab.acs.laser.dao.xml.Threshold)

	/**
	 * Method setThreshold
	 * 
	 * 
	 * 
	 * @param thresholdArray
	 */
	public void setThreshold(
			com.cosylab.acs.laser.dao.xml.Threshold[] thresholdArray)
	{
		// -- copy array
		_thresholdList.clear();
		for (int i = 0; i < thresholdArray.length; i++) {
			_thresholdList.add(thresholdArray[i]);
		}
	} // -- void setThreshold(com.cosylab.acs.laser.dao.xml.Threshold)

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
		return (com.cosylab.acs.laser.dao.xml.Thresholds) Unmarshaller
				.unmarshal(com.cosylab.acs.laser.dao.xml.Thresholds.class,
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
