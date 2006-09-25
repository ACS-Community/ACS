/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: AlarmCategoryLinkDefinitionListType.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import java.util.ArrayList;

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * Class AlarmCategoryLinkDefinitionListType.
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 */
public class AlarmCategoryLinkDefinitionListType implements
		java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * LASER alarm-category link definition
	 */
	private java.util.ArrayList _alarmCategoryLinkList;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public AlarmCategoryLinkDefinitionListType()
	{
		super();
		_alarmCategoryLinkList = new ArrayList();
	} // --
		// com.cosylab.acs.laser.dao.xml.AlarmCategoryLinkDefinitionListType()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Method addAlarmCategoryLink
	 * 
	 * 
	 * 
	 * @param vAlarmCategoryLink
	 */
	public void addAlarmCategoryLink(
			com.cosylab.acs.laser.dao.xml.AlarmCategoryLink vAlarmCategoryLink)
			throws java.lang.IndexOutOfBoundsException
	{
		_alarmCategoryLinkList.add(vAlarmCategoryLink);
	} // -- void
		// addAlarmCategoryLink(com.cosylab.acs.laser.dao.xml.AlarmCategoryLink)

	/**
	 * Method addAlarmCategoryLink
	 * 
	 * 
	 * 
	 * @param index
	 * @param vAlarmCategoryLink
	 */
	public void addAlarmCategoryLink(int index,
			com.cosylab.acs.laser.dao.xml.AlarmCategoryLink vAlarmCategoryLink)
			throws java.lang.IndexOutOfBoundsException
	{
		_alarmCategoryLinkList.add(index, vAlarmCategoryLink);
	} // -- void addAlarmCategoryLink(int,
		// com.cosylab.acs.laser.dao.xml.AlarmCategoryLink)

	/**
	 * Method clearAlarmCategoryLink
	 * 
	 */
	public void clearAlarmCategoryLink()
	{
		_alarmCategoryLinkList.clear();
	} // -- void clearAlarmCategoryLink()

	/**
	 * Method enumerateAlarmCategoryLink
	 * 
	 * 
	 * 
	 * @return Enumeration
	 */
	public java.util.Enumeration enumerateAlarmCategoryLink()
	{
		return new org.exolab.castor.util.IteratorEnumeration(
				_alarmCategoryLinkList.iterator());
	} // -- java.util.Enumeration enumerateAlarmCategoryLink()

	/**
	 * Method getAlarmCategoryLink
	 * 
	 * 
	 * 
	 * @param index
	 * @return AlarmCategoryLink
	 */
	public com.cosylab.acs.laser.dao.xml.AlarmCategoryLink getAlarmCategoryLink(
			int index) throws java.lang.IndexOutOfBoundsException
	{
		// -- check bounds for index
		if ((index < 0) || (index > _alarmCategoryLinkList.size())) {
			throw new IndexOutOfBoundsException();
		}

		return (com.cosylab.acs.laser.dao.xml.AlarmCategoryLink) _alarmCategoryLinkList
				.get(index);
	} // -- com.cosylab.acs.laser.dao.xml.AlarmCategoryLink
		// getAlarmCategoryLink(int)

	/**
	 * Method getAlarmCategoryLink
	 * 
	 * 
	 * 
	 * @return AlarmCategoryLink
	 */
	public com.cosylab.acs.laser.dao.xml.AlarmCategoryLink[] getAlarmCategoryLink()
	{
		int size = _alarmCategoryLinkList.size();
		com.cosylab.acs.laser.dao.xml.AlarmCategoryLink[] mArray = new com.cosylab.acs.laser.dao.xml.AlarmCategoryLink[size];
		for (int index = 0; index < size; index++) {
			mArray[index] = (com.cosylab.acs.laser.dao.xml.AlarmCategoryLink) _alarmCategoryLinkList
					.get(index);
		}
		return mArray;
	} // -- com.cosylab.acs.laser.dao.xml.AlarmCategoryLink[]
		// getAlarmCategoryLink()

	/**
	 * Method getAlarmCategoryLinkCount
	 * 
	 * 
	 * 
	 * @return int
	 */
	public int getAlarmCategoryLinkCount()
	{
		return _alarmCategoryLinkList.size();
	} // -- int getAlarmCategoryLinkCount()

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
	 * Method removeAlarmCategoryLink
	 * 
	 * 
	 * 
	 * @param vAlarmCategoryLink
	 * @return boolean
	 */
	public boolean removeAlarmCategoryLink(
			com.cosylab.acs.laser.dao.xml.AlarmCategoryLink vAlarmCategoryLink)
	{
		boolean removed = _alarmCategoryLinkList.remove(vAlarmCategoryLink);
		return removed;
	} // -- boolean
		// removeAlarmCategoryLink(com.cosylab.acs.laser.dao.xml.AlarmCategoryLink)

	/**
	 * Method setAlarmCategoryLink
	 * 
	 * 
	 * 
	 * @param index
	 * @param vAlarmCategoryLink
	 */
	public void setAlarmCategoryLink(int index,
			com.cosylab.acs.laser.dao.xml.AlarmCategoryLink vAlarmCategoryLink)
			throws java.lang.IndexOutOfBoundsException
	{
		// -- check bounds for index
		if ((index < 0) || (index > _alarmCategoryLinkList.size())) {
			throw new IndexOutOfBoundsException();
		}
		_alarmCategoryLinkList.set(index, vAlarmCategoryLink);
	} // -- void setAlarmCategoryLink(int,
		// com.cosylab.acs.laser.dao.xml.AlarmCategoryLink)

	/**
	 * Method setAlarmCategoryLink
	 * 
	 * 
	 * 
	 * @param alarmCategoryLinkArray
	 */
	public void setAlarmCategoryLink(
			com.cosylab.acs.laser.dao.xml.AlarmCategoryLink[] alarmCategoryLinkArray)
	{
		// -- copy array
		_alarmCategoryLinkList.clear();
		for (int i = 0; i < alarmCategoryLinkArray.length; i++) {
			_alarmCategoryLinkList.add(alarmCategoryLinkArray[i]);
		}
	} // -- void
		// setAlarmCategoryLink(com.cosylab.acs.laser.dao.xml.AlarmCategoryLink)

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
		return (com.cosylab.acs.laser.dao.xml.AlarmCategoryLinkDefinitionListType) Unmarshaller
				.unmarshal(
						com.cosylab.acs.laser.dao.xml.AlarmCategoryLinkDefinitionListType.class,
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
