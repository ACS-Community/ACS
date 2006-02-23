/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: ReductionLinkDefinitionListType.java,v 1.2 2005/06/14 15:21:12 mslenc Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import java.util.ArrayList;

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * Class ReductionLinkDefinitionListType.
 * 
 * @version $Revision: 1.2 $ $Date: 2005/06/14 15:21:12 $
 */
public class ReductionLinkDefinitionListType implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * LASER reduction relationship link
	 */
	private java.util.ArrayList _reductionLinkList;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public ReductionLinkDefinitionListType()
	{
		super();
		_reductionLinkList = new ArrayList();
	} // -- com.cosylab.acs.laser.dao.xml.ReductionLinkDefinitionListType()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Method addReductionLink
	 * 
	 * 
	 * 
	 * @param vReductionLink
	 */
	public void addReductionLink(
			com.cosylab.acs.laser.dao.xml.ReductionLink vReductionLink)
			throws java.lang.IndexOutOfBoundsException
	{
		_reductionLinkList.add(vReductionLink);
	} // -- void addReductionLink(com.cosylab.acs.laser.dao.xml.ReductionLink)

	/**
	 * Method addReductionLink
	 * 
	 * 
	 * 
	 * @param index
	 * @param vReductionLink
	 */
	public void addReductionLink(int index,
			com.cosylab.acs.laser.dao.xml.ReductionLink vReductionLink)
			throws java.lang.IndexOutOfBoundsException
	{
		_reductionLinkList.add(index, vReductionLink);
	} // -- void addReductionLink(int,
		// com.cosylab.acs.laser.dao.xml.ReductionLink)

	/**
	 * Method clearReductionLink
	 * 
	 */
	public void clearReductionLink()
	{
		_reductionLinkList.clear();
	} // -- void clearReductionLink()

	/**
	 * Method enumerateReductionLink
	 * 
	 * 
	 * 
	 * @return Enumeration
	 */
	public java.util.Enumeration enumerateReductionLink()
	{
		return new org.exolab.castor.util.IteratorEnumeration(
				_reductionLinkList.iterator());
	} // -- java.util.Enumeration enumerateReductionLink()

	/**
	 * Method getReductionLink
	 * 
	 * 
	 * 
	 * @param index
	 * @return ReductionLink
	 */
	public com.cosylab.acs.laser.dao.xml.ReductionLink getReductionLink(
			int index) throws java.lang.IndexOutOfBoundsException
	{
		// -- check bounds for index
		if ((index < 0) || (index > _reductionLinkList.size())) {
			throw new IndexOutOfBoundsException();
		}

		return (com.cosylab.acs.laser.dao.xml.ReductionLink) _reductionLinkList
				.get(index);
	} // -- com.cosylab.acs.laser.dao.xml.ReductionLink getReductionLink(int)

	/**
	 * Method getReductionLink
	 * 
	 * 
	 * 
	 * @return ReductionLink
	 */
	public com.cosylab.acs.laser.dao.xml.ReductionLink[] getReductionLink()
	{
		int size = _reductionLinkList.size();
		com.cosylab.acs.laser.dao.xml.ReductionLink[] mArray = new com.cosylab.acs.laser.dao.xml.ReductionLink[size];
		for (int index = 0; index < size; index++) {
			mArray[index] = (com.cosylab.acs.laser.dao.xml.ReductionLink) _reductionLinkList
					.get(index);
		}
		return mArray;
	} // -- com.cosylab.acs.laser.dao.xml.ReductionLink[] getReductionLink()

	/**
	 * Method getReductionLinkCount
	 * 
	 * 
	 * 
	 * @return int
	 */
	public int getReductionLinkCount()
	{
		return _reductionLinkList.size();
	} // -- int getReductionLinkCount()

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
	 * Method removeReductionLink
	 * 
	 * 
	 * 
	 * @param vReductionLink
	 * @return boolean
	 */
	public boolean removeReductionLink(
			com.cosylab.acs.laser.dao.xml.ReductionLink vReductionLink)
	{
		boolean removed = _reductionLinkList.remove(vReductionLink);
		return removed;
	} // -- boolean
		// removeReductionLink(com.cosylab.acs.laser.dao.xml.ReductionLink)

	/**
	 * Method setReductionLink
	 * 
	 * 
	 * 
	 * @param index
	 * @param vReductionLink
	 */
	public void setReductionLink(int index,
			com.cosylab.acs.laser.dao.xml.ReductionLink vReductionLink)
			throws java.lang.IndexOutOfBoundsException
	{
		// -- check bounds for index
		if ((index < 0) || (index > _reductionLinkList.size())) {
			throw new IndexOutOfBoundsException();
		}
		_reductionLinkList.set(index, vReductionLink);
	} // -- void setReductionLink(int,
		// com.cosylab.acs.laser.dao.xml.ReductionLink)

	/**
	 * Method setReductionLink
	 * 
	 * 
	 * 
	 * @param reductionLinkArray
	 */
	public void setReductionLink(
			com.cosylab.acs.laser.dao.xml.ReductionLink[] reductionLinkArray)
	{
		// -- copy array
		_reductionLinkList.clear();
		for (int i = 0; i < reductionLinkArray.length; i++) {
			_reductionLinkList.add(reductionLinkArray[i]);
		}
	} // -- void setReductionLink(com.cosylab.acs.laser.dao.xml.ReductionLink)

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
		return (com.cosylab.acs.laser.dao.xml.ReductionLinkDefinitionListType) Unmarshaller
				.unmarshal(
						com.cosylab.acs.laser.dao.xml.ReductionLinkDefinitionListType.class,
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
