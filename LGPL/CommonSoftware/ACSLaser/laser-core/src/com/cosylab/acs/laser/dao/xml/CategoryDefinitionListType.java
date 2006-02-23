/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: CategoryDefinitionListType.java,v 1.2 2005/06/14 15:21:12 mslenc Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import java.util.ArrayList;

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * Class CategoryDefinitionListType.
 * 
 * @version $Revision: 1.2 $ $Date: 2005/06/14 15:21:12 $
 */
public class CategoryDefinitionListType implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * LASER alarm category definition
	 */
	private java.util.ArrayList _categoryDefinitionList;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public CategoryDefinitionListType()
	{
		super();
		_categoryDefinitionList = new ArrayList();
	} // -- com.cosylab.acs.laser.dao.xml.CategoryDefinitionListType()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Method addCategoryDefinition
	 * 
	 * 
	 * 
	 * @param vCategoryDefinition
	 */
	public void addCategoryDefinition(
			com.cosylab.acs.laser.dao.xml.CategoryDefinition vCategoryDefinition)
			throws java.lang.IndexOutOfBoundsException
	{
		_categoryDefinitionList.add(vCategoryDefinition);
	} // -- void
		// addCategoryDefinition(com.cosylab.acs.laser.dao.xml.CategoryDefinition)

	/**
	 * Method addCategoryDefinition
	 * 
	 * 
	 * 
	 * @param index
	 * @param vCategoryDefinition
	 */
	public void addCategoryDefinition(int index,
			com.cosylab.acs.laser.dao.xml.CategoryDefinition vCategoryDefinition)
			throws java.lang.IndexOutOfBoundsException
	{
		_categoryDefinitionList.add(index, vCategoryDefinition);
	} // -- void addCategoryDefinition(int,
		// com.cosylab.acs.laser.dao.xml.CategoryDefinition)

	/**
	 * Method clearCategoryDefinition
	 * 
	 */
	public void clearCategoryDefinition()
	{
		_categoryDefinitionList.clear();
	} // -- void clearCategoryDefinition()

	/**
	 * Method enumerateCategoryDefinition
	 * 
	 * 
	 * 
	 * @return Enumeration
	 */
	public java.util.Enumeration enumerateCategoryDefinition()
	{
		return new org.exolab.castor.util.IteratorEnumeration(
				_categoryDefinitionList.iterator());
	} // -- java.util.Enumeration enumerateCategoryDefinition()

	/**
	 * Method getCategoryDefinition
	 * 
	 * 
	 * 
	 * @param index
	 * @return CategoryDefinition
	 */
	public com.cosylab.acs.laser.dao.xml.CategoryDefinition getCategoryDefinition(
			int index) throws java.lang.IndexOutOfBoundsException
	{
		// -- check bounds for index
		if ((index < 0) || (index > _categoryDefinitionList.size())) {
			throw new IndexOutOfBoundsException();
		}

		return (com.cosylab.acs.laser.dao.xml.CategoryDefinition) _categoryDefinitionList
				.get(index);
	} // -- com.cosylab.acs.laser.dao.xml.CategoryDefinition
		// getCategoryDefinition(int)

	/**
	 * Method getCategoryDefinition
	 * 
	 * 
	 * 
	 * @return CategoryDefinition
	 */
	public com.cosylab.acs.laser.dao.xml.CategoryDefinition[] getCategoryDefinition()
	{
		int size = _categoryDefinitionList.size();
		com.cosylab.acs.laser.dao.xml.CategoryDefinition[] mArray = new com.cosylab.acs.laser.dao.xml.CategoryDefinition[size];
		for (int index = 0; index < size; index++) {
			mArray[index] = (com.cosylab.acs.laser.dao.xml.CategoryDefinition) _categoryDefinitionList
					.get(index);
		}
		return mArray;
	} // -- com.cosylab.acs.laser.dao.xml.CategoryDefinition[]
		// getCategoryDefinition()

	/**
	 * Method getCategoryDefinitionCount
	 * 
	 * 
	 * 
	 * @return int
	 */
	public int getCategoryDefinitionCount()
	{
		return _categoryDefinitionList.size();
	} // -- int getCategoryDefinitionCount()

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
	 * Method removeCategoryDefinition
	 * 
	 * 
	 * 
	 * @param vCategoryDefinition
	 * @return boolean
	 */
	public boolean removeCategoryDefinition(
			com.cosylab.acs.laser.dao.xml.CategoryDefinition vCategoryDefinition)
	{
		boolean removed = _categoryDefinitionList.remove(vCategoryDefinition);
		return removed;
	} // -- boolean
		// removeCategoryDefinition(com.cosylab.acs.laser.dao.xml.CategoryDefinition)

	/**
	 * Method setCategoryDefinition
	 * 
	 * 
	 * 
	 * @param index
	 * @param vCategoryDefinition
	 */
	public void setCategoryDefinition(int index,
			com.cosylab.acs.laser.dao.xml.CategoryDefinition vCategoryDefinition)
			throws java.lang.IndexOutOfBoundsException
	{
		// -- check bounds for index
		if ((index < 0) || (index > _categoryDefinitionList.size())) {
			throw new IndexOutOfBoundsException();
		}
		_categoryDefinitionList.set(index, vCategoryDefinition);
	} // -- void setCategoryDefinition(int,
		// com.cosylab.acs.laser.dao.xml.CategoryDefinition)

	/**
	 * Method setCategoryDefinition
	 * 
	 * 
	 * 
	 * @param categoryDefinitionArray
	 */
	public void setCategoryDefinition(
			com.cosylab.acs.laser.dao.xml.CategoryDefinition[] categoryDefinitionArray)
	{
		// -- copy array
		_categoryDefinitionList.clear();
		for (int i = 0; i < categoryDefinitionArray.length; i++) {
			_categoryDefinitionList.add(categoryDefinitionArray[i]);
		}
	} // -- void
		// setCategoryDefinition(com.cosylab.acs.laser.dao.xml.CategoryDefinition)

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
		return (com.cosylab.acs.laser.dao.xml.CategoryDefinitionListType) Unmarshaller
				.unmarshal(
						com.cosylab.acs.laser.dao.xml.CategoryDefinitionListType.class,
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
