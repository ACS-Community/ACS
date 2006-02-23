/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: SourceDefinitionListType.java,v 1.2 2005/06/14 15:21:12 mslenc Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import java.util.ArrayList;

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * Class SourceDefinitionListType.
 * 
 * @version $Revision: 1.2 $ $Date: 2005/06/14 15:21:12 $
 */
public class SourceDefinitionListType implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * LASER alarm source definition
	 */
	private java.util.ArrayList _sourceDefinitionList;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public SourceDefinitionListType()
	{
		super();
		_sourceDefinitionList = new ArrayList();
	} // -- com.cosylab.acs.laser.dao.xml.SourceDefinitionListType()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Method addSourceDefinition
	 * 
	 * 
	 * 
	 * @param vSourceDefinition
	 */
	public void addSourceDefinition(
			com.cosylab.acs.laser.dao.xml.SourceDefinition vSourceDefinition)
			throws java.lang.IndexOutOfBoundsException
	{
		_sourceDefinitionList.add(vSourceDefinition);
	} // -- void
		// addSourceDefinition(com.cosylab.acs.laser.dao.xml.SourceDefinition)

	/**
	 * Method addSourceDefinition
	 * 
	 * 
	 * 
	 * @param index
	 * @param vSourceDefinition
	 */
	public void addSourceDefinition(int index,
			com.cosylab.acs.laser.dao.xml.SourceDefinition vSourceDefinition)
			throws java.lang.IndexOutOfBoundsException
	{
		_sourceDefinitionList.add(index, vSourceDefinition);
	} // -- void addSourceDefinition(int,
		// com.cosylab.acs.laser.dao.xml.SourceDefinition)

	/**
	 * Method clearSourceDefinition
	 * 
	 */
	public void clearSourceDefinition()
	{
		_sourceDefinitionList.clear();
	} // -- void clearSourceDefinition()

	/**
	 * Method enumerateSourceDefinition
	 * 
	 * 
	 * 
	 * @return Enumeration
	 */
	public java.util.Enumeration enumerateSourceDefinition()
	{
		return new org.exolab.castor.util.IteratorEnumeration(
				_sourceDefinitionList.iterator());
	} // -- java.util.Enumeration enumerateSourceDefinition()

	/**
	 * Method getSourceDefinition
	 * 
	 * 
	 * 
	 * @param index
	 * @return SourceDefinition
	 */
	public com.cosylab.acs.laser.dao.xml.SourceDefinition getSourceDefinition(
			int index) throws java.lang.IndexOutOfBoundsException
	{
		// -- check bounds for index
		if ((index < 0) || (index > _sourceDefinitionList.size())) {
			throw new IndexOutOfBoundsException();
		}

		return (com.cosylab.acs.laser.dao.xml.SourceDefinition) _sourceDefinitionList
				.get(index);
	} // -- com.cosylab.acs.laser.dao.xml.SourceDefinition
		// getSourceDefinition(int)

	/**
	 * Method getSourceDefinition
	 * 
	 * 
	 * 
	 * @return SourceDefinition
	 */
	public com.cosylab.acs.laser.dao.xml.SourceDefinition[] getSourceDefinition()
	{
		int size = _sourceDefinitionList.size();
		com.cosylab.acs.laser.dao.xml.SourceDefinition[] mArray = new com.cosylab.acs.laser.dao.xml.SourceDefinition[size];
		for (int index = 0; index < size; index++) {
			mArray[index] = (com.cosylab.acs.laser.dao.xml.SourceDefinition) _sourceDefinitionList
					.get(index);
		}
		return mArray;
	} // -- com.cosylab.acs.laser.dao.xml.SourceDefinition[]
		// getSourceDefinition()

	/**
	 * Method getSourceDefinitionCount
	 * 
	 * 
	 * 
	 * @return int
	 */
	public int getSourceDefinitionCount()
	{
		return _sourceDefinitionList.size();
	} // -- int getSourceDefinitionCount()

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
	 * Method removeSourceDefinition
	 * 
	 * 
	 * 
	 * @param vSourceDefinition
	 * @return boolean
	 */
	public boolean removeSourceDefinition(
			com.cosylab.acs.laser.dao.xml.SourceDefinition vSourceDefinition)
	{
		boolean removed = _sourceDefinitionList.remove(vSourceDefinition);
		return removed;
	} // -- boolean
		// removeSourceDefinition(com.cosylab.acs.laser.dao.xml.SourceDefinition)

	/**
	 * Method setSourceDefinition
	 * 
	 * 
	 * 
	 * @param index
	 * @param vSourceDefinition
	 */
	public void setSourceDefinition(int index,
			com.cosylab.acs.laser.dao.xml.SourceDefinition vSourceDefinition)
			throws java.lang.IndexOutOfBoundsException
	{
		// -- check bounds for index
		if ((index < 0) || (index > _sourceDefinitionList.size())) {
			throw new IndexOutOfBoundsException();
		}
		_sourceDefinitionList.set(index, vSourceDefinition);
	} // -- void setSourceDefinition(int,
		// com.cosylab.acs.laser.dao.xml.SourceDefinition)

	/**
	 * Method setSourceDefinition
	 * 
	 * 
	 * 
	 * @param sourceDefinitionArray
	 */
	public void setSourceDefinition(
			com.cosylab.acs.laser.dao.xml.SourceDefinition[] sourceDefinitionArray)
	{
		// -- copy array
		_sourceDefinitionList.clear();
		for (int i = 0; i < sourceDefinitionArray.length; i++) {
			_sourceDefinitionList.add(sourceDefinitionArray[i]);
		}
	} // -- void
		// setSourceDefinition(com.cosylab.acs.laser.dao.xml.SourceDefinition)

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
		return (com.cosylab.acs.laser.dao.xml.SourceDefinitionListType) Unmarshaller
				.unmarshal(
						com.cosylab.acs.laser.dao.xml.SourceDefinitionListType.class,
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
