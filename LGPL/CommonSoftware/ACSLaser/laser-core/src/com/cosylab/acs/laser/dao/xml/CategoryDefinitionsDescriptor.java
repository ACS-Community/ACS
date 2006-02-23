/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: CategoryDefinitionsDescriptor.java,v 1.2 2005/06/14 15:21:12 mslenc Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

/**
 * Class CategoryDefinitionsDescriptor.
 * 
 * @version $Revision: 1.2 $ $Date: 2005/06/14 15:21:12 $
 */
public class CategoryDefinitionsDescriptor extends org.exolab.castor.xml.util.XMLClassDescriptorImpl
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * Field nsPrefix
	 */
	private java.lang.String nsPrefix;

	/**
	 * Field nsURI
	 */
	private java.lang.String nsURI;

	/**
	 * Field xmlName
	 */
	private java.lang.String xmlName;

	/**
	 * Field identity
	 */
	private org.exolab.castor.xml.XMLFieldDescriptor identity;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public CategoryDefinitionsDescriptor()
	{
		super();
		nsURI = "urn:schemas-cosylab-com:Alarm:1.0";
		xmlName = "category-definitions";
		org.exolab.castor.xml.util.XMLFieldDescriptorImpl desc = null;
		org.exolab.castor.mapping.FieldHandler handler = null;
		org.exolab.castor.xml.FieldValidator fieldValidator = null;
		// -- initialize attribute descriptors

		// -- initialize element descriptors

		// -- _categoriesToCreate
		desc = new org.exolab.castor.xml.util.XMLFieldDescriptorImpl(
				com.cosylab.acs.laser.dao.xml.CategoriesToCreate.class,
				"_categoriesToCreate", "categories-to-create",
				org.exolab.castor.xml.NodeType.Element);
		handler = new org.exolab.castor.xml.XMLFieldHandler() {
			public java.lang.Object getValue(java.lang.Object object)
					throws IllegalStateException
			{
				CategoryDefinitions target = (CategoryDefinitions) object;
				return target.getCategoriesToCreate();
			}

			public void setValue(java.lang.Object object, java.lang.Object value)
					throws IllegalStateException, IllegalArgumentException
			{
				try {
					CategoryDefinitions target = (CategoryDefinitions) object;
					target
							.setCategoriesToCreate((com.cosylab.acs.laser.dao.xml.CategoriesToCreate) value);
				} catch (java.lang.Exception ex) {
					throw new IllegalStateException(ex.toString());
				}
			}

			public java.lang.Object newInstance(java.lang.Object parent)
			{
				return new com.cosylab.acs.laser.dao.xml.CategoriesToCreate();
			}
		};
		desc.setHandler(handler);
		desc.setNameSpaceURI("urn:schemas-cosylab-com:Alarm:1.0");
		desc.setMultivalued(false);
		addFieldDescriptor(desc);

		// -- validation code for: _categoriesToCreate
		fieldValidator = new org.exolab.castor.xml.FieldValidator();
		{ // -- local scope
		}
		desc.setValidator(fieldValidator);
		// -- _categoriesToUpdate
		desc = new org.exolab.castor.xml.util.XMLFieldDescriptorImpl(
				com.cosylab.acs.laser.dao.xml.CategoriesToUpdate.class,
				"_categoriesToUpdate", "categories-to-update",
				org.exolab.castor.xml.NodeType.Element);
		handler = new org.exolab.castor.xml.XMLFieldHandler() {
			public java.lang.Object getValue(java.lang.Object object)
					throws IllegalStateException
			{
				CategoryDefinitions target = (CategoryDefinitions) object;
				return target.getCategoriesToUpdate();
			}

			public void setValue(java.lang.Object object, java.lang.Object value)
					throws IllegalStateException, IllegalArgumentException
			{
				try {
					CategoryDefinitions target = (CategoryDefinitions) object;
					target
							.setCategoriesToUpdate((com.cosylab.acs.laser.dao.xml.CategoriesToUpdate) value);
				} catch (java.lang.Exception ex) {
					throw new IllegalStateException(ex.toString());
				}
			}

			public java.lang.Object newInstance(java.lang.Object parent)
			{
				return new com.cosylab.acs.laser.dao.xml.CategoriesToUpdate();
			}
		};
		desc.setHandler(handler);
		desc.setNameSpaceURI("urn:schemas-cosylab-com:Alarm:1.0");
		desc.setMultivalued(false);
		addFieldDescriptor(desc);

		// -- validation code for: _categoriesToUpdate
		fieldValidator = new org.exolab.castor.xml.FieldValidator();
		{ // -- local scope
		}
		desc.setValidator(fieldValidator);
		// -- _categoriesToRemove
		desc = new org.exolab.castor.xml.util.XMLFieldDescriptorImpl(
				com.cosylab.acs.laser.dao.xml.CategoriesToRemove.class,
				"_categoriesToRemove", "categories-to-remove",
				org.exolab.castor.xml.NodeType.Element);
		handler = new org.exolab.castor.xml.XMLFieldHandler() {
			public java.lang.Object getValue(java.lang.Object object)
					throws IllegalStateException
			{
				CategoryDefinitions target = (CategoryDefinitions) object;
				return target.getCategoriesToRemove();
			}

			public void setValue(java.lang.Object object, java.lang.Object value)
					throws IllegalStateException, IllegalArgumentException
			{
				try {
					CategoryDefinitions target = (CategoryDefinitions) object;
					target
							.setCategoriesToRemove((com.cosylab.acs.laser.dao.xml.CategoriesToRemove) value);
				} catch (java.lang.Exception ex) {
					throw new IllegalStateException(ex.toString());
				}
			}

			public java.lang.Object newInstance(java.lang.Object parent)
			{
				return new com.cosylab.acs.laser.dao.xml.CategoriesToRemove();
			}
		};
		desc.setHandler(handler);
		desc.setNameSpaceURI("urn:schemas-cosylab-com:Alarm:1.0");
		desc.setMultivalued(false);
		addFieldDescriptor(desc);

		// -- validation code for: _categoriesToRemove
		fieldValidator = new org.exolab.castor.xml.FieldValidator();
		{ // -- local scope
		}
		desc.setValidator(fieldValidator);
	} // -- com.cosylab.acs.laser.dao.xml.CategoryDefinitionsDescriptor()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Method getAccessMode
	 * 
	 * 
	 * 
	 * @return AccessMode
	 */
	public org.exolab.castor.mapping.AccessMode getAccessMode()
	{
		return null;
	} // -- org.exolab.castor.mapping.AccessMode getAccessMode()

	/**
	 * Method getExtends
	 * 
	 * 
	 * 
	 * @return ClassDescriptor
	 */
	public org.exolab.castor.mapping.ClassDescriptor getExtends()
	{
		return null;
	} // -- org.exolab.castor.mapping.ClassDescriptor getExtends()

	/**
	 * Method getIdentity
	 * 
	 * 
	 * 
	 * @return FieldDescriptor
	 */
	public org.exolab.castor.mapping.FieldDescriptor getIdentity()
	{
		return identity;
	} // -- org.exolab.castor.mapping.FieldDescriptor getIdentity()

	/**
	 * Method getJavaClass
	 * 
	 * 
	 * 
	 * @return Class
	 */
	public java.lang.Class getJavaClass()
	{
		return com.cosylab.acs.laser.dao.xml.CategoryDefinitions.class;
	} // -- java.lang.Class getJavaClass()

	/**
	 * Method getNameSpacePrefix
	 * 
	 * 
	 * 
	 * @return String
	 */
	public java.lang.String getNameSpacePrefix()
	{
		return nsPrefix;
	} // -- java.lang.String getNameSpacePrefix()

	/**
	 * Method getNameSpaceURI
	 * 
	 * 
	 * 
	 * @return String
	 */
	public java.lang.String getNameSpaceURI()
	{
		return nsURI;
	} // -- java.lang.String getNameSpaceURI()

	/**
	 * Method getValidator
	 * 
	 * 
	 * 
	 * @return TypeValidator
	 */
	public org.exolab.castor.xml.TypeValidator getValidator()
	{
		return this;
	} // -- org.exolab.castor.xml.TypeValidator getValidator()

	/**
	 * Method getXMLName
	 * 
	 * 
	 * 
	 * @return String
	 */
	public java.lang.String getXMLName()
	{
		return xmlName;
	} // -- java.lang.String getXMLName()

}
