/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: AlarmCategoryDefinitionsDescriptor.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

/**
 * Class AlarmCategoryDefinitionsDescriptor.
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 */
public class AlarmCategoryDefinitionsDescriptor extends org.exolab.castor.xml.util.XMLClassDescriptorImpl
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

	public AlarmCategoryDefinitionsDescriptor()
	{
		super();
		nsURI = "urn:schemas-cosylab-com:Alarm:1.0";
		xmlName = "alarm-category-definitions";
		org.exolab.castor.xml.util.XMLFieldDescriptorImpl desc = null;
		org.exolab.castor.mapping.FieldHandler handler = null;
		org.exolab.castor.xml.FieldValidator fieldValidator = null;
		// -- initialize attribute descriptors

		// -- initialize element descriptors

		// -- _categoryLinksToCreate
		desc = new org.exolab.castor.xml.util.XMLFieldDescriptorImpl(
				com.cosylab.acs.laser.dao.xml.CategoryLinksToCreate.class,
				"_categoryLinksToCreate", "category-links-to-create",
				org.exolab.castor.xml.NodeType.Element);
		handler = new org.exolab.castor.xml.XMLFieldHandler() {
			public java.lang.Object getValue(java.lang.Object object)
					throws IllegalStateException
			{
				AlarmCategoryDefinitions target = (AlarmCategoryDefinitions) object;
				return target.getCategoryLinksToCreate();
			}

			public void setValue(java.lang.Object object, java.lang.Object value)
					throws IllegalStateException, IllegalArgumentException
			{
				try {
					AlarmCategoryDefinitions target = (AlarmCategoryDefinitions) object;
					target
							.setCategoryLinksToCreate((com.cosylab.acs.laser.dao.xml.CategoryLinksToCreate) value);
				} catch (java.lang.Exception ex) {
					throw new IllegalStateException(ex.toString());
				}
			}

			public java.lang.Object newInstance(java.lang.Object parent)
			{
				return new com.cosylab.acs.laser.dao.xml.CategoryLinksToCreate();
			}
		};
		desc.setHandler(handler);
		desc.setNameSpaceURI("urn:schemas-cosylab-com:Alarm:1.0");
		desc.setMultivalued(false);
		addFieldDescriptor(desc);

		// -- validation code for: _categoryLinksToCreate
		fieldValidator = new org.exolab.castor.xml.FieldValidator();
		{ // -- local scope
		}
		desc.setValidator(fieldValidator);
		// -- _categoryLinksToRemove
		desc = new org.exolab.castor.xml.util.XMLFieldDescriptorImpl(
				com.cosylab.acs.laser.dao.xml.CategoryLinksToRemove.class,
				"_categoryLinksToRemove", "category-links-to-remove",
				org.exolab.castor.xml.NodeType.Element);
		handler = new org.exolab.castor.xml.XMLFieldHandler() {
			public java.lang.Object getValue(java.lang.Object object)
					throws IllegalStateException
			{
				AlarmCategoryDefinitions target = (AlarmCategoryDefinitions) object;
				return target.getCategoryLinksToRemove();
			}

			public void setValue(java.lang.Object object, java.lang.Object value)
					throws IllegalStateException, IllegalArgumentException
			{
				try {
					AlarmCategoryDefinitions target = (AlarmCategoryDefinitions) object;
					target
							.setCategoryLinksToRemove((com.cosylab.acs.laser.dao.xml.CategoryLinksToRemove) value);
				} catch (java.lang.Exception ex) {
					throw new IllegalStateException(ex.toString());
				}
			}

			public java.lang.Object newInstance(java.lang.Object parent)
			{
				return new com.cosylab.acs.laser.dao.xml.CategoryLinksToRemove();
			}
		};
		desc.setHandler(handler);
		desc.setNameSpaceURI("urn:schemas-cosylab-com:Alarm:1.0");
		desc.setMultivalued(false);
		addFieldDescriptor(desc);

		// -- validation code for: _categoryLinksToRemove
		fieldValidator = new org.exolab.castor.xml.FieldValidator();
		{ // -- local scope
		}
		desc.setValidator(fieldValidator);
	} // -- com.cosylab.acs.laser.dao.xml.AlarmCategoryDefinitionsDescriptor()

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
		return com.cosylab.acs.laser.dao.xml.AlarmCategoryDefinitions.class;
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
