/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: SourceDefinitionsDescriptor.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

/**
 * Class SourceDefinitionsDescriptor.
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 */
public class SourceDefinitionsDescriptor extends org.exolab.castor.xml.util.XMLClassDescriptorImpl
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

	public SourceDefinitionsDescriptor()
	{
		super();
		nsURI = "urn:schemas-cosylab-com:Alarm:1.0";
		xmlName = "source-definitions";
		org.exolab.castor.xml.util.XMLFieldDescriptorImpl desc = null;
		org.exolab.castor.mapping.FieldHandler handler = null;
		org.exolab.castor.xml.FieldValidator fieldValidator = null;
		// -- initialize attribute descriptors

		// -- initialize element descriptors

		// -- _sourcesToCreate
		desc = new org.exolab.castor.xml.util.XMLFieldDescriptorImpl(
				com.cosylab.acs.laser.dao.xml.SourcesToCreate.class,
				"_sourcesToCreate", "sources-to-create",
				org.exolab.castor.xml.NodeType.Element);
		handler = new org.exolab.castor.xml.XMLFieldHandler() {
			public java.lang.Object getValue(java.lang.Object object)
					throws IllegalStateException
			{
				SourceDefinitions target = (SourceDefinitions) object;
				return target.getSourcesToCreate();
			}

			public void setValue(java.lang.Object object, java.lang.Object value)
					throws IllegalStateException, IllegalArgumentException
			{
				try {
					SourceDefinitions target = (SourceDefinitions) object;
					target
							.setSourcesToCreate((com.cosylab.acs.laser.dao.xml.SourcesToCreate) value);
				} catch (java.lang.Exception ex) {
					throw new IllegalStateException(ex.toString());
				}
			}

			public java.lang.Object newInstance(java.lang.Object parent)
			{
				return new com.cosylab.acs.laser.dao.xml.SourcesToCreate();
			}
		};
		desc.setHandler(handler);
		desc.setNameSpaceURI("urn:schemas-cosylab-com:Alarm:1.0");
		desc.setMultivalued(false);
		addFieldDescriptor(desc);

		// -- validation code for: _sourcesToCreate
		fieldValidator = new org.exolab.castor.xml.FieldValidator();
		{ // -- local scope
		}
		desc.setValidator(fieldValidator);
		// -- _sourcesToUpdate
		desc = new org.exolab.castor.xml.util.XMLFieldDescriptorImpl(
				com.cosylab.acs.laser.dao.xml.SourcesToUpdate.class,
				"_sourcesToUpdate", "sources-to-update",
				org.exolab.castor.xml.NodeType.Element);
		handler = new org.exolab.castor.xml.XMLFieldHandler() {
			public java.lang.Object getValue(java.lang.Object object)
					throws IllegalStateException
			{
				SourceDefinitions target = (SourceDefinitions) object;
				return target.getSourcesToUpdate();
			}

			public void setValue(java.lang.Object object, java.lang.Object value)
					throws IllegalStateException, IllegalArgumentException
			{
				try {
					SourceDefinitions target = (SourceDefinitions) object;
					target
							.setSourcesToUpdate((com.cosylab.acs.laser.dao.xml.SourcesToUpdate) value);
				} catch (java.lang.Exception ex) {
					throw new IllegalStateException(ex.toString());
				}
			}

			public java.lang.Object newInstance(java.lang.Object parent)
			{
				return new com.cosylab.acs.laser.dao.xml.SourcesToUpdate();
			}
		};
		desc.setHandler(handler);
		desc.setNameSpaceURI("urn:schemas-cosylab-com:Alarm:1.0");
		desc.setMultivalued(false);
		addFieldDescriptor(desc);

		// -- validation code for: _sourcesToUpdate
		fieldValidator = new org.exolab.castor.xml.FieldValidator();
		{ // -- local scope
		}
		desc.setValidator(fieldValidator);
		// -- _sourcesToRemove
		desc = new org.exolab.castor.xml.util.XMLFieldDescriptorImpl(
				com.cosylab.acs.laser.dao.xml.SourcesToRemove.class,
				"_sourcesToRemove", "sources-to-remove",
				org.exolab.castor.xml.NodeType.Element);
		handler = new org.exolab.castor.xml.XMLFieldHandler() {
			public java.lang.Object getValue(java.lang.Object object)
					throws IllegalStateException
			{
				SourceDefinitions target = (SourceDefinitions) object;
				return target.getSourcesToRemove();
			}

			public void setValue(java.lang.Object object, java.lang.Object value)
					throws IllegalStateException, IllegalArgumentException
			{
				try {
					SourceDefinitions target = (SourceDefinitions) object;
					target
							.setSourcesToRemove((com.cosylab.acs.laser.dao.xml.SourcesToRemove) value);
				} catch (java.lang.Exception ex) {
					throw new IllegalStateException(ex.toString());
				}
			}

			public java.lang.Object newInstance(java.lang.Object parent)
			{
				return new com.cosylab.acs.laser.dao.xml.SourcesToRemove();
			}
		};
		desc.setHandler(handler);
		desc.setNameSpaceURI("urn:schemas-cosylab-com:Alarm:1.0");
		desc.setMultivalued(false);
		addFieldDescriptor(desc);

		// -- validation code for: _sourcesToRemove
		fieldValidator = new org.exolab.castor.xml.FieldValidator();
		{ // -- local scope
		}
		desc.setValidator(fieldValidator);
	} // -- com.cosylab.acs.laser.dao.xml.SourceDefinitionsDescriptor()

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
		return com.cosylab.acs.laser.dao.xml.SourceDefinitions.class;
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
