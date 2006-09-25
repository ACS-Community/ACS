/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: AlarmDefinitionDescriptor.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.validators.IntegerValidator;
import org.exolab.castor.xml.validators.StringValidator;

/**
 * Class AlarmDefinitionDescriptor.
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 */
public class AlarmDefinitionDescriptor extends com.cosylab.acs.laser.dao.xml.AlarmDefinitionTypeDescriptor
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

	public AlarmDefinitionDescriptor()
	{
		super();
		setExtendsWithoutFlatten(new com.cosylab.acs.laser.dao.xml.AlarmDefinitionTypeDescriptor());
		nsURI = "urn:schemas-cosylab-com:Alarm:1.0";
		xmlName = "alarm-definition";
		org.exolab.castor.xml.util.XMLFieldDescriptorImpl desc = null;
		org.exolab.castor.mapping.FieldHandler handler = null;
		org.exolab.castor.xml.FieldValidator fieldValidator = null;
		// -- initialize attribute descriptors

		// -- _faultFamily
		desc = new org.exolab.castor.xml.util.XMLFieldDescriptorImpl(
				java.lang.String.class, "_faultFamily", "fault-family",
				org.exolab.castor.xml.NodeType.Attribute);
		desc.setImmutable(true);
		handler = new org.exolab.castor.xml.XMLFieldHandler() {
			public java.lang.Object getValue(java.lang.Object object)
					throws IllegalStateException
			{
				AlarmDefinition target = (AlarmDefinition) object;
				return target.getFaultFamily();
			}

			public void setValue(java.lang.Object object, java.lang.Object value)
					throws IllegalStateException, IllegalArgumentException
			{
				try {
					AlarmDefinition target = (AlarmDefinition) object;
					target.setFaultFamily((java.lang.String) value);
				} catch (java.lang.Exception ex) {
					throw new IllegalStateException(ex.toString());
				}
			}

			public java.lang.Object newInstance(java.lang.Object parent)
			{
				return null;
			}
		};
		desc.setHandler(handler);
		desc.setRequired(true);
		desc.setMultivalued(false);
		addFieldDescriptor(desc);

		// -- validation code for: _faultFamily
		fieldValidator = new org.exolab.castor.xml.FieldValidator();
		fieldValidator.setMinOccurs(1);
		{ // -- local scope
			StringValidator typeValidator = new StringValidator();
			typeValidator.setMaxLength(255);
			typeValidator.setWhiteSpace("preserve");
			fieldValidator.setValidator(typeValidator);
		}
		desc.setValidator(fieldValidator);
		// -- _faultMember
		desc = new org.exolab.castor.xml.util.XMLFieldDescriptorImpl(
				java.lang.String.class, "_faultMember", "fault-member",
				org.exolab.castor.xml.NodeType.Attribute);
		desc.setImmutable(true);
		handler = new org.exolab.castor.xml.XMLFieldHandler() {
			public java.lang.Object getValue(java.lang.Object object)
					throws IllegalStateException
			{
				AlarmDefinition target = (AlarmDefinition) object;
				return target.getFaultMember();
			}

			public void setValue(java.lang.Object object, java.lang.Object value)
					throws IllegalStateException, IllegalArgumentException
			{
				try {
					AlarmDefinition target = (AlarmDefinition) object;
					target.setFaultMember((java.lang.String) value);
				} catch (java.lang.Exception ex) {
					throw new IllegalStateException(ex.toString());
				}
			}

			public java.lang.Object newInstance(java.lang.Object parent)
			{
				return null;
			}
		};
		desc.setHandler(handler);
		desc.setRequired(true);
		desc.setMultivalued(false);
		addFieldDescriptor(desc);

		// -- validation code for: _faultMember
		fieldValidator = new org.exolab.castor.xml.FieldValidator();
		fieldValidator.setMinOccurs(1);
		{ // -- local scope
			StringValidator typeValidator = new StringValidator();
			typeValidator.setMaxLength(255);
			typeValidator.setWhiteSpace("preserve");
			fieldValidator.setValidator(typeValidator);
		}
		desc.setValidator(fieldValidator);
		// -- _faultCode
		desc = new org.exolab.castor.xml.util.XMLFieldDescriptorImpl(
				java.lang.Integer.TYPE, "_faultCode", "fault-code",
				org.exolab.castor.xml.NodeType.Attribute);
		handler = new org.exolab.castor.xml.XMLFieldHandler() {
			public java.lang.Object getValue(java.lang.Object object)
					throws IllegalStateException
			{
				AlarmDefinition target = (AlarmDefinition) object;
				if (!target.hasFaultCode())
					return null;
				return new java.lang.Integer(target.getFaultCode());
			}

			public void setValue(java.lang.Object object, java.lang.Object value)
					throws IllegalStateException, IllegalArgumentException
			{
				try {
					AlarmDefinition target = (AlarmDefinition) object;
					// ignore null values for non optional primitives
					if (value == null)
						return;

					target.setFaultCode(((java.lang.Integer) value).intValue());
				} catch (java.lang.Exception ex) {
					throw new IllegalStateException(ex.toString());
				}
			}

			public java.lang.Object newInstance(java.lang.Object parent)
			{
				return null;
			}
		};
		desc.setHandler(handler);
		desc.setRequired(true);
		desc.setMultivalued(false);
		addFieldDescriptor(desc);

		// -- validation code for: _faultCode
		fieldValidator = new org.exolab.castor.xml.FieldValidator();
		fieldValidator.setMinOccurs(1);
		{ // -- local scope
			IntegerValidator typeValidator = new IntegerValidator();
			typeValidator.setMinInclusive(0);
			typeValidator.setMaxInclusive(2147483647);
			fieldValidator.setValidator(typeValidator);
		}
		desc.setValidator(fieldValidator);
		// -- initialize element descriptors

	} // -- com.cosylab.acs.laser.dao.xml.AlarmDefinitionDescriptor()

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
		return super.getExtends();
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
		if (identity == null)
			return super.getIdentity();
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
		return com.cosylab.acs.laser.dao.xml.AlarmDefinition.class;
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
