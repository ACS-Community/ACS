/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: AlarmDefinitionType.java,v 1.2 2005/06/14 15:21:12 mslenc Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * Class AlarmDefinitionType.
 * 
 * @version $Revision: 1.2 $ $Date: 2005/06/14 15:21:12 $
 */
public class AlarmDefinitionType implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * FS visual fields
	 */
	private com.cosylab.acs.laser.dao.xml.VisualFields _visualFields;

	/**
	 * binary/instant FS
	 */
	private boolean _instant = false;

	/**
	 * keeps track of state for field: _instant
	 */
	private boolean _has_instant;

	/**
	 * FS cause description
	 */
	private java.lang.String _cause;

	/**
	 * FS action to be taken
	 */
	private java.lang.String _action;

	/**
	 * FS consequence description
	 */
	private java.lang.String _consequence;

	/**
	 * FS priority
	 */
	private int _priority = 0;

	/**
	 * keeps track of state for field: _priority
	 */
	private boolean _has_priority;

	/**
	 * FS responsible person CERN identifier
	 */
	private int _responsibleId;

	/**
	 * keeps track of state for field: _responsibleId
	 */
	private boolean _has_responsibleId;

	/**
	 * FS piquet GSM number
	 */
	private java.lang.String _piquetGSM;

	/**
	 * FS help information URL
	 */
	private java.lang.String _helpUrl;

	/**
	 * FS source name
	 */
	private java.lang.String _sourceName;

	/**
	 * FS location
	 */
	private com.cosylab.acs.laser.dao.xml.Location _location;

	/**
	 * FS piquet Email address
	 */
	private java.lang.String _piquetEmail;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public AlarmDefinitionType()
	{
		super();
	} // -- com.cosylab.acs.laser.dao.xml.AlarmDefinitionType()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Method deleteInstant
	 * 
	 */
	public void deleteInstant()
	{
		this._has_instant = false;
	} // -- void deleteInstant()

	/**
	 * Method deletePriority
	 * 
	 */
	public void deletePriority()
	{
		this._has_priority = false;
	} // -- void deletePriority()

	/**
	 * Method deleteResponsibleId
	 * 
	 */
	public void deleteResponsibleId()
	{
		this._has_responsibleId = false;
	} // -- void deleteResponsibleId()

	/**
	 * Returns the value of field 'action'. The field 'action' has the following
	 * description: FS action to be taken
	 * 
	 * @return String
	 * @return the value of field 'action'.
	 */
	public java.lang.String getAction()
	{
		return this._action;
	} // -- java.lang.String getAction()

	/**
	 * Returns the value of field 'cause'. The field 'cause' has the following
	 * description: FS cause description
	 * 
	 * @return String
	 * @return the value of field 'cause'.
	 */
	public java.lang.String getCause()
	{
		return this._cause;
	} // -- java.lang.String getCause()

	/**
	 * Returns the value of field 'consequence'. The field 'consequence' has the
	 * following description: FS consequence description
	 * 
	 * @return String
	 * @return the value of field 'consequence'.
	 */
	public java.lang.String getConsequence()
	{
		return this._consequence;
	} // -- java.lang.String getConsequence()

	/**
	 * Returns the value of field 'helpUrl'. The field 'helpUrl' has the
	 * following description: FS help information URL
	 * 
	 * @return String
	 * @return the value of field 'helpUrl'.
	 */
	public java.lang.String getHelpUrl()
	{
		return this._helpUrl;
	} // -- java.lang.String getHelpUrl()

	/**
	 * Returns the value of field 'instant'. The field 'instant' has the
	 * following description: binary/instant FS
	 * 
	 * @return boolean
	 * @return the value of field 'instant'.
	 */
	public boolean getInstant()
	{
		return this._instant;
	} // -- boolean getInstant()

	/**
	 * Returns the value of field 'location'. The field 'location' has the
	 * following description: FS location
	 * 
	 * @return Location
	 * @return the value of field 'location'.
	 */
	public com.cosylab.acs.laser.dao.xml.Location getLocation()
	{
		return this._location;
	} // -- com.cosylab.acs.laser.dao.xml.Location getLocation()

	/**
	 * Returns the value of field 'piquetEmail'. The field 'piquetEmail' has the
	 * following description: FS piquet Email address
	 * 
	 * @return String
	 * @return the value of field 'piquetEmail'.
	 */
	public java.lang.String getPiquetEmail()
	{
		return this._piquetEmail;
	} // -- java.lang.String getPiquetEmail()

	/**
	 * Returns the value of field 'piquetGSM'. The field 'piquetGSM' has the
	 * following description: FS piquet GSM number
	 * 
	 * @return String
	 * @return the value of field 'piquetGSM'.
	 */
	public java.lang.String getPiquetGSM()
	{
		return this._piquetGSM;
	} // -- java.lang.String getPiquetGSM()

	/**
	 * Returns the value of field 'priority'. The field 'priority' has the
	 * following description: FS priority
	 * 
	 * @return int
	 * @return the value of field 'priority'.
	 */
	public int getPriority()
	{
		return this._priority;
	} // -- int getPriority()

	/**
	 * Returns the value of field 'responsibleId'. The field 'responsibleId' has
	 * the following description: FS responsible person CERN identifier
	 * 
	 * @return int
	 * @return the value of field 'responsibleId'.
	 */
	public int getResponsibleId()
	{
		return this._responsibleId;
	} // -- int getResponsibleId()

	/**
	 * Returns the value of field 'sourceName'. The field 'sourceName' has the
	 * following description: FS source name
	 * 
	 * @return String
	 * @return the value of field 'sourceName'.
	 */
	public java.lang.String getSourceName()
	{
		return this._sourceName;
	} // -- java.lang.String getSourceName()

	/**
	 * Returns the value of field 'visualFields'. The field 'visualFields' has
	 * the following description: FS visual fields
	 * 
	 * @return VisualFields
	 * @return the value of field 'visualFields'.
	 */
	public com.cosylab.acs.laser.dao.xml.VisualFields getVisualFields()
	{
		return this._visualFields;
	} // -- com.cosylab.acs.laser.dao.xml.VisualFields getVisualFields()

	/**
	 * Method hasInstant
	 * 
	 * 
	 * 
	 * @return boolean
	 */
	public boolean hasInstant()
	{
		return this._has_instant;
	} // -- boolean hasInstant()

	/**
	 * Method hasPriority
	 * 
	 * 
	 * 
	 * @return boolean
	 */
	public boolean hasPriority()
	{
		return this._has_priority;
	} // -- boolean hasPriority()

	/**
	 * Method hasResponsibleId
	 * 
	 * 
	 * 
	 * @return boolean
	 */
	public boolean hasResponsibleId()
	{
		return this._has_responsibleId;
	} // -- boolean hasResponsibleId()

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
	 * Sets the value of field 'action'. The field 'action' has the following
	 * description: FS action to be taken
	 * 
	 * @param action
	 *            the value of field 'action'.
	 */
	public void setAction(java.lang.String action)
	{
		this._action = action;
	} // -- void setAction(java.lang.String)

	/**
	 * Sets the value of field 'cause'. The field 'cause' has the following
	 * description: FS cause description
	 * 
	 * @param cause
	 *            the value of field 'cause'.
	 */
	public void setCause(java.lang.String cause)
	{
		this._cause = cause;
	} // -- void setCause(java.lang.String)

	/**
	 * Sets the value of field 'consequence'. The field 'consequence' has the
	 * following description: FS consequence description
	 * 
	 * @param consequence
	 *            the value of field 'consequence'.
	 */
	public void setConsequence(java.lang.String consequence)
	{
		this._consequence = consequence;
	} // -- void setConsequence(java.lang.String)

	/**
	 * Sets the value of field 'helpUrl'. The field 'helpUrl' has the following
	 * description: FS help information URL
	 * 
	 * @param helpUrl
	 *            the value of field 'helpUrl'.
	 */
	public void setHelpUrl(java.lang.String helpUrl)
	{
		this._helpUrl = helpUrl;
	} // -- void setHelpUrl(java.lang.String)

	/**
	 * Sets the value of field 'instant'. The field 'instant' has the following
	 * description: binary/instant FS
	 * 
	 * @param instant
	 *            the value of field 'instant'.
	 */
	public void setInstant(boolean instant)
	{
		this._instant = instant;
		this._has_instant = true;
	} // -- void setInstant(boolean)

	/**
	 * Sets the value of field 'location'. The field 'location' has the
	 * following description: FS location
	 * 
	 * @param location
	 *            the value of field 'location'.
	 */
	public void setLocation(com.cosylab.acs.laser.dao.xml.Location location)
	{
		this._location = location;
	} // -- void setLocation(com.cosylab.acs.laser.dao.xml.Location)

	/**
	 * Sets the value of field 'piquetEmail'. The field 'piquetEmail' has the
	 * following description: FS piquet Email address
	 * 
	 * @param piquetEmail
	 *            the value of field 'piquetEmail'.
	 */
	public void setPiquetEmail(java.lang.String piquetEmail)
	{
		this._piquetEmail = piquetEmail;
	} // -- void setPiquetEmail(java.lang.String)

	/**
	 * Sets the value of field 'piquetGSM'. The field 'piquetGSM' has the
	 * following description: FS piquet GSM number
	 * 
	 * @param piquetGSM
	 *            the value of field 'piquetGSM'.
	 */
	public void setPiquetGSM(java.lang.String piquetGSM)
	{
		this._piquetGSM = piquetGSM;
	} // -- void setPiquetGSM(java.lang.String)

	/**
	 * Sets the value of field 'priority'. The field 'priority' has the
	 * following description: FS priority
	 * 
	 * @param priority
	 *            the value of field 'priority'.
	 */
	public void setPriority(int priority)
	{
		this._priority = priority;
		this._has_priority = true;
	} // -- void setPriority(int)

	/**
	 * Sets the value of field 'responsibleId'. The field 'responsibleId' has
	 * the following description: FS responsible person CERN identifier
	 * 
	 * @param responsibleId
	 *            the value of field 'responsibleId'.
	 */
	public void setResponsibleId(int responsibleId)
	{
		this._responsibleId = responsibleId;
		this._has_responsibleId = true;
	} // -- void setResponsibleId(int)

	/**
	 * Sets the value of field 'sourceName'. The field 'sourceName' has the
	 * following description: FS source name
	 * 
	 * @param sourceName
	 *            the value of field 'sourceName'.
	 */
	public void setSourceName(java.lang.String sourceName)
	{
		this._sourceName = sourceName;
	} // -- void setSourceName(java.lang.String)

	/**
	 * Sets the value of field 'visualFields'. The field 'visualFields' has the
	 * following description: FS visual fields
	 * 
	 * @param visualFields
	 *            the value of field 'visualFields'.
	 */
	public void setVisualFields(
			com.cosylab.acs.laser.dao.xml.VisualFields visualFields)
	{
		this._visualFields = visualFields;
	} // -- void setVisualFields(com.cosylab.acs.laser.dao.xml.VisualFields)

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
		return (com.cosylab.acs.laser.dao.xml.AlarmDefinitionType) Unmarshaller
				.unmarshal(
						com.cosylab.acs.laser.dao.xml.AlarmDefinitionType.class,
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
