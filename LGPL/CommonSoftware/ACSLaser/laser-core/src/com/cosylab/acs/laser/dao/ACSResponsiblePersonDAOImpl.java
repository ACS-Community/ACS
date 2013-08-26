/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *                 and Cosylab
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *    MA 02111-1307  USA
 */
package com.cosylab.acs.laser.dao;

import cern.laser.business.dao.AlarmDAO;
import cern.laser.business.dao.ResponsiblePersonDAO;
import cern.laser.business.data.ResponsiblePerson;

public class ACSResponsiblePersonDAOImpl implements ResponsiblePersonDAO
{
	public static final ResponsiblePerson theResponsiblePerson;
	static {
		theResponsiblePerson=new ResponsiblePerson();
		theResponsiblePerson.setEMail("alarms@localhost");
		theResponsiblePerson.setFirstName("Responsible");
		theResponsiblePerson.setFamilyName("Person");
		theResponsiblePerson.setGsmNumber("+386 1 000 00 00");
		theResponsiblePerson.setPhoneNumber("");
		theResponsiblePerson.setResponsibleId(new Integer(1));
	}
	
	AlarmDAO alarmDAO;
	
	public ACSResponsiblePersonDAOImpl()
	{
		// do nothing
	}
	
	public void setAlarmDAO(AlarmDAO alarmDAO)
	{
		this.alarmDAO=alarmDAO;
	}
	
	public ResponsiblePerson getResponsiblePerson(Integer identifier)
	{
		if (identifier!=null && identifier.intValue()==1)
			return theResponsiblePerson;
		
		return null;
	}

	public String[] getAlarms(Integer responsibleId)
	{
		if (alarmDAO==null)
			throw new IllegalStateException("alarmDAO not set");
		
		if (!(alarmDAO instanceof ACSAlarmDAOImpl)) {
			throw new UnsupportedOperationException();
		}
		
		return ((ACSAlarmDAOImpl)alarmDAO).getAllAlarmIDs();
	}

	public ResponsiblePerson[] findAllResponsiblePersons()
	{
		return new ResponsiblePerson[] { theResponsiblePerson };
	}	
}

/*
 	This is all commented, because we're currently using a single
 	hard-coded responsible person. It may be of use once something
 	else is actually implemented.
 

	
	DAL dal;

	HashMap responsiblePeople = new HashMap();

	public ResponsiblePerson getResponsiblePerson(Integer identifier)
	{
		if (responsiblePeople.containsKey(identifier)) {
			return (ResponsiblePerson) responsiblePeople.get(identifier);
		}

		if (dal==null)
			throw new IllegalStateException("DAL was not set for ACSResponsiblePersonDAOImpl");
		
		// TODO: is this ok?
		String acsIdent = "/AlarmSystem/ResponsiblePeople/"
				+ identifier.intValue();

		String xml = null;
		try {
			xml = dal.get_DAO(acsIdent);

			// TODO: log, throw, ?
		} catch (XMLerror e) {
		} catch (RecordDoesNotExist e) {
		}

		if (xml == null) {
			return null;
		}

		Document doc = null;
		try {
			doc = DAOUtil.parseXML(xml);

			// TODO: log, throw, ?
		} catch (IOException e) {
		} catch (ParserConfigurationException e) {
		} catch (SAXException e) {
		}

		if (doc == null)
			return null;

		Element e = doc.getDocumentElement();
		if (e.getNodeName().equals("responsible-person-definition")) {
			ResponsiblePerson person = parseResponsiblePerson(e);

			if(!person.getResponsibleId().equals(identifier)) {
				throw new IllegalStateException();
			}

			responsiblePeople.put(identifier, person);

			return person;
		} else {
			throw new IllegalStateException(
					"Responsible person definition must contain a <responsible-person-definition>");
		}
	}

	ResponsiblePerson parseResponsiblePerson(Element e)
	{
		Node n = e.getFirstChild();

		String eMail = null, familyName = null, firstName = null, gsmNumber = null, phoneNumber = null;
		Integer responsibleId = null;

		while (n != null) {
			if (n.getNodeType() == Node.ELEMENT_NODE) {
				if ("eMail".equals(n.getNodeName())) {
					eMail = DAOUtil.getTextOfEl(n);
				} else if ("familyName".equals(n.getNodeName())) {
					familyName = DAOUtil.getTextOfEl(n);
				} else if ("firstName".equals(n.getNodeName())) {
					firstName = DAOUtil.getTextOfEl(n);
				} else if ("gsmNumber".equals(n.getNodeName())) {
					gsmNumber = DAOUtil.getTextOfEl(n);
				} else if ("phoneNumber".equals(n.getNodeName())) {
					phoneNumber = DAOUtil.getTextOfEl(n);
				} else if ("responsibleId".equals(n.getNodeName())) {
					String tmp = DAOUtil.getTextOfEl(n);
					try {
						int tmpi = Integer.parseInt(tmp);
						responsibleId = new Integer(tmpi);
					} catch (NumberFormatException ex) {
						throw new IllegalStateException(
								"Responsible person identifier must be an integer");
					}
				} else {
					throw new IllegalStateException(
							"Unknown node in <responsible-person-definition>");
				}
			} else {
				throw new IllegalStateException(
						"Unknown node in <responsible-person-definition>");
			}

			n = n.getNextSibling();
		}

		if (responsibleId == null)
			throw new IllegalStateException(
					"No identifier in <responsible-person-definition>");

		ResponsiblePerson result = new ResponsiblePerson();

		result.setEMail(eMail);
		result.setFamilyName(familyName);
		result.setFirstName(firstName);
		result.setGsmNumber(gsmNumber);
		result.setPhoneNumber(phoneNumber);
		result.setResponsibleId(responsibleId);

		return result;
	}

	public String[] getAlarms(Integer responsibleId)
	{
		throw new UnsupportedOperationException("ACSResponsiblePersonDAOImpl.getAlarms");
	}

	public ResponsiblePerson[] findAllResponsiblePersons()
	{
		if (dal==null)
			throw new IllegalStateException("DAL was not set for ACSResponsiblePersonDAOImpl");

		// TODO: is this OK?
		String acsIdent="/AlarmSystem/ResponsiblePeople/IDList";
		
		String xml;
		
		try {
			xml=dal.get_DAO(acsIdent);
		} catch (XMLerror e) {
			throw new IllegalStateException(acsIdent+" is not valid XML");
		} catch (RecordDoesNotExist e) {
			throw new IllegalStateException(acsIdent+" does not exist");
		}
		
		Document doc;
		try {
			doc=DAOUtil.parseXML(xml);
		} catch (SAXException e1) {
			throw new IllegalStateException(acsIdent+" is not valid XML");
		} catch (IOException e1) {
			// this should never happen at all!
			throw new IllegalStateException();
		} catch (ParserConfigurationException e1) {
			throw new IllegalStateException();
		}
		
		Element mama=doc.getDocumentElement();
		int[] ids;
		if (mama.getNodeName().equals("id-list")) {
			try {
				ids=DAOUtil.parseIntIdList(mama);
			} catch (Exception ex) {
				throw new IllegalStateException("Failed to parse <id-list>: "+ex.getMessage());
			}
		} else {
			throw new IllegalStateException(acsIdent+" must contain <id-list>");
		}
		
		int l=ids.length;
		ResponsiblePerson[] result=new ResponsiblePerson[l];
		for (int a=0; a<l; a++) {
			ResponsiblePerson thisOne=getResponsiblePerson(new Integer(a));
			if (thisOne==null) { 
				throw new IllegalStateException("Responsible person list contains an unknown identifier");
			}
			result[a]=thisOne;
		}
		
		return result;
	}

	public void setDAL(DAL dal)
	{
		this.dal = dal;
	}
}*/