/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
 *    Copyright by ESO (in the framework of the ALMA collaboration),
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
package alma.ACSCOURSE_MOUNT.Mount6Impl;

import java.util.concurrent.ThreadFactory;
import java.util.logging.Level;

import alma.ACS.ROdouble;
import alma.ACS.ROdoubleHelper;
import alma.ACS.ROdoublePOATie;
import alma.ACS.impl.CharacteristicComponentImpl;
import alma.ACS.impl.ROdoubleImpl;
import alma.ACSCOURSE_MOUNT.Mount6J;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acscourse.xmlbinding.myxmlconfigdata.MyNestedDataT;
import alma.acscourse.xmlbinding.myxmlconfigdata.MyXmlConfigData;
import alma.entities.commonentity.EntityT;

/**
 * @author hsommer
 * created Aug 16, 2004 4:22:51 PM
 */
public class Mount6Impl extends CharacteristicComponentImpl implements Mount6J
{
	private ROdouble m_actAz;
//	private DataAccess m_actAzDataAccess;
	private ROdouble m_actEl;
	private ROdouble m_cmdAz;
	private ROdouble m_cmdEl;

	
	public void initialize(ContainerServices containerServices) throws ComponentLifecycleException {

		super.initialize(containerServices);
		
		try
		{ 
			ROdoubleImpl actAzImpl = new ROdoubleImpl("actAz", this);
//			m_actAzDataAccess = actAzImpl.getDataAccess();
			ROdoublePOATie actAzTie = new ROdoublePOATie(actAzImpl);
			m_actAz = ROdoubleHelper.narrow(this.registerProperty(actAzImpl, actAzTie));

			ROdoubleImpl actElImpl = new ROdoubleImpl("actEl", this);
			ROdoublePOATie actElTie = new ROdoublePOATie(actElImpl);
			m_actEl = ROdoubleHelper.narrow(this.registerProperty(actElImpl, actElTie));

			ROdoubleImpl cmdAzImpl = new ROdoubleImpl("cmdAz", this);
			ROdoublePOATie cmdAzTie = new ROdoublePOATie(cmdAzImpl);
			m_cmdAz = ROdoubleHelper.narrow(this.registerProperty(cmdAzImpl, cmdAzTie));

			ROdoubleImpl cmdElImpl = new ROdoubleImpl("cmdEl", this);
			ROdoublePOATie cmdElTie = new ROdoublePOATie(cmdElImpl);
			m_cmdEl = ROdoubleHelper.narrow(this.registerProperty(cmdElImpl, cmdElTie));
		}
		catch (Throwable th)
		{
			throw new ComponentLifecycleException("Failed to create properties.", th); 
		}
	}
	

	/**
	 * Creates and returns binding classes which the container will automatically serialize to the following XML
	 * string which can be seen for example in the objexp client. 
	 * 	<verbatim>
	 * 	  &lt;?xml version=&quot;1.0&quot; encoding=&quot;ISO-8859-1&quot;?&gt;<br>
	 * 	  &lt;ns1:MyXmlConfigData someAttribute=&quot;great attribute here!&quot; xmlns:ns1=&quot;AlmaTest/MyXmlConfigData&quot;&gt;<br>
	 * 	  &lt;ns1:MyXmlConfigDataEntity<br>
	 * 	  entityIdEncrypted=&quot;-- id encryption not yet implemented --&quot;<br>
	 * 	  entityId=&quot;uid://X96fe17ec9c9de1be/X00000000&quot; entityTypeName=&quot;MyXmlConfigData&quot;/&gt;<br>
	 * 	  &lt;ns1:myStringData&gt;string child elements looks like a string attribute 
	 * 	  when using binding classes...&lt;/ns1:myStringData&gt;<br>
	 * 	  &lt;ns1:childData&gt;<br>
	 * 	  &lt;ns1:someNestedData/&gt;<br>
	 * 	  &lt;ns1:someOtherData&gt;one more silly string&lt;/ns1:someOtherData&gt;<br>
	 * 	  &lt;/ns1:childData&gt;<br>
	 * 	  &lt;/ns1:MyXmlConfigData&gt;<br>
	 * 	</verbatim>
	 */
	public MyXmlConfigData createMyXmlConfigData()
	{
		MyXmlConfigData xmlData = new MyXmlConfigData();
		
		try
		{
			EntityT entity = new EntityT();
			// the next line could be omitted if we had defined a subtype of EntityT in the XML schema, 
			// as it is done in the schemas of module acstestentities
			entity.setEntityTypeName("MyXmlConfigData"); 
			m_containerServices.assignUniqueEntityId(entity);
			xmlData.setMyXmlConfigDataEntity(entity);
			
			// note how we use the type-safe methods: "SomeAttribute" and "MyStringData" are names derived from the schema
			xmlData.setSomeAttribute("great attribute here!");
			xmlData.setMyStringData("string child elements looks like a string attribute when using binding classes...");
			
			MyNestedDataT childData = new MyNestedDataT();
			childData.setSomeOtherData("one more silly string");
			xmlData.setChildData(childData);
			
			// of the possible 0..7 (grand)child elements, we just add 1
			MyNestedDataT grandchildData = new MyNestedDataT();
			childData.addSomeNestedData(grandchildData);
		}
		catch (AcsJContainerServicesEx e)
		{
			m_logger.log(Level.SEVERE, "failed to create ObsProposal. ", e);
		}
		
		return xmlData;
	}
	
	
	public ROdouble actAz()
	{
		return m_actAz;
	}
	public ROdouble actEl()
	{
		return m_actEl;
	}
	public ROdouble cmdAz()
	{
		return m_cmdAz;
	}
	public ROdouble cmdEl()
	{
		return m_cmdEl;
	}
}

