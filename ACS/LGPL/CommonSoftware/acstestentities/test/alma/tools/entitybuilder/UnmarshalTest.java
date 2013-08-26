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
package alma.tools.entitybuilder;

import junit.framework.TestCase;
import alma.xmljbind.test.obsproject.ObsProject;
import alma.xmljbind.test.obsproposal.ObsProposal;

/**
 * @author hsommer created Feb 24, 2005 11:58:14 AM
 */
public class UnmarshalTest extends TestCase
{
	private XmlInOut unm;

	protected void setUp() throws Exception {
		unm = new XmlInOut();
	}

	public void testUnmarshalObsProposal() throws Exception {
		ObsProposal oprop = unm.unmarshalObsProposal("ObsProposal1.xml");
		oprop.validate();
		assertEquals("uid://x0/x1/x2", oprop.getObsProposalEntity().getEntityId());
		assertEquals("Pretty damn stable", oprop.getObsPlan().getPreconditions().getWeatherConstraints().getPhaseStability());

		oprop.getDateReceived();
	}

	public void testUnmarshalObsProject() throws Exception {
		ObsProject oproj = unm.unmarshalObsProject("ObsProject1.xml");
		oproj.validate();
	}
}
