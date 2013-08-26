/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
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
package alma.acs.entityutil;

import java.io.StringReader;
import java.io.StringWriter;

import junit.framework.TestCase;

import alma.entities.commonentity.EntityRefT;
import alma.xmljbind.test.obsproject.ObsProject;
import alma.xmljbind.test.obsproposal.ObsProposal;
import alma.xmljbind.test.obsproposal.ObsProposalRefT;

/**
 * @author hsommer Apr 24, 2003 2:48:40 PM
 */
public class EntityRefFinderTest extends TestCase
{
	private EntityRefFinder m_entityRefFinder;
	private TestEntityFactory m_entityFactory;

	/**
	 * @see TestCase#setUp()
	 */
	protected void setUp() throws Exception
	{
		super.setUp();
		m_entityRefFinder = new EntityRefFinder(true); // toggle for debug output
		m_entityFactory = new TestEntityFactory();
	}

	
	public void testObsProject() throws Throwable
	{
		ObsProject proj = m_entityFactory.getObsProject();
		EntityRefT[] refs = m_entityRefFinder.findEntityReferences(proj);		
		assertNotNull(refs);
		
		EntityRefT[] knownRefs = m_entityFactory.getReferencesInObsProject(proj);		
		assertTrue("correct number of entity references", 
			(refs.length == knownRefs.length) );
		
		for (int i = 0; i < refs.length; i++)
		{
			assertNotNull("ref not null", refs[i]);
			assertTrue(refs[i] == knownRefs[i]);
		}

		// for manual inspection of output
		if (refs.length > 0 && (refs[0] instanceof ObsProposalRefT) ) {
			StringWriter writer = new StringWriter();
			refs[0].marshal(writer);
			String serializedObsProposalRef = writer.toString();
			System.out.println("serialized ObsProposalRefT object:");
			System.out.println(serializedObsProposalRef);
			ObsProposalRefT recreatedRef = ObsProposalRefT.unmarshalObsProposalRefT(new StringReader(serializedObsProposalRef));
			writer = new StringWriter();
			System.out.println("recreated and reserialized ObsProposalRefT:");
			recreatedRef.marshal(writer);
			String reserializedObsProposalRef = writer.toString();
			System.out.println(reserializedObsProposalRef);
		}
		
		
//		assertTrue("reference to ObsProject", refs[0] instanceof ObsProjectRefT);
//		String projId = ((ObsProjectRefT) refs[0]).getEntityId();
//		assertEquals("correct id", "dummyObsProjectId", projId);
	}

	public void testObsProposal() throws Throwable
	{
		ObsProposal prop = m_entityFactory.getObsProposal();
		EntityRefT[] refs = m_entityRefFinder.findEntityReferences(prop);
		assertNotNull(refs);
//		assertTrue("one entity reference", (refs.length == 1 && refs[0] != null) );
//		assertTrue("reference to ObsProject", refs[0] instanceof ObsProjectRefT);
//		String projId = ((ObsProjectRefT) refs[0]).getEntityId();
//		assertEquals("correct id", "dummyObsProjectId", projId);
	}



	public static void main(String[] args)
	{
		junit.textui.TestRunner.run(EntityRefFinderTest.class);
	}

}
