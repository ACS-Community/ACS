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

import java.util.ArrayList;

import alma.entities.commonentity.EntityRefT;
import alma.xmljbind.test.obsproject.ObsProgramT;
import alma.xmljbind.test.obsproject.ObsProject;
import alma.xmljbind.test.obsproject.ObsProjectEntityT;
import alma.xmljbind.test.obsproject.ObsProjectRefT;
import alma.xmljbind.test.obsproject.ObsUnitSetT;
import alma.xmljbind.test.obsproject.ObsUnitSetTChoice;
import alma.xmljbind.test.obsproposal.ObsProposal;
import alma.xmljbind.test.obsproposal.ObsProposalEntityT;
import alma.xmljbind.test.obsproposal.ObsProposalRefT;
import alma.xmljbind.test.schedblock.SchedBlockRefT;

/**
 * @author hsommer
 * created Dec 11, 2003 2:44:19 PM
 */
class TestEntityFactory
{
	ObsProject getObsProject() 
	{
		ObsProject proj = null;

		proj = new ObsProject();

		ObsProjectEntityT entity = new ObsProjectEntityT();
		proj.setObsProjectEntity(entity);

		entity.setEntityId("uid://X1230000000000000/X00000001");
		entity.setSchemaVersion("1.0"); 
		
		ObsProposalRefT ref = new ObsProposalRefT();
		proj.setObsProposalRef(ref);
		
		ref.setEntityId("uid://X1230000000000000/X00000002");
		
		ObsProgramT prog = new ObsProgramT();
		proj.setObsProgram(prog);
		ObsUnitSetT obsPlan = new ObsUnitSetT(); 
		prog.setObsPlan(obsPlan);
		ObsUnitSetTChoice choice = new ObsUnitSetTChoice();
		obsPlan.setObsUnitSetTChoice(choice);
		SchedBlockRefT schedBlockRef1 = new SchedBlockRefT();
		choice.addSchedBlockRef(schedBlockRef1);
		schedBlockRef1.setEntityId("uid://X1230000000000000/X00000003");
		
		return proj;
	} 

	ObsProposal getObsProposal() 
	{
		ObsProposal prop = null;

		prop = new ObsProposal();

		ObsProposalEntityT entity = new ObsProposalEntityT();
		prop.setObsProposalEntity(entity);
		entity.setEntityId("uid://X1230000000000000/X00000002");

		ObsProjectRefT ref = new ObsProjectRefT();
		ref.setEntityId("uid://X1230000000000000/X00000001");
		prop.setObsProjectRef(ref);

		return prop;
	}


//	SchedBlock[] getSchedblocks() 
//	{
//		ArrayList schedBlockList = new ArrayList();
//
//		SchedBlock sb1 = new SchedBlock();
//		schedBlockList.add(sb1);
//
//		SchedBlockEntityT entity = new SchedBlockEntityT();
//		sb1.setSchedBlockEntity(entity);
//
//		entity.setEntityId("uid://X1230000000000000/X00000003");
//		
//		ObsProjectRefT ref = new ObsProjectRefT();
//		ref.setEntityId("uid://X1230000000000000/X00000001");
//		sb1.setObsProjectRef(ref);
//
//		return (SchedBlock[]) schedBlockList.toArray(new SchedBlock[0]);
//	}
//
	/**
	 * Returns (hardcoded) the EntityRef objects that are part of the ObsProject
	 * that can be retrieved from {@link #getObsProject}.
	 * Needed to test the code that dynamically finds these objects.
	 * @return EntityRefT[]
	 */
	EntityRefT[] getReferencesInObsProject(ObsProject proj) 
	{
		ArrayList<EntityRefT> entityRefs = new ArrayList<EntityRefT>();

		EntityRefT propRef = proj.getObsProposalRef();
		entityRefs.add(propRef);

		EntityRefT sb1Ref =
		proj
		.getObsProgram()
		.getObsPlan()
		.getObsUnitSetTChoice()
		.getSchedBlockRef()[0];
		entityRefs.add(sb1Ref);

		return entityRefs.toArray(new EntityRefT[0]);
	}


}
