package alma.demo.XmlOffShootImpl;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServices;
import alma.demo.XmlOffshootJ;
import alma.xmljbind.test.obsproject.TargetSpaceT;
import alma.xmljbind.test.obsproposal.ObsProposal;
import alma.xmljbind.test.obsproposal.ObsProposalEntityT;
import alma.xmljbind.test.schedblock.SchedBlock;
import alma.xmljbind.test.schedblock.SchedBlockEntityT;

/**
 * Dummy OffShoot implementation using automatic XML entities serialization/deserialization
 * 
 * @author rtobar, Aug 20th, 2010
 *
 */
public class XmlOffShootImpl implements XmlOffshootJ {

	private ObsProposal obsProposal;
	private SchedBlock  schedBlock;

	public XmlOffShootImpl(ContainerServices cs) {

		// Default obsProposal
		TargetSpaceT[] tSpaces = new TargetSpaceT[2];
		for(int i=0; i!=2; i++) {
			tSpaces[i] = new TargetSpaceT();
			tSpaces[i].setName("target " + i);
		}

		ObsProposalEntityT obsPropEntity = new ObsProposalEntityT();
		SchedBlockEntityT schedBlockEntity = new SchedBlockEntityT();

		try {
			cs.assignUniqueEntityId(obsPropEntity);
			cs.assignUniqueEntityId(schedBlockEntity);
		} catch (AcsJContainerServicesEx e) {
			e.printStackTrace();
		}

		obsProposal = new ObsProposal();
		obsProposal.setObsProposalEntity(obsPropEntity);
		obsProposal.setCode("2010.0045.34S");
		obsProposal.setPI("rtobar");
		obsProposal.setScientificJustification("just for fun");
		obsProposal.setTargetSpace(tSpaces);

		// Default schedBlock
		schedBlock = new SchedBlock();
		schedBlock.setSchedBlockEntity(schedBlockEntity);
		schedBlock.setName("holography");
		schedBlock.setStatus("DONE");
		schedBlock.setStandardMode(true);

	}

	@Override
	public ObsProposal getObsProposal() {
		return obsProposal;
	}

	@Override
	public SchedBlock getSchedBlock() {
		return schedBlock;
	}

	@Override
	public void setObsProposal(ObsProposal obsPropIn) {
		obsProposal = obsPropIn;
	}

	@Override
	public void setSchedBlock(SchedBlock schedBlockIn) {
		schedBlock = schedBlockIn;
	}

}
