/*
 * Created on Oct 21, 2004 by mschilli
 */
package alma.acs.callbacks;

import alma.ACS.CBDescIn;
import alma.ACS.CBdouble;
import alma.ACS.CBdoubleHelper;
import alma.ACS.CBdoubleSeq;
import alma.ACS.CBdoubleSeqHelper;
import alma.ACS.CBlong;
import alma.ACS.CBlongHelper;
import alma.ACS.CBlongSeq;
import alma.ACS.CBlongSeqHelper;
import alma.ACS.CBstring;
import alma.ACS.CBstringHelper;
import alma.ACS.CBstringSeq;
import alma.ACS.CBstringSeqHelper;
import alma.ACS.OffShoot;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServicesBase;



/**
 * 
 * @author mschilli
 */
public class RequesterUtil {

	
	// ========================================================
	// =====================  Requests  =======================
	// ========================================================

	
	/**
	 * 
	 * @param cs
	 * @param x
	 * @return
	 * @throws AcsJContainerServicesEx
	 */
	static public CBstring giveCBString (ContainerServicesBase cs, ResponseReceiver<String> x) throws AcsJContainerServicesEx {
		CBstring ret;

		Objects.CBstringImpl cb = new Objects.CBstringImpl(x);
		OffShoot offshoot = cs.activateOffShoot(cb);
		ret = CBstringHelper.narrow(offshoot);

		return ret;
	}

	/**
	 * 
	 * @param cs
	 * @param x
	 * @return
	 * @throws AcsJContainerServicesEx
	 */
	static public CBstringSeq giveCBStringSequence (ContainerServicesBase cs, ResponseReceiver<String[]> x) throws AcsJContainerServicesEx {
		CBstringSeq ret;

		Objects.CBstringSeqImpl cb = new Objects.CBstringSeqImpl(x);
		OffShoot offshoot = cs.activateOffShoot(cb);
		ret = CBstringSeqHelper.narrow(offshoot);

		return ret;
	}


	/**
	 * 
	 * @param cs
	 * @param x
	 * @return
	 * @throws AcsJContainerServicesEx
	 */
	static public CBlong giveCBLong (ContainerServicesBase cs, ResponseReceiver<Integer> x) throws AcsJContainerServicesEx {
		CBlong ret;

		Objects.CBlongImpl cb = new Objects.CBlongImpl(x);
		OffShoot offshoot = cs.activateOffShoot(cb);
		ret = CBlongHelper.narrow(offshoot);

		return ret;
	}

	/**
	 * 
	 * @param cs
	 * @param x
	 * @return
	 * @throws AcsJContainerServicesEx
	 */
	static public CBlongSeq giveCBLongSequence (ContainerServicesBase cs, ResponseReceiver<Integer[]> x) throws AcsJContainerServicesEx {
		CBlongSeq ret;

		Objects.CBlongSeqImpl cb = new Objects.CBlongSeqImpl(x);
		OffShoot offshoot = cs.activateOffShoot(cb);
		ret = CBlongSeqHelper.narrow(offshoot);

		return ret;
	}
	
	
	/**
	 * 
	 * @param cs
	 * @param x
	 * @return
	 * @throws AcsJContainerServicesEx
	 */
	static public CBdouble giveCBDouble (ContainerServicesBase cs, ResponseReceiver<Double> x) throws AcsJContainerServicesEx {
		CBdouble ret;

		Objects.CBdoubleImpl cb = new Objects.CBdoubleImpl(x);
		OffShoot offshoot = cs.activateOffShoot(cb);
		ret = CBdoubleHelper.narrow(offshoot);

		return ret;
	}


	/**
	 * 
	 * @param cs
	 * @param x
	 * @return
	 * @throws AcsJContainerServicesEx
	 */
	static public CBdoubleSeq giveCBDoubleSequence (ContainerServicesBase cs, ResponseReceiver<Double[]> x) throws AcsJContainerServicesEx {
		CBdoubleSeq ret;

		Objects.CBdoubleSeqImpl cb = new Objects.CBdoubleSeqImpl(x);
		OffShoot offshoot = cs.activateOffShoot(cb);
		ret = CBdoubleSeqHelper.narrow(offshoot);

		return ret;
	}
	
	
	/**
	 * @return
	 */
	public static CBDescIn giveDescIn () {
		return new CBDescIn();
	}

}

//
//
//
//
//
//
//
//
//
//
//
//