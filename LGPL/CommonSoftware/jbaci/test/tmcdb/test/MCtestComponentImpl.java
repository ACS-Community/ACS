package tmcdb.test;

import alma.ACS.ROdouble;
import alma.ACS.ROdoubleHelper;
import alma.ACS.ROdoublePOATie;
import alma.ACS.ROdoubleSeq;
import alma.ACS.ROdoubleSeqHelper;
import alma.ACS.ROdoubleSeqPOATie;
import alma.ACS.ROpattern;
import alma.ACS.ROpatternHelper;
import alma.ACS.ROpatternPOATie;
import alma.ACS.RWlong;
import alma.ACS.RWlongHelper;
import alma.ACS.RWlongPOATie;
import alma.ACS.RWlongSeq;
import alma.ACS.RWlongSeqHelper;
import alma.ACS.RWlongSeqPOATie;
import alma.ACS.impl.CharacteristicComponentImpl;
import alma.ACS.impl.ROdoubleImpl;
import alma.ACS.impl.ROdoubleSeqImpl;
import alma.ACS.impl.ROpatternImpl;
import alma.ACS.impl.RWlongImpl;
import alma.ACS.impl.RWlongSeqImpl;
import alma.ACS.jbaci.DataAccess;
import alma.TMCDB.MCtestComponentOperations;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;

public class MCtestComponentImpl extends CharacteristicComponentImpl implements
		MCtestComponentOperations {

	private ROdouble doubleProp;
	private ROdoubleImpl doublePropImpl;
	private RWlong longProp;
	private RWlongImpl longPropImpl;
	private RWlongSeq longSeqProp;
	private RWlongSeqImpl longSeqPropImpl;
	private ROdoubleSeq doubleSeqProp;
	private ROdoubleSeqImpl doubleSeqPropImpl;
	private ROpattern patternProp;
	private ROpatternImpl patternPropImpl;

	private static double   initValDouble  =  -2.0;
	private static int      initValLong    =    15;
	private static long     initValPattern = 0x23L;
	private static int[]    initValLongSeq()   {
		int[] a = new int[25];		
		for (int i = 0; i < a.length; i++) { a[i] = -2; }
		return new int[25];
	}
	private static double[] initValDoubleSeq() {
		double[] a = new double[25];
		for (int i = 0; i < a.length; i++) { a[i] = -2.0; }
		return a;
	}
	
	@Override
	public void initialize(ContainerServices containerServices)
			throws ComponentLifecycleException {
		super.initialize(containerServices);
		
		{
			DataAccess<Double> doubleROPropDA = new MCTestDataAccess<Double>(initValDouble, 134608945243381570L);
			doublePropImpl =  new ROdoubleImpl("doubleProp", this, doubleROPropDA);
			ROdoublePOATie currentTie = new ROdoublePOATie(doublePropImpl);
			doubleProp = ROdoubleHelper.narrow(this.registerProperty(doublePropImpl, currentTie));
		}
		{
			DataAccess<Integer> longROPropDA = new MCTestDataAccess<Integer>(initValLong, 134608945243381570L);
			longPropImpl =  new RWlongImpl("longProp", this, longROPropDA);
			RWlongPOATie currentTie = new RWlongPOATie(longPropImpl);
			longProp = RWlongHelper.narrow(this.registerProperty(longPropImpl, currentTie));
		}
		{
			DataAccess<int[]> longSeqROPropDA = new MCTestDataAccessSeq<int[]>(initValLongSeq(), 134608945243381570L);
			longSeqPropImpl =  new RWlongSeqImpl("longSeqProp", this, longSeqROPropDA);
			RWlongSeqPOATie currentTie = new RWlongSeqPOATie(longSeqPropImpl);
			longSeqProp = RWlongSeqHelper.narrow(this.registerProperty(longSeqPropImpl, currentTie));
		}
		{
			DataAccess<double[]> doubleSeqROPropDA = new MCTestDataAccessSeq<double[]>(initValDoubleSeq(), 134608945243381570L);
			doubleSeqPropImpl =  new ROdoubleSeqImpl("doubleSeqProp", this, doubleSeqROPropDA);
			ROdoubleSeqPOATie currentTie = new ROdoubleSeqPOATie(doubleSeqPropImpl);
			doubleSeqProp = ROdoubleSeqHelper.narrow(this.registerProperty(doubleSeqPropImpl, currentTie));
		}
		{
			DataAccess<Long> patternROPropDA = new MCTestDataAccess<Long>(initValPattern, 134608945243381570L);
			patternPropImpl =  new ROpatternImpl("patternProp", this, patternROPropDA);
			ROpatternPOATie currentTie = new ROpatternPOATie(patternPropImpl);
			patternProp = ROpatternHelper.narrow(this.registerProperty(patternPropImpl, currentTie));
		}
		
	}

	@Override
	public ROdoubleSeq doubleSeqProp() {
		return doubleSeqProp;
	}

	@Override
	public ROdouble doubleProp() {
		return doubleProp;
	}

	@Override
	public RWlongSeq longSeqProp() {
		return longSeqProp;
	}

	@Override
	public RWlong longProp() {
		return longProp;
	}

	@Override
	public ROpattern patternProp() {
		return patternProp;
	}

	@Override
	public void reset() {
		try {
			doublePropImpl.getDataAccess().set(initValDouble, null);
		} catch (AcsJException e) {
			e.printStackTrace();
		}
		try {
			longPropImpl.getDataAccess().set(initValLong, null);
		} catch (AcsJException e) {
			e.printStackTrace();
		}
		try {
			longSeqPropImpl.getDataAccess().set(initValLongSeq(), null);
		} catch (AcsJException e) {
			e.printStackTrace();
		}
		try {
			doubleSeqPropImpl.getDataAccess().set(initValDoubleSeq(), null);
		} catch (AcsJException e) {
			e.printStackTrace();
		}
		try {
			patternPropImpl.getDataAccess().set(initValPattern, null);
		} catch (AcsJException e) {
			e.printStackTrace();
		}
	}


}
