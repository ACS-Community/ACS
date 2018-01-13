package tmcdb.test.properties;

import alma.ACS.ROboolean;
import alma.ACS.RObooleanHelper;
import alma.ACS.RObooleanPOATie;
import alma.ACS.RObooleanSeq;
import alma.ACS.RObooleanSeqHelper;
import alma.ACS.RObooleanSeqPOATie;
import alma.ACS.ROdouble;
import alma.ACS.ROdoubleHelper;
import alma.ACS.ROdoublePOATie;
import alma.ACS.ROdoubleSeq;
import alma.ACS.ROdoubleSeqHelper;
import alma.ACS.ROdoubleSeqPOATie;
import alma.ACS.ROfloat;
import alma.ACS.ROfloatHelper;
import alma.ACS.ROfloatPOATie;
import alma.ACS.ROfloatSeq;
import alma.ACS.ROfloatSeqHelper;
import alma.ACS.ROfloatSeqPOATie;
import alma.ACS.ROlong;
import alma.ACS.ROlongHelper;
import alma.ACS.ROlongLong;
import alma.ACS.ROlongLongHelper;
import alma.ACS.ROlongLongPOATie;
import alma.ACS.ROlongPOATie;
import alma.ACS.ROlongSeq;
import alma.ACS.ROlongSeqHelper;
import alma.ACS.ROlongSeqPOATie;
import alma.ACS.ROpattern;
import alma.ACS.ROpatternHelper;
import alma.ACS.ROpatternPOATie;
import alma.ACS.ROstring;
import alma.ACS.ROstringHelper;
import alma.ACS.ROstringPOATie;
import alma.ACS.ROuLong;
import alma.ACS.ROuLongHelper;
import alma.ACS.ROuLongLong;
import alma.ACS.ROuLongLongHelper;
import alma.ACS.ROuLongLongPOATie;
import alma.ACS.ROuLongPOATie;
import alma.ACS.ROuLongSeq;
import alma.ACS.ROuLongSeqHelper;
import alma.ACS.ROuLongSeqPOATie;
import alma.ACS.RWboolean;
import alma.ACS.RWbooleanHelper;
import alma.ACS.RWbooleanPOATie;
import alma.ACS.RWbooleanSeq;
import alma.ACS.RWbooleanSeqHelper;
import alma.ACS.RWbooleanSeqPOATie;
import alma.ACS.RWdouble;
import alma.ACS.RWdoubleHelper;
import alma.ACS.RWdoublePOATie;
import alma.ACS.RWdoubleSeq;
import alma.ACS.RWdoubleSeqHelper;
import alma.ACS.RWdoubleSeqPOATie;
import alma.ACS.RWfloat;
import alma.ACS.RWfloatHelper;
import alma.ACS.RWfloatPOATie;
import alma.ACS.RWfloatSeq;
import alma.ACS.RWfloatSeqHelper;
import alma.ACS.RWfloatSeqPOATie;
import alma.ACS.RWlong;
import alma.ACS.RWlongHelper;
import alma.ACS.RWlongLong;
import alma.ACS.RWlongLongHelper;
import alma.ACS.RWlongLongPOATie;
import alma.ACS.RWlongPOATie;
import alma.ACS.RWlongSeq;
import alma.ACS.RWlongSeqHelper;
import alma.ACS.RWlongSeqPOATie;
import alma.ACS.RWpattern;
import alma.ACS.RWpatternHelper;
import alma.ACS.RWpatternPOATie;
import alma.ACS.RWstring;
import alma.ACS.RWstringHelper;
import alma.ACS.RWstringPOATie;
import alma.ACS.RWuLong;
import alma.ACS.RWuLongHelper;
import alma.ACS.RWuLongLong;
import alma.ACS.RWuLongLongHelper;
import alma.ACS.RWuLongLongPOATie;
import alma.ACS.RWuLongPOATie;
import alma.ACS.RWuLongSeq;
import alma.ACS.RWuLongSeqHelper;
import alma.ACS.RWuLongSeqPOATie;
import alma.ACS.impl.CharacteristicComponentImpl;
import alma.ACS.impl.CommonROEnumPropertyImpl;
import alma.ACS.impl.CommonRWEnumPropertyImpl;
import alma.ACS.impl.RObooleanImpl;
import alma.ACS.impl.RObooleanSeqImpl;
import alma.ACS.impl.ROdoubleImpl;
import alma.ACS.impl.ROdoubleSeqImpl;
import alma.ACS.impl.ROfloatImpl;
import alma.ACS.impl.ROfloatSeqImpl;
import alma.ACS.impl.ROlongImpl;
import alma.ACS.impl.ROlongLongImpl;
import alma.ACS.impl.ROlongSeqImpl;
import alma.ACS.impl.ROpatternImpl;
import alma.ACS.impl.ROstringImpl;
import alma.ACS.impl.ROuLongLongImpl;
import alma.ACS.impl.ROulongImpl;
import alma.ACS.impl.ROulongSeqImpl;
import alma.ACS.impl.RWbooleanImpl;
import alma.ACS.impl.RWbooleanSeqImpl;
import alma.ACS.impl.RWdoubleImpl;
import alma.ACS.impl.RWdoubleSeqImpl;
import alma.ACS.impl.RWfloatImpl;
import alma.ACS.impl.RWfloatSeqImpl;
import alma.ACS.impl.RWlongImpl;
import alma.ACS.impl.RWlongLongImpl;
import alma.ACS.impl.RWlongSeqImpl;
import alma.ACS.impl.RWpatternImpl;
import alma.ACS.impl.RWstringImpl;
import alma.ACS.impl.RWuLongImpl;
import alma.ACS.impl.RWuLongLongImpl;
import alma.ACS.impl.RWuLongSeqImpl;
import alma.ACS.jbaci.DataAccess;
import alma.TMCDB.EnumTest;
import alma.TMCDB.MCtestPropertiesComponentOperations;
import alma.TMCDB.ROEnumTest;
import alma.TMCDB.ROEnumTestHelper;
import alma.TMCDB.ROEnumTestOperations;
import alma.TMCDB.ROEnumTestPOATie;
import alma.TMCDB.RWEnumTest;
import alma.TMCDB.RWEnumTestHelper;
import alma.TMCDB.RWEnumTestOperations;
import alma.TMCDB.RWEnumTestPOATie;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;

public class MCtestPropertiesComponentImpl extends CharacteristicComponentImpl
		implements MCtestPropertiesComponentOperations {

	//RO
	private ROdouble m_doubleROProp_p;
	private double m_doubleROVal = 1.0;
	private long m_time1 = 134608945243381570L;
	private DataAccess<Double> m_doubleRODevIO = new MCtestDevIONoIncremental<Double>(m_time1, m_doubleROVal);
	
	private ROfloat m_floatROProp_p;
	private float m_floatROVal = 2.0F;
	private long m_time2 = 134608945243381570L;
	private DataAccess<Float> m_floatRODevIO = new MCtestDevIONoIncremental<Float>(m_time2, m_floatROVal);
	
	private ROlong m_longROProp_p;
	private int m_longROVal = 3;
	private long m_time3 = 134608945243381570L;
	private DataAccess<Integer> m_longRODevIO = new MCtestDevIONoIncremental<Integer>(m_time3, m_longROVal);
	
	private ROuLong m_uLongROProp_p;
	private int m_uLongROVal = 4;
	private long m_time4 = 134608945243381570L;
	private DataAccess<Integer> m_uLongRODevIO = new MCtestDevIONoIncremental<Integer>(m_time4, m_uLongROVal);
	
	private ROpattern m_patternROProp_p;
	private long m_patternROVal = 5;
	private long m_time5 = 134608945243381570L;
	private DataAccess<Long> m_patternRODevIO = new MCtestDevIONoIncremental<Long>(m_time5, m_patternROVal);
	
	private ROstring m_stringROProp_p;
	private String m_stringROVal = "strinRO";
	private long m_time6 = 134608945243381570L;
	private DataAccess<String> m_stringRODevIO = new MCtestDevIONoIncremental<String>(m_time6, m_stringROVal);
	
	private ROlongLong m_longLongROProp_p;
	private long m_longLongROVal = 6;
	private long m_time7 = 134608945243381570L;
	private DataAccess<Long> m_longLongRODevIO = new MCtestDevIONoIncremental<Long>(m_time7, m_longLongROVal);
	
	private ROuLongLong m_uLongLongROProp_p;
	private long m_uLongLongROVal = 7;
	private long m_time8 = 134608945243381570L;
	private DataAccess<Long> m_ulongLongRODevIO = new MCtestDevIONoIncremental<Long>(m_time8, m_uLongLongROVal);
	
	private ROboolean m_booleanROProp_p;
	private boolean m_booleanROVal = false;
	private long m_time9 = 134608945243381570L;
	private DataAccess<Boolean> m_booleanRODevIo = new MCtestDevIONoIncremental<Boolean>(m_time9, m_booleanROVal);
	
	private ROdoubleSeq m_doubleSeqROProp_p;
	private double[] m_doubleSeqROVal = new double[] {8.0, 9.0};
	private long m_time10 = 134608945243381570L;
	private DataAccess<double[]> m_doubleSeqRODevIO = new MCtestDevIONoIncremental<double[]>(m_time10, m_doubleSeqROVal);
	
	private ROfloatSeq m_floatSeqROProp_p;
	private float[] m_floatSeqROVal  = new float[] {10.0F, 11.0F};
	private long m_time11 = 134608945243381570L;
	private DataAccess<float[]> m_floatSeqRODevIO = new MCtestDevIONoIncremental<float[]>(m_time11, m_floatSeqROVal);
	
	private ROlongSeq m_longSeqROProp_p;
	private int[] m_longSeqROVal = new int[] {12, 13};
	private long m_time12 = 134608945243381570L;
	private DataAccess<int[]> m_longSeqRODevIO = new MCtestDevIONoIncremental<int[]>(m_time12, m_longSeqROVal);
	
	private ROuLongSeq m_uLongSeqROProp_p;
	private int[] m_uLongSeqROVal = new int[] {14, 15};
	private long m_time13 = 134608945243381570L;
	private DataAccess<int[]> m_ulongSeqRODevIO = new MCtestDevIONoIncremental<int[]>(m_time13, m_uLongSeqROVal);
	
	private RObooleanSeq m_booleanSeqROProp_p;
	private boolean[] m_booleanSeqROVal = new boolean[]{false, true};
	private long m_time14 = 134608945243381570L;
	private DataAccess<boolean[]> m_booleanSeqRODevIO = new MCtestDevIONoIncremental<boolean[]>(m_time14, m_booleanSeqROVal);
	
	private ROEnumTest m_EnumTestROProp_p;
	private EnumTest m_EnumTestROVal = EnumTest.STATE1;
	private long m_time15 = 134608945243381570L;
	private DataAccess<EnumTest> m_EnumTestRODevIO = new MCtestDevIONoIncremental<EnumTest>(m_time15, m_EnumTestROVal);
	
	//RW
	private RWdouble m_doubleRWProp_p;
	private double m_doubleRWVal = 16D;
	private long m_time16 = 134608945243381570L;
	private DataAccess<Double> m_doubleRWDevIO =  new MCtestDevIONoIncremental<Double>(m_time16, m_doubleRWVal);
	
	private RWfloat m_floatRWProp_p;
	private float m_floatRWVal = 17F;
	private long m_time17 = 134608945243381570L;
	private DataAccess<Float> m_floatRWDevIO = new MCtestDevIONoIncremental<Float>(m_time17, m_floatRWVal);
	
	private RWlong m_longRWProp_p;
	private int m_longRWVal = 18;
	private long m_time18 = 134608945243381570L;
	private DataAccess<Integer> m_longRWDevIO = new MCtestDevIONoIncremental<Integer>(m_time18, m_longRWVal);
	
	private RWuLong m_uLongRWProp_p;
	private int m_uLongRWVal = 19;
	private long m_time19 = 134608945243381570L;
	private DataAccess<Integer> m_uLongRWDevIO = new MCtestDevIONoIncremental<Integer>(m_time19, m_uLongRWVal);
	
	private RWpattern m_patternRWProp_p;
	private long m_patternRWVal = 20;
	private long m_time20 = 134608945243381570L;
	private DataAccess<Long> m_patternRWDevIO = new MCtestDevIONoIncremental<Long>(m_time20, m_patternRWVal);
	
	private RWstring m_stringRWProp_p;
	private String m_stringRWVal = "stringRW";
	private long m_time21 = 134608945243381570L;
	private DataAccess<String> m_stringRWDevIO = new MCtestDevIONoIncremental<String>(m_time21, m_stringRWVal);
	
	private RWlongLong m_longLongRWProp_p;
	private long m_longLongRWVal = 21;
	private long m_time22 = 134608945243381570L;
	private DataAccess<Long> m_longLongRWDevIO = new MCtestDevIONoIncremental<Long>(m_time22, m_longLongRWVal);
	
	private RWuLongLong m_uLongLongRWProp_p;
	private long m_uLongLongRWVal = 22;
	private long m_time23 = 134608945243381570L;
	private DataAccess<Long> m_uLongLongRWDevIO = new MCtestDevIONoIncremental<Long>(m_time23, m_uLongLongRWVal);
	
	private RWboolean m_booleanRWProp_p;
	private boolean m_booleanRWVal = true;
	private long m_time24 = 134608945243381570L;
	private DataAccess<Boolean> m_booleanRWDevIO = new MCtestDevIONoIncremental<Boolean>(m_time24, m_booleanRWVal);
	
	private RWdoubleSeq m_doubleSeqRWProp_p;
	private double[] m_doubleSeqRWVal = new double[]{23D, 24D};
	private long m_time25 = 134608945243381570L;
	private DataAccess<double[]> m_doubleSeqRWDevIO = new MCtestDevIONoIncremental<double[]>(m_time25, m_doubleSeqRWVal);
	
	private RWfloatSeq m_floatSeqRWProp_p;
	private float[] m_floatSeqRWVal = new float[] {25F, 26F};
	private long m_time26 = 134608945243381570L;
	private DataAccess<float[]> m_floatSeqRWDevIO = new MCtestDevIONoIncremental<float[]>(m_time26, m_floatSeqRWVal);
	
	private RWlongSeq m_longSeqRWProp_p;
	private int[] m_longSeqRWVal = new int[]{27 ,28};
	private long m_time27 = 134608945243381570L;
	private DataAccess<int[]> m_longSeqRWDevIO = new MCtestDevIONoIncremental<int[]>(m_time27, m_longSeqRWVal);
	
	private RWuLongSeq m_uLongSeqRWProp_p;
	private int[] m_uLongSeqRWVal = new int[]{29 ,30};
	private long m_time28 = 134608945243381570L;
	private DataAccess<int[]> m_uLongSeqRWDevIO = new MCtestDevIONoIncremental<int[]>(m_time28, m_uLongSeqRWVal);
	
	private RWbooleanSeq m_booleanSeqRWProp_p;
	private boolean[] m_booleanSeqRWVal = new boolean[] {true, false};
	private long m_time29 = 134608945243381570L;
	private DataAccess<boolean[]> m_booleanSeqRWDevIO = new MCtestDevIONoIncremental<boolean[]>(m_time29, m_booleanSeqRWVal);
	
	private RWEnumTest m_EnumTestRWProp_p;
	private EnumTest m_EnumTestRWVal = EnumTest.STATE1;
	private long m_time30 = 134608945243381570L;
	private DataAccess<EnumTest> m_EnumTestRWDevIO = new MCtestDevIONoIncremental<EnumTest>(m_time30, m_EnumTestRWVal);
	
	@Override
	public void initialize(ContainerServices containerServices) throws ComponentLifecycleException {
		super.initialize(containerServices);
		{
			ROdoubleImpl impl = new ROdoubleImpl("doubleROProp", this, m_doubleRODevIO);
			ROdoublePOATie tie = new ROdoublePOATie(impl);
			m_doubleROProp_p = ROdoubleHelper.narrow(registerProperty(impl, tie));
		}
		{
			ROfloatImpl impl =  new ROfloatImpl("floatROProp", this, m_floatRODevIO);
			ROfloatPOATie tie = new ROfloatPOATie(impl);
			m_floatROProp_p =  ROfloatHelper.narrow(registerProperty(impl, tie));
		}
		{
			ROlongImpl impl = new ROlongImpl("longROProp", this, m_longRODevIO);
			ROlongPOATie tie = new ROlongPOATie(impl);
			m_longROProp_p = ROlongHelper.narrow(registerProperty(impl, tie));
		}
		{
			ROulongImpl impl = new ROulongImpl("uLongROProp", this, m_uLongRODevIO);
			ROuLongPOATie tie = new ROuLongPOATie(impl);
			m_uLongROProp_p = ROuLongHelper.narrow(registerProperty(impl, tie));
		}
		{
			RObooleanImpl impl = new RObooleanImpl("booleanROProp", this, m_booleanRODevIo);
			RObooleanPOATie tie = new RObooleanPOATie(impl);
			m_booleanROProp_p = RObooleanHelper.narrow(registerProperty(impl, tie));
		}
		{
			ROpatternImpl impl = new ROpatternImpl("patternROProp", this, m_patternRODevIO);
			ROpatternPOATie tie = new ROpatternPOATie(impl);
			m_patternROProp_p = ROpatternHelper.narrow(registerProperty(impl, tie));
		}
		{
			ROstringImpl impl = new ROstringImpl("stringROProp", this, m_stringRODevIO);
			ROstringPOATie tie = new ROstringPOATie(impl);
			m_stringROProp_p = ROstringHelper.narrow(registerProperty(impl, tie));
		}
		{
			ROlongLongImpl impl = new ROlongLongImpl("longLongROProp", this, m_longLongRODevIO);
			ROlongLongPOATie tie = new ROlongLongPOATie(impl);
			m_longLongROProp_p = ROlongLongHelper.narrow(registerProperty(impl, tie));
		}
		{
			ROuLongLongImpl impl = new ROuLongLongImpl("uLongLongROProp", this, m_ulongLongRODevIO);
			ROuLongLongPOATie tie = new ROuLongLongPOATie(impl);
			m_uLongLongROProp_p = ROuLongLongHelper.narrow(registerProperty(impl, tie));
		}
		{
			RObooleanImpl impl = new RObooleanImpl("booleanROProp", this, m_booleanRODevIo);
			RObooleanPOATie tie = new RObooleanPOATie(impl);
			m_booleanROProp_p = RObooleanHelper.narrow(registerProperty(impl, tie));
		}
		{
			ROdoubleSeqImpl impl = new ROdoubleSeqImpl("doubleSeqROProp", this, m_doubleSeqRODevIO);
			ROdoubleSeqPOATie tie = new ROdoubleSeqPOATie(impl);
			m_doubleSeqROProp_p = ROdoubleSeqHelper.narrow(registerProperty(impl, tie));
		}
		{
			ROfloatSeqImpl impl = new ROfloatSeqImpl("floatSeqROProp", this, m_floatSeqRODevIO);
			ROfloatSeqPOATie tie = new ROfloatSeqPOATie(impl);
			m_floatSeqROProp_p = ROfloatSeqHelper.narrow(registerProperty(impl, tie));
		}
		{
			ROlongSeqImpl impl = new ROlongSeqImpl("longSeqROProp", this, m_longSeqRODevIO);
			ROlongSeqPOATie tie = new ROlongSeqPOATie(impl);
			m_longSeqROProp_p = ROlongSeqHelper.narrow(registerProperty(impl, tie));
		}
		{
			ROulongSeqImpl impl = new ROulongSeqImpl("uLongSeqROProp", this, m_ulongSeqRODevIO);
			ROuLongSeqPOATie tie = new ROuLongSeqPOATie(impl);
			m_uLongSeqROProp_p = ROuLongSeqHelper.narrow(registerProperty(impl, tie));
		}
		{
			RObooleanSeqImpl impl = new RObooleanSeqImpl("booleanSeqROProp", this, m_booleanSeqRODevIO);
			RObooleanSeqPOATie tie = new RObooleanSeqPOATie(impl);
			m_booleanSeqROProp_p = RObooleanSeqHelper.narrow(registerProperty(impl, tie));
		}
		{
			ROEnumTestOperations impl = 
					(ROEnumTestOperations) CommonROEnumPropertyImpl.createEnumProperty(
					ROEnumTestOperations.class,
					EnumTest.class, 
					"EnumTestROProp",
					this,
					m_EnumTestRODevIO);
			ROEnumTestPOATie tie = new ROEnumTestPOATie(impl);
			m_EnumTestROProp_p = ROEnumTestHelper.narrow(registerProperty(impl, tie));
		}
		//RW
		{
			RWdoubleImpl impl = new RWdoubleImpl("doubleRWProp", this, m_doubleRWDevIO);
			RWdoublePOATie tie = new RWdoublePOATie(impl);
			m_doubleRWProp_p = RWdoubleHelper.narrow(registerProperty(impl, tie));
		}
		{
			RWfloatImpl impl =  new RWfloatImpl("floatRWProp", this, m_floatRWDevIO);
			RWfloatPOATie tie = new RWfloatPOATie(impl);
			m_floatRWProp_p =  RWfloatHelper.narrow(registerProperty(impl, tie));
		}
		{
			RWlongImpl impl = new RWlongImpl("longRWProp", this, m_longRWDevIO);
			RWlongPOATie tie = new RWlongPOATie(impl);
			m_longRWProp_p = RWlongHelper.narrow(registerProperty(impl, tie));
		}
		{
			RWuLongImpl impl = new RWuLongImpl("uLongRWProp", this, m_uLongRWDevIO);
			RWuLongPOATie tie = new RWuLongPOATie(impl);
			m_uLongRWProp_p = RWuLongHelper.narrow(registerProperty(impl, tie));
		}
		{
			RWbooleanImpl impl = new RWbooleanImpl("booleanRWProp", this, m_booleanRWDevIO);
			RWbooleanPOATie tie = new RWbooleanPOATie(impl);
			m_booleanRWProp_p = RWbooleanHelper.narrow(registerProperty(impl, tie));
		}
		{
			RWpatternImpl impl = new RWpatternImpl("patternRWProp", this, m_patternRWDevIO);
			RWpatternPOATie tie = new RWpatternPOATie(impl);
			m_patternRWProp_p = RWpatternHelper.narrow(registerProperty(impl, tie));
		}
		{
			RWstringImpl impl = new RWstringImpl("stringRWProp", this, m_stringRWDevIO);
			RWstringPOATie tie = new RWstringPOATie(impl);
			m_stringRWProp_p = RWstringHelper.narrow(registerProperty(impl, tie));
		}
		{
			RWlongLongImpl impl = new RWlongLongImpl("longLongRWProp", this, m_longLongRWDevIO);
			RWlongLongPOATie tie = new RWlongLongPOATie(impl);
			m_longLongRWProp_p = RWlongLongHelper.narrow(registerProperty(impl, tie));
		}
		{
			RWuLongLongImpl impl = new RWuLongLongImpl("uLongLongRWProp", this, m_uLongLongRWDevIO);
			RWuLongLongPOATie tie = new RWuLongLongPOATie(impl);
			m_uLongLongRWProp_p = RWuLongLongHelper.narrow(registerProperty(impl, tie));
		}
		{
			RWbooleanImpl impl = new RWbooleanImpl("booleanRWProp", this, m_booleanRWDevIO);
			RWbooleanPOATie tie = new RWbooleanPOATie(impl);
			m_booleanRWProp_p = RWbooleanHelper.narrow(registerProperty(impl, tie));
		}
		{
			RWdoubleSeqImpl impl = new RWdoubleSeqImpl("doubleSeqRWProp", this, m_doubleSeqRWDevIO);
			RWdoubleSeqPOATie tie = new RWdoubleSeqPOATie(impl);
			m_doubleSeqRWProp_p = RWdoubleSeqHelper.narrow(registerProperty(impl, tie));
		}
		{
			RWfloatSeqImpl impl = new RWfloatSeqImpl("floatSeqRWProp", this, m_floatSeqRWDevIO);
			RWfloatSeqPOATie tie = new RWfloatSeqPOATie(impl);
			m_floatSeqRWProp_p = RWfloatSeqHelper.narrow(registerProperty(impl, tie));
		}
		{
			RWlongSeqImpl impl = new RWlongSeqImpl("longSeqRWProp", this, m_longSeqRWDevIO);
			RWlongSeqPOATie tie = new RWlongSeqPOATie(impl);
			m_longSeqRWProp_p = RWlongSeqHelper.narrow(registerProperty(impl, tie));
		}
		{
			RWuLongSeqImpl impl = new RWuLongSeqImpl("uLongSeqRWProp", this, m_uLongSeqRWDevIO);
			RWuLongSeqPOATie tie = new RWuLongSeqPOATie(impl);
			m_uLongSeqRWProp_p = RWuLongSeqHelper.narrow(registerProperty(impl, tie));
		}
		{
			RWbooleanSeqImpl impl = new RWbooleanSeqImpl("booleanSeqRWProp", this, m_booleanSeqRWDevIO);
			RWbooleanSeqPOATie tie = new RWbooleanSeqPOATie(impl);
			m_booleanSeqRWProp_p = RWbooleanSeqHelper.narrow(registerProperty(impl, tie));
		}
		{
			RWEnumTestOperations impl = 
					(RWEnumTestOperations) CommonRWEnumPropertyImpl.createEnumProperty(
					RWEnumTestOperations.class,
					EnumTest.class, 
					"EnumTestRWProp",
					this,
					m_EnumTestRWDevIO);
			RWEnumTestPOATie tie = new RWEnumTestPOATie(impl);
			m_EnumTestRWProp_p = RWEnumTestHelper.narrow(registerProperty(impl, tie));
		}
	}

	@Override
	public ROdouble doubleROProp() {
		return m_doubleROProp_p;
	}

	@Override
	public ROfloat floatROProp() {
		return m_floatROProp_p;
	}

	@Override
	public ROlong longROProp() {
		return m_longROProp_p;
	}

	@Override
	public ROuLong uLongROProp() {
		return m_uLongROProp_p;
	}

	@Override
	public ROpattern patternROProp() {
		return m_patternROProp_p;
	}

	@Override
	public ROstring stringROProp() {
		return m_stringROProp_p;
	}

	@Override
	public ROlongLong longLongROProp() {
		return m_longLongROProp_p;
	}

	@Override
	public ROuLongLong uLongLongROProp() {
		return m_uLongLongROProp_p;
	}

	@Override
	public ROboolean booleanROProp() {
		return m_booleanROProp_p;
	}

	@Override
	public ROdoubleSeq doubleSeqROProp() {
		return m_doubleSeqROProp_p;
	}

	@Override
	public ROfloatSeq floatSeqROProp() {
		return m_floatSeqROProp_p;
	}

	@Override
	public ROlongSeq longSeqROProp() {
		return m_longSeqROProp_p;
	}

	@Override
	public ROuLongSeq uLongSeqROProp() {
		return m_uLongSeqROProp_p;
	}

	@Override
	public RObooleanSeq booleanSeqROProp() {
		return m_booleanSeqROProp_p;
	}

	@Override
	public ROEnumTest EnumTestROProp() {
		return m_EnumTestROProp_p;
	}

	@Override
	public RWdouble doubleRWProp() {
		return m_doubleRWProp_p;
	}

	@Override
	public RWfloat floatRWProp() {
		return m_floatRWProp_p;
	}

	@Override
	public RWlong longRWProp() {
		return m_longRWProp_p;
	}

	@Override
	public RWuLong uLongRWProp() {
		return m_uLongRWProp_p;
	}

	@Override
	public RWpattern patternRWProp() {
		return m_patternRWProp_p;
	}

	@Override
	public RWstring stringRWProp() {
		return m_stringRWProp_p;
	}

	@Override
	public RWlongLong longLongRWProp() {
		return m_longLongRWProp_p;
	}

	@Override
	public RWuLongLong uLongLongRWProp() {
		return m_uLongLongRWProp_p;
	}

	@Override
	public RWboolean booleanRWProp() {
		return m_booleanRWProp_p;
	}

	@Override
	public RWdoubleSeq doubleSeqRWProp() {
		return m_doubleSeqRWProp_p;
	}

	@Override
	public RWfloatSeq floatSeqRWProp() {
		return m_floatSeqRWProp_p;
	}

	@Override
	public RWlongSeq longSeqRWProp() {
		return m_longSeqRWProp_p;
	}

	@Override
	public RWuLongSeq uLongSeqRWProp() {
		return m_uLongSeqRWProp_p;
	}

	@Override
	public RWbooleanSeq booleanSeqRWProp() {
		return m_booleanSeqRWProp_p;
	}

	@Override
	public RWEnumTest EnumTestRWProp() {
		return m_EnumTestRWProp_p;
	}

}
