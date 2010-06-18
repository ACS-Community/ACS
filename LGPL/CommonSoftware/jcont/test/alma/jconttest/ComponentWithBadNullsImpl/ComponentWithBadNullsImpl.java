package alma.jconttest.ComponentWithBadNullsImpl;

import org.omg.CORBA.StringHolder;

import alma.acs.component.ComponentImplBase;
import alma.jconttest.ComponentWithBadNullsOperations;
import alma.jconttest.ComponentWithBadNullsPackage.Enum1;
import alma.jconttest.ComponentWithBadNullsPackage.Struct1;
import alma.jconttest.ComponentWithBadNullsPackage.Struct1Holder;
import alma.jconttest.ComponentWithBadNullsPackage.Struct2;

public class ComponentWithBadNullsImpl extends ComponentImplBase implements ComponentWithBadNullsOperations
{
	/**
	 * Creates a valid instance of the IDL-defined {@link Struct2}.
	 */
	public static Struct2 createGoodStruct2() {
		Struct1 goodStruct1 = createGoodStruct1();
		Struct1[] goodStruct1Array = new Struct1[1];
		goodStruct1Array[0] = goodStruct1;
		Struct2 goodStruct2 = new Struct2(goodStruct1, goodStruct1Array);
		return goodStruct2;
	}

	public static Struct1 createGoodStruct1() {
		Enum1 goodEnum1 = Enum1.MY_ONLY_VALUE;
		Struct1 goodStruct1 = new Struct1("goodstring", goodEnum1);
		return goodStruct1;
	}

	/**
	 * For testing, this method returns the 2 in parameters both as inout and out parameters.
	 */
	@Override
	public Struct1 methodWithReturnData(String instring, Struct1 instruct1, 
										StringHolder inoutstring, Struct1Holder inoutstruct1, 
										StringHolder outstring, Struct1Holder outstruct1) 
	{
		inoutstring.value = instring; //"in-and-out string";
		inoutstruct1.value = instruct1;
		outstring.value = instring; //"my mega out string";
		outstruct1.value = instruct1;
		return createGoodStruct1();
	}
}
