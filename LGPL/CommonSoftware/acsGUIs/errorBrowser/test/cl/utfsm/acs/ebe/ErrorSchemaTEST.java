package cl.utfsm.acs.ebe;

import junit.framework.TestCase;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;

import cl.utfsm.acs.xml.*;
import cl.utfsm.acs.types.*;
import cl.utfsm.acs.ebe.*;

public class ErrorSchemaTEST extends TestCase {
	ErrorSchema test;
	protected void setUp() throws Exception {
		test=new ErrorSchema();
	}
	public void testTypeSchema(){
		AcsComplexType t=test.getTypeSchema();
		assertEquals("Wrong value, ",t.namespace,"acserr");
		assertEquals("Wrong value, ",t.name,"Type");
	}
	public void testTypeError(){
		AcsComplexType t=test.getErrorSchema();
		assertEquals("Wrong value, ",t.namespace,"acserr");
		assertEquals("Wrong value, ",t.name,"ErrorCode");
	}
	public void testTypeCompletion(){
		AcsComplexType t=test.getCompletionSchema();
		assertEquals("Wrong value, ",t.namespace,"acserr");
		assertEquals("Wrong value, ",t.name,"Code");
	}
	
						
}
