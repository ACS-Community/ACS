/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2004 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: parameterTest.cpp,v 1.3 2006/11/29 23:01:27 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring 2005-03-24 created
*/

#include <ParamSetDef.h>
#include <ParameterSet.h>
#include <string>

using std::string;
using std::cout;
using std::endl;
using namespace Parameters;

static const string BOOL_PARAM_NAME = "overwrite";
static const string INT_PARAM_NAME = "testInt";
static const string DOUBLE_PARAM_NAME = "centerfreq";
static const string STRING_PARAM_NAME = "bandname";
static const string INT_ARRAY_PARAM_NAME = "testIntArray";
static const string DOUBLE_ARRAY_PARAM_NAME = "testDoubleArray";
static const string STRING_ARRAY_PARAM_NAME = "testStringArray";

void exitUsage()
{
	cout << endl << "Usage: " << endl;
	cout << "parameterTest -file <name of xml file containing parameter set instance data> " << endl;
	exit(1);
}

int main(int argc, char *argv[])
{
	if(3 != argc) {
		exitUsage();
	}
	string xmlFileName;
	if(NULL == argv[1] || NULL == argv[2]) {
		exitUsage();
	}
	string currArg(argv[1]);
	if (currArg == "-file") {
		xmlFileName = argv[2];
	}
	else {
		exitUsage();
	}
	ParameterSet pset(xmlFileName);

	try 
	{
		// test string param
		StringParam testStringVal = pset.getStringParam(STRING_PARAM_NAME);
		cout << STRING_PARAM_NAME << " was: " << testStringVal.getValue() << endl; 
		StringParam strP(STRING_PARAM_NAME, "newstring");
		pset.setParam(STRING_PARAM_NAME, strP);
		testStringVal = pset.getStringParam(STRING_PARAM_NAME);
		cout << STRING_PARAM_NAME << " now: " << testStringVal.getValue() << endl << endl; 

		// test double param
		auto_ptr<string> units;
		DoubleParam testDoubleVal = pset.getDoubleParam(DOUBLE_PARAM_NAME);
		cout << DOUBLE_PARAM_NAME << " was: " << testDoubleVal.getValue() << endl; 
		DoubleParam dP(5.5, DOUBLE_PARAM_NAME, units);
		pset.setParam(DOUBLE_PARAM_NAME, dP);
		testDoubleVal = pset.getDoubleParam(DOUBLE_PARAM_NAME);
		cout << DOUBLE_PARAM_NAME << " now: " << testDoubleVal.getValue() << endl << endl; 

		// test int param
		IntParam testIntVal = pset.getIntParam(INT_PARAM_NAME);
		cout << INT_PARAM_NAME << " was: " << testIntVal.getValue() << endl; 
		IntParam iP(11, INT_PARAM_NAME, units);
		pset.setParam(INT_PARAM_NAME, iP);
		testIntVal = pset.getIntParam(INT_PARAM_NAME);
		cout << INT_PARAM_NAME << " now: " << testIntVal.getValue() << endl << endl; 

		// test bool param
		BoolParam testBoolVal = pset.getBoolParam(BOOL_PARAM_NAME);
		cout << BOOL_PARAM_NAME << " was: " << testBoolVal.getValue() << endl; 
		BoolParam bP(false, BOOL_PARAM_NAME);
		pset.setParam(BOOL_PARAM_NAME, bP);
		testBoolVal = pset.getBoolParam(BOOL_PARAM_NAME);
		cout << BOOL_PARAM_NAME << " now: " << testBoolVal.getValue() << endl << endl; 

		// test stringarray param
		StringArrayParam testStringArray = pset.getStringArrayParam(STRING_ARRAY_PARAM_NAME);
		cout << STRING_ARRAY_PARAM_NAME << " was: " << endl; 
		for(unsigned int i = 0; i < testStringArray.getValues().size(); ++i)
		{
			cout << "string[" << i << "]: " << testStringArray.getValues().begin()[i] << endl;
		}
		cout << endl;

		vector<string> strData;
		strData.push_back("newstr1");
		strData.push_back("newstr2");
		StringArrayParam newStringArrayParam(strData, STRING_ARRAY_PARAM_NAME);
		pset.setParam(STRING_ARRAY_PARAM_NAME, newStringArrayParam);
		testStringArray = pset.getStringArrayParam(STRING_ARRAY_PARAM_NAME);
		cout << STRING_ARRAY_PARAM_NAME << " now: " << endl;
		for(unsigned int i = 0; i < testStringArray.getValues().size(); ++i)
		{
			cout << "string[" << i << "]: " << testStringArray.getValues().begin()[i] << endl;
		}
		cout << endl;

		// test doublearray param
		DoubleArrayParam testDoubleArray = pset.getDoubleArrayParam(DOUBLE_ARRAY_PARAM_NAME);
		cout << DOUBLE_ARRAY_PARAM_NAME << " was: " << endl; 
		for(unsigned int i = 0; i < testDoubleArray.getValues().size(); ++i)
		{
			cout << "double[" << i << "]: " << testDoubleArray.getValues().begin()[i] << endl;
		}
		cout << endl;

		vector<double> dblData;
		dblData.push_back(5.5);
		dblData.push_back(6.6);
		DoubleArrayParam newDoubleArrayParam(dblData, DOUBLE_ARRAY_PARAM_NAME, units);
		pset.setParam(DOUBLE_ARRAY_PARAM_NAME, newDoubleArrayParam);
		testDoubleArray = pset.getDoubleArrayParam(DOUBLE_ARRAY_PARAM_NAME);
		cout << DOUBLE_ARRAY_PARAM_NAME << " now: " << endl;
		for(unsigned int i = 0; i < testDoubleArray.getValues().size(); ++i)
		{
			cout << "double[" << i << "]: " << testDoubleArray.getValues().begin()[i] << endl;
		}
		cout << endl;

		// test intarray param
		IntArrayParam testIntArray = pset.getIntArrayParam(INT_ARRAY_PARAM_NAME);
		cout << INT_ARRAY_PARAM_NAME << " was: " << endl; 
		for(unsigned int i = 0; i < testIntArray.getValues().size(); ++i)
		{
			cout << "int[" << i << "]: " << testIntArray.getValues().begin()[i] << endl;
		}
		cout << endl;

		vector<int> junkData;
		junkData.push_back(5);
		junkData.push_back(6);
		IntArrayParam newIntArrayParam(junkData, INT_ARRAY_PARAM_NAME, units);
		pset.setParam(INT_ARRAY_PARAM_NAME, newIntArrayParam);
		testIntArray = pset.getIntArrayParam(INT_ARRAY_PARAM_NAME);
		cout << INT_ARRAY_PARAM_NAME << " now: " << endl;
		for(unsigned int i = 0; i < testIntArray.getValues().size(); ++i)
		{
			cout << "int[" << i << "]: " << testIntArray.getValues().begin()[i] << endl;
		}

		cout << endl<< "toString:" << endl << endl << pset.toString() << endl;
		cout << endl;
		cout.flush();
	}
	catch(domain_error exObj) 
	{
		cout << "Exception caught!" <<  endl << exObj.what() << endl << endl; 
		cout << endl;
		cout.flush();
	}
}
