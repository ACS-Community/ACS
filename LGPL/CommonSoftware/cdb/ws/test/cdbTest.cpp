#include <vltPort.h>

#include <cdbDALaccess.h>
#include <logging.h>
#include <ace/ARGV.h>

#ifdef MAKE_VXWORKS
#	include "rebootLib.h"
#	include "acsutilArgUnpack.h"
//******************************************************************************
//*    ALMA - Atacama Large Millimiter Array
//*    (c) European Southern Observatory, 2002
//*    Copyright by ESO (in the framework of the ALMA collaboration)
//*    and Cosylab 2002, All rights reserved
//*
//*    This library is free software; you can redistribute it and/or
//*    modify it under the terms of the GNU Lesser General Public
//*    License as published by the Free Software Foundation; either
//*    version 2.1 of the License, or (at your option) any later version.
//*
//*    This library is distributed in the hope that it will be useful,
//*    but WITHOUT ANY WARRANTY; without even the implied warranty of
//*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//*    Lesser General Public License for more details.
//*
//*    You should have received a copy of the GNU Lesser General Public
//*    License along with this library; if not, write to the Free Software
//*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
//*

int startCdbTest (char *szCmdLn)
{
    int  argc;
    char *argv[100];

    argc = argUnpack(szCmdLn, argv);
    argv[0] = "cdbTest";
#else
int main(int argc, char* argv[]) 
{
#endif // defined( MAKE_VXWORKS )

  
try{
    CORBA::ULong ul;
    cdb::Field dbFld;
    cdb::Table* dataBase;
    ACE_CString curl, field;
    ACE_CString strCmdLine;
    ACE_CString strField;
    LoggingProxy logger(0, 0, 31);
    
    // initialize the rest of LoggingProxy
    LoggingProxy::ProcessName(argv[0]);
    LoggingProxy::ThreadName("main");
    
    LoggingProxy::init (&logger); 
    
    ACS_SHORT_LOG ((LM_INFO, "cdbTestClient starts"));
    
    dataBase = cdb::getDatabase( argc, argv );

    curl = "MACI/Managers/Manager";
    
    // something for sure
    field = "Timeout";
    if (dataBase->GetField(curl, field, dbFld))
	{
	dbFld.GetString(strCmdLine);
        ACS_SHORT_LOG((LM_INFO, "Curl %s field %s OK", curl.c_str(), field.c_str() ));
        dbFld.ToString(strField, true);
        ACS_SHORT_LOG((LM_INFO, "ToString: %s", strField.c_str()));
        ACE_CString strFrom = "<String>maci::test";
        ACS_SHORT_LOG((LM_INFO, "FromString output : %d", dbFld.FromString(strFrom)));
	}
    else
	ACS_SHORT_LOG((LM_ERROR, "Curl %s does not have %s field", curl.c_str(), field.c_str() ));
    
    // litle bit conversion
    field = "ClientPingInterval";
    if (dataBase->GetField(curl, field, dbFld))
	{
	if( dbFld.GetULong(ul) ) 
	    {
	    ACS_SHORT_LOG((LM_INFO, "Curl %s field %s OK", curl.c_str(), field.c_str() ));
	    }
	else 
	    {
	    ACS_SHORT_LOG((LM_ERROR, "Curl %s : invalid value in %s field", curl.c_str(), field.c_str() ));
	    }
	}
    else
	ACS_SHORT_LOG((LM_ERROR, "Curl %s does not have %s field", curl.c_str(), field.c_str() ));
    
    // try something ilegal
    field = "StrangeFieldName";
    if (dataBase->GetField(curl, field, dbFld))
	{
	// will hardly get here
	}
    else
	ACS_SHORT_LOG((LM_ERROR, "Curl %s does not have %s field", curl.c_str(), field.c_str() ));
    
    // Container
    curl = "MACI/Containers/Container";
    
    // Container should have
    field = "UseIFR";
    if (dataBase->GetField(curl, field, dbFld))
	{
	dbFld.GetString(strCmdLine);
	ACS_SHORT_LOG((LM_INFO, "Curl %s field %s OK", curl.c_str(), field.c_str() ));
	}
    else
	ACS_SHORT_LOG((LM_ERROR, "Curl %s does not have %s field", curl.c_str(), field.c_str() ));
    

    ACS_SHORT_LOG((LM_INFO, "The test will create database with remote DAOs"));

    // this is done by local parser now try it with remote DAO
    ACE_ARGV myArgs;
    myArgs.add (argv);
    myArgs.add("-DAOremote");
    
    destroyDatabase(dataBase);
    dataBase = cdb::getDatabase( myArgs.argc(), myArgs.argv(), CORBA::ORB::_nil(), NULL, 1 );
    //dataBase = getDatabase( myArgs.argc(), myArgs.argv() );    

    // PS
    curl = "alma/TEST_PS_1";
    
    field = "current/graph_max";
    if (dataBase->GetField(curl, field, dbFld))
	{
	if( dbFld.GetULong(ul) ) 
	    {
	    ACS_SHORT_LOG((LM_INFO, "Curl %s field %s OK", curl.c_str(), field.c_str() ));
	    }
	else 
	    {
	    ACS_SHORT_LOG((LM_ERROR, "Curl %s : invalid value in %s field", curl.c_str(), field.c_str() ));
	    }
	}
    else
	ACS_SHORT_LOG((LM_ERROR, "Curl %s does not have %s field", curl.c_str(), field.c_str() ));

    // make an exception on remote DAO

    field = "StrangeFieldName";
    if (dataBase->GetField(curl, field, dbFld))
	{
	// will hardly get here
	}
    else
	ACS_SHORT_LOG((LM_ERROR, "Curl %s does not have %s field", curl.c_str(), field.c_str() ));
		
    ACS_SHORT_LOG((LM_INFO, "Test arrays"));
    // array
    curl = curl + "/status";
    field = "bitDescription";
    if (!dataBase->GetField(curl, field, dbFld)) 
	{
	ACS_SHORT_LOG((LM_ERROR, "Curl %s does not have %s field", curl.c_str(), field.c_str() ));
	}
    else 
	{
	int nElements = 0;
	cdb::StringArray * strarray = dbFld.GetStringArray();
	cdb::StringArray::const_iterator iter = strarray->begin();
	for (; iter != strarray->end(); iter++)
	    {
	    ACS_SHORT_LOG((LM_ERROR, "Curl %s in %s field values: %s", curl.c_str(), field.c_str(), iter->c_str() ));
	    nElements++;
	    }
	if( nElements == 9 ) 
	    {
	    ACS_SHORT_LOG((LM_INFO, "Test arrays OK"));
	    }
	else 
	    {
	    ACS_SHORT_LOG((LM_INFO, "Test arrays FAILED"));
	    }
	}

    destroyDatabase(dataBase);
    dataBase = 0;

    // test database free by singleton
/*    for( int i=0; i<100; i++ ) 
	{
	dataBase = getDatabase();
	}
*/
    ACE_OS::sleep(1);

  
    ACS_SHORT_LOG ((LM_INFO, "cdbTestClient ends"));
    
    }
catch(CORBA::Exception &ex)
    {
    ACE_PRINT_EXCEPTION(ACE_ANY_EXCEPTION, "main");
    }
  
  
return 0;
}



