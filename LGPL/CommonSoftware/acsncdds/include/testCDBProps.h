#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#ifndef _CDB_PROPS_H_
#define _CDB_PROPS_H

#include <acscomponentImpl.h>

#include "CDBPropsS.h"

class CDBPropsCompImpl:
	virtual public POA_CDB_PROPS::cdbProps, 
	virtual public acscomponent::ACSComponentImpl
{
	public:
		CDBPropsCompImpl(const ACE_CString &name, 
				maci::ContainerServices *containerServices);

		virtual CORBA::Long runTest(); 

		virtual ~CDBPropsCompImpl();
};

#endif
