/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: VxWorksDLL.cpp,v 1.79 2003/05/06 13:44:17 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bgustafs 2002-01-15 added function vxworks_modulefind
* bgustafs 2001-06-26 added header, removed warnings for unused arguments
*/

#include <ace/OS.h>
#include <sysLib.h>
#include <rebootLib.h>
//#include <loadLib.h>
#include <moduleLib.h>
#include <usrLib.h>
#include <unldLib.h>
#include <symLib.h>
#include <sysSymTbl.h>


ACE_SHLIB_HANDLE vxworks_dlopen(const char *szDLL)
{
    return ld(0, 0, const_cast<char*>(szDLL));
}

int vxworks_dlclose(ACE_SHLIB_HANDLE lib)
{
    return unldByModuleId((MODULE_ID)lib, 0); 
}

struct SymbolIteratorStruct
{
    char      *szName;
    MODULE_ID lib;
    void      *ptr;
};

BOOL SymbolIterator(char *name, int val, SYM_TYPE type, int arg, UINT16 group)
{   
  ACE_UNUSED_ARG(type);

    SymbolIteratorStruct *sis = (SymbolIteratorStruct*)arg;
    
    if(sis->szName == 0 || sis->lib == 0) return 0;

    if(sis->lib->group == group &&
       strcmp(name, sis->szName) == 0)
    {
        sis->ptr = (void*)val;
        return 0;
    }

    return 1;
}

void* vxworks_dlsym(ACE_SHLIB_HANDLE lib, char *szName)
{
   SymbolIteratorStruct sis;

   sis.ptr = 0;
   sis.szName = szName;
   sis.lib = (MODULE_ID)lib; 
 
   symEach(sysSymTbl, (FUNCPTR)SymbolIterator, (int)&sis);

   return sis.ptr; 
}

ACE_SHLIB_HANDLE vxworks_modulefind(char *name)
{
    return moduleFindByName(name);
}
