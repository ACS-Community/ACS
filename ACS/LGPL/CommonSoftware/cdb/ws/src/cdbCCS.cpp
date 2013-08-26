/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2011
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: cdbCCS.cpp,v 1.28 2011/10/28 15:05:05 hsommer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2011-10-28  created
*/

//
// CCS wrapper for the VLT standard database
//
// ************************************************************************

#include <vltPort.h>

// include only macros
#include <logging.h>

#include "ccs.h"
#include "db.h"
#include "string.h"
#include "cdbCCS.h"
#include <algorithm>
#ifndef MAKE_VXWORKS
#include "dbPrivate.h"
#else
#include "lccErrors.h"

#define dbUNDEFINED_SIZE 0

typedef struct
{
  dbTYPE               type;              /* data type                      */
  const char          *mnemonic;          /* mnemonic of data type          */
  vltUINT16            size;              /* size of data type              */
} dbTYPES;

static const dbTYPES   dbTypes[] = {
  {dbUNDEFINED,   "",              dbUNDEFINED_SIZE      },
  {dbLOGICAL,     "dbLOGICAL",     sizeof(vltLOGICAL)    },
  {dbINT8,        "dbINT8",        sizeof(vltINT8)       },
  {dbUINT8,       "dbUINT8",       sizeof(vltUINT8)      },
  {dbINT16,       "dbINT16",       sizeof(vltINT16)      },
  {dbUINT16,      "dbUINT16",      sizeof(vltUINT16)     },
  {dbINT32,       "dbINT32",       sizeof(vltINT32)      },
  {dbUINT32,      "dbUINT32",      sizeof(vltUINT32)     },
  {dbFLOAT,       "dbFLOAT",       sizeof(vltFLOAT)      },
  {dbDOUBLE,      "dbDOUBLE",      sizeof(vltDOUBLE)     },
  {dbPOLAR,       "dbPOLAR",       sizeof(vltPOLAR)      },
  {dbRECTANGULAR, "dbRECTANGULAR", sizeof(vltRECTANGULAR)},
  {dbUNDEFINED,   "",              dbUNDEFINED_SIZE      },
  {dbUNDEFINED,   "",              dbUNDEFINED_SIZE      },
  {dbUNDEFINED,   "",              dbUNDEFINED_SIZE      },
  {dbUNDEFINED,   "",              dbUNDEFINED_SIZE      },
  {dbBYTES4,      "dbBYTES4",      sizeof(vltBYTES4)     },
  {dbBYTES8,      "dbBYTES8",      sizeof(vltBYTES8)     },
  {dbBYTES12,     "dbBYTES12",     sizeof(vltBYTES12)    },
  {dbBYTES16,     "dbBYTES16",     sizeof(vltBYTES16)    },
  {dbBYTES20,     "dbBYTES20",     sizeof(vltBYTES20)    },
  {dbBYTES32,     "dbBYTES32",     sizeof(vltBYTES32)    },
  {dbBYTES48,     "dbBYTES48",     sizeof(vltBYTES48)    },
  {dbBYTES64,     "dbBYTES64",     sizeof(vltBYTES64)    },
  {dbBYTES80,     "dbBYTES80",     sizeof(vltBYTES80)    },
  {dbBYTES128,    "dbBYTES128",    sizeof(vltBYTES128)   },
  {dbBYTES256,    "dbBYTES256",    sizeof(vltBYTES256)   },
  {dbUNDEFINED,   "",              dbUNDEFINED_SIZE      },
  {dbXREF,        "dbXREF",        sizeof(vltDBXREF)     },
  {dbDATE,        "dbDATE",        sizeof(vltDATE)       },
  {dbTIME_OF_DAY, "dbTIME_OF_DAY", sizeof(vltTIME_OF_DAY)},
  {dbABS_TIME,    "dbABS_TIME",    sizeof(vltABS_TIME)   }
};
#endif
#include "err.h"
#include "log.h"

static char *rcsId="@(#) $Id: cdbCCS.cpp,v 1.28 2011/10/28 15:05:05 hsommer Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

namespace cdb {

Table* CCS::createTable( int argc, char** argv, CORBA::ORB_ptr orb)
{
	const char* pProcess = ""; 
	if( argc > 0 )
		pProcess = argv[0];
	
  return new CCS( pProcess );
}

  CCS::CCS(String process) : isInit(FALSE)
{
  ccsERROR error;
  
  if (ccsInit(process.c_str(),0,NULL,NULL,&error)==FAILURE) {
#ifndef MAKE_VXWORKS
        ACS_LOG (LM_RUNTIME_CONTEXT, "cdb::CCS::CCS",
         (LM_ERROR,  "Error %d occured trying to initialize database", error.errorNumber));
#else
    if (error.errorNumber == lccERR_IN_USE) {
      isInit=TRUE;
    }
    else {
      ACS_LOG (LM_RUNTIME_CONTEXT, "cdb::CCS::CCS",
             (LM_ERROR,  "Error %d occured trying to initialize database", error.errorNumber));
      errCloseStack(&error);
    }
#endif
  }
  else
    isInit=TRUE;

}

CCS::~CCS()
{
  ccsERROR error;	
  errResetStack(&error);
  if (ccsExit(&error)==FAILURE) {
    ACS_LOG (LM_RUNTIME_CONTEXT, "cdb::CCS::~CCS",
	     (LM_ERROR,  "Error %d occured trying to close database", error.errorNumber));
    errCloseStack(&error);
  }
}

Boolean CCS::CreateRecord(const String &strRecordName,
			  Boolean bTruncate)
{
  ACE_UNUSED_ARG(strRecordName);
  ACE_UNUSED_ARG(bTruncate);
  return FALSE;
}

ULong CCS::GetRecordState(const String &strRecordName)
{
  ACE_UNUSED_ARG(strRecordName);
  return CDB_RECORD_READABLE | CDB_RECORD_WRITABLE;
}

/****************************************************/
/* procedure used to get data from the CCS database */ 
/****************************************************/

Boolean CCS::GetField(const String &strRecordName,
		      const String &strFieldName,
		      Field &fld)
{

  ACE_Guard<ACE_Recursive_Thread_Mutex> guard(cdbMutex);

  dbSYMADDRESS  pointName;
  dbATTRIBUTE   attrName;
  dbTYPE        dataType[dbMAX_FIELD_CNT];
  char	  buffer[10000];	
  int           i,j,totField;
  vltINT32      actual;
  vltUINT16     recordCnt;
  dbATTRTYPE    attrType;
  ccsERROR      error;

  unsigned char   flag;     /* cast to rtLOGICAL datatype */
  unsigned char   c;
  short           s;
  long            l;
  float           f;
  double          d;
  char            string[257],*pBuf;

  ShortArray      shortArray;
  OctetArray      octetArray;
  StringArray     stringArray;
  DoubleArray     doubleArray;
  FloatArray      floatArray;
  LongArray       longArray;
  ULongArray      uLongArray;
  UShortArray     uShortArray;
    
  errResetStack(&error);

  strcpy((char *) pointName,strRecordName.c_str());
  strcpy((char *) attrName,strFieldName.c_str());

  if ( dbReadSymbolic(pointName,attrName,dataType,
		      buffer,sizeof(buffer),
		      &actual,&recordCnt,&attrType,&error) == FAILURE )
    { 

      char errmsg[256];
      errmsg[0] = 0;
      switch (error.errorNumber) {
#ifndef MAKE_VXWORKS
      case ccsERR_DB_INV_NAME:  ACE_OS::strcpy(errmsg, "Invalid point, attribute or alias name"); break;
      case ccsERR_DB_READ:      ACE_OS::strcpy(errmsg, "Generic failure during reading"); break;
      case ccsERR_DB_LCU:       ACE_OS::strcpy(errmsg, "Failure during access to a LCU"); break;
      case ccsERR_DB_QUALITY:   ACE_OS::strcpy(errmsg, "Bad data quality"); break;
      case ccsERR_NULL_PTR:     ACE_OS::strcpy(errmsg, "In case of NULL pointer in input parameters"); break;
      case ccsERR_BUFF_SIZE:    ACE_OS::strcpy(errmsg, "Buffer size too small"); break;
      case ccsERR_DB_NO_RTAP:   ACE_OS::strcpy(errmsg, "Failure when calling RTAP with CCS LITE"); break;
      case ccsERR_DB_CATEGORY:  ACE_OS::strcpy(errmsg, "Category mismatch"); break;
      case ccsERR_DB_GROUP:     ACE_OS::strcpy(errmsg, "Group mismatch"); break;
      case ccsERR_DB_INV_RECEL: ACE_OS::strcpy(errmsg, "nvalid record/element in address"); break;
      case ccsERR_DB_INV_FIELD: ACE_OS::strcpy(errmsg, "Invalid field specifier in address"); break;
      case ccsERR_ENV_NOT_ACTIVE: ACE_OS::strcpy(errmsg, "Environment is not active"); break;
      case ccsERR_REMOTE_LINK:   ACE_OS::strcpy(errmsg, "The DB is unreacheable"); break;
      case ccsERR_DB_OPEN: 	ACE_OS::strcpy(errmsg, "DB can not be opened"); break;
#else
      case lccERR_BUF_SIZE:  ACE_OS::strcpy(errmsg, "Buffer size too small"); break;
      case lccERR_NOT_FOUND: ACE_OS::strcpy(errmsg, "Invalid point, attribute or alias name"); break;
      case lccERR_ATTRIBUTE: ACE_OS::strcpy(errmsg, "Invalid attribute name"); break;
      case lccERR_POINT:     ACE_OS::strcpy(errmsg, "Invalid point name"); break;
      case lccERR_INV_NAME:  ACE_OS::strcpy(errmsg, "Invalid point, attribute or alias name"); break;
      case lccERR_DE_TYPE:   ACE_OS::strcpy(errmsg, "Invalid data type"); break;
      case lccERR_ATTR_TYPE: ACE_OS::strcpy(errmsg, "Invalid attribute type"); break;
      case lccERR_INV_RECEL: ACE_OS::strcpy(errmsg, "Invalid record/element in address"); break;
#endif
      default: ACE_OS::strcpy(errmsg, "Unknown error in reading CDB");
      }

      ACS_LOG (LM_RUNTIME_CONTEXT, "cdb::CCS::GetField",
	       (LM_NOTICE, "Error reading '%s.%s' : %s", pointName, attrName, errmsg));

      errCloseStack(&error);
      return FALSE;
    }
	
  i=0;
  pBuf = (char *)buffer;
  totField = 0;
  while (dataType[totField] != dbUNDEFINED) totField++;

  for (j=0; j<recordCnt ; j++)   
    {
      switch(dataType[i])
	{
	case dbINT8:
	  dbMemCpy((char *) &c,pBuf,dataType[i]);
	  if (attrType==dbSCALAR) fld=Field(Short(c));
	  else 
	    { 
	      shortArray.push_back(Short(c));
	      pBuf += sizeof(s);
	      if (j==(recordCnt-1)) fld=Field(shortArray);			     
	    }
	  break;
	case dbUINT8:
	  dbMemCpy((char *) &c,pBuf,dataType[i]);
	  if (attrType==dbSCALAR) fld=Field(Octet(c));
	  else
	    {
	      octetArray.push_back(Octet(c));
	      pBuf += sizeof(c);
	      if (j==(recordCnt-1)) fld=Field(octetArray);
	    }
	  break;

	case dbINT16:
	  dbMemCpy((char *) &s,pBuf,dataType[i]);
	  if (attrType==dbSCALAR) fld=Field(Short(s));
	  else
	    {
	      shortArray.push_back(Short(s));
	      pBuf += sizeof(s);
	      if (j==(recordCnt-1)) fld=Field(shortArray);
	    }
	  break;
	case dbUINT16:
	  dbMemCpy((char *) &s,pBuf,dataType[i]);
	  if (attrType==dbSCALAR) fld=Field(UShort(s));
	  else
	    {
	      uShortArray.push_back(UShort(s));
	      pBuf += sizeof(s);
	      if (j==(recordCnt-1)) fld=Field(uShortArray);
	    }
	  break;
            
	case dbINT32:
	  dbMemCpy((char *) &l,pBuf,dataType[i]);
	  if (attrType==dbSCALAR) fld=Field(Long(l));
	  else
	    {
	      longArray.push_back(Long(l));
	      pBuf += sizeof(l);
	      if (j==(recordCnt-1)) fld = Field(longArray);
	    }
	  break;
	
	case dbUINT32:
	  dbMemCpy((char *) &l,pBuf,dataType[i]);
	  if (attrType==dbSCALAR) fld=Field(ULong(l));
	  else
	    {
	      uLongArray.push_back(ULong(l));
	      pBuf += sizeof(l);
	      if (j==(recordCnt-1)) fld= Field(uLongArray);
	    }  
	  break;
	case dbDOUBLE: 
	  dbMemCpy((char *) &d,pBuf,dataType[i]);          
	  if (attrType==dbSCALAR) fld=Field(Double(d));
	  else
	    {
	      doubleArray.push_back(Double(d));
	      pBuf += sizeof(d);
	      if (j==(recordCnt-1)) fld = Field(doubleArray);
	    }
	  break;

	case dbFLOAT: 
	  dbMemCpy((char *) &f,pBuf,dataType[i]);          
	  if (attrType==dbSCALAR) fld=Field(Float(f));
	  else
	    {
	      floatArray.push_back(Float(f));
	      pBuf += sizeof(f);
	      if (j==(recordCnt-1)) fld = Field(floatArray);
	    }  
	  break;

	case dbLOGICAL:
	  dbMemCpy((char *) &flag,pBuf,dataType[i]);          
	  //fld=Field(Boolean(flag)); !!!
	  pBuf += sizeof(flag);
	  break;

	case dbBYTES4:
	case dbBYTES8:
	case dbBYTES12:
	case dbBYTES16:
	case dbBYTES20:
	case dbBYTES32:
	case dbBYTES48:
	case dbBYTES64:
	case dbBYTES80:
	case dbBYTES128:
	case dbBYTES256:
	  dbMemCpy(string,pBuf,dataType[i]);
	  string[sizeof(string) - 1] = '\0';
	  if (attrType==dbSCALAR) fld=Field(String(string));
	  else
	    {
	      stringArray.push_back(String(string));
	      if (j==(recordCnt-1)) fld=Field(stringArray);
	    }
#ifndef MAKE_VXWORKS
	  pBuf += dbDataElemSize[dataType[i]];
#else
	  pBuf += dbTypes[dataType[i]].size;
#endif

	  break;

	case dbTIME_OF_DAY:
	  dbDeToStr(dbTIME_OF_DAY,pBuf,string,0,2);
	  if (attrType==dbSCALAR) fld=Field(String(string));
	  else
	    {
	      stringArray.push_back(String(string));
	      if (j==(recordCnt-1)) fld=Field(stringArray);
	    }
#ifndef MAKE_VXWORKS

#else
	  pBuf += dbTypes[dataType[i]].size;
#endif
	  break;

	case dbDATE:
	  dbDeToStr(dbDATE,pBuf,string,0,2);
	  if (attrType==dbSCALAR) fld=Field(String(string));
	  else
	    {
	      stringArray.push_back(String(string));
	      if (j==recordCnt) fld=Field(stringArray);
	    }
#ifndef MAKE_VXWORKS
	  pBuf += dbDataElemSize[dataType[i]];
#else
	  pBuf += dbTypes[dataType[i]].size;
#endif
	  break;
  
	default:        
	  return FALSE;
	}   //switch   
    }     //for
	 

	
  return TRUE;
}

Boolean CCS::SetField(const String &strRecordName,
		      const String &strFieldName,
		      const Field &fld,
		      Boolean bCreate)
{

  ACE_Guard<ACE_Recursive_Thread_Mutex> guard(cdbMutex);

  dbATTRTYPE          attrType;
  dbTYPE              dataType[dbMAX_FIELD_CNT];
    
  ccsERROR            error;
  unsigned int         i,j,totRec;
    
  vltINT32            actual,bufSize;
  vltUINT32           recSize;
  vltUINT16           recCnt,/*startRec,endRec,*/recsUsed;   
  vltUINT8            fieldCnt/*,startField,endField*/;

  
  Short cs;
  UShort us;
  ULong ul;
  Boolean bf;
  unsigned char   flag;     /* cast to rtLOGICAL datatype */
  unsigned char   c;
  short  s;
  long   l;
  float  f;
  double d;

  /*  Field::Type fieldType;
      ccsTIMEVAL     myTime;
      vltPOLAR       myPolar;
      vltRECTANGULAR myRec;    
      vltDBXREF      myXref;
  */
  String         string;
  StringArray    stringArray;
  OctetArray     octetArray;
  ShortArray     shortArray;
  LongArray      longArray;
  UShortArray    uShortArray;
  ULongArray     uLongArray;
  FloatArray     floatArray;
  DoubleArray    doubleArray;

  char   userBuf[10000],*pBuf;
  char str[256];

  ACE_UNUSED_ARG(bCreate);

  errResetStack(&error);

  /****************************************
   *   Check if attribute name is correct  *
   *   and get type of data                *
   *****************************************/
  if (dbGetAttrInfo(strRecordName.c_str(),strFieldName.c_str(),&attrType,
		    &fieldCnt,&recCnt,&recSize,
		    &recsUsed,dataType,&error) == FAILURE) goto exit;
 
  /*   Parameter Initialization according to the data type */
    
  /*    if (dbParseIndex(strFieldName,attrType,fieldCnt,recCnt,
	&startRec,&endRec,&startField,
	&endField) == FAILURE) goto exit;
  */

  //ACS_SHORT_LOG((LM_DEBUG, "fieldCnt: %d, recCnt: %d, recSize: %d.", fieldCnt, recCnt, recSize));

  totRec = recCnt;
    
  //    totRec     = endRec - startRec +1;

  pBuf       = userBuf;
    
  /************************************
   *   Prepare buffer to be written    *
   ************************************/ 
  bufSize = 0;  
  for (j=0; j<totRec; j++)
    {
      i = 0;
      switch(dataType[i])
        {
        case dbINT8:
          if (totRec==1) {
	    if (!fld.GetShort(cs)) goto exit;
	    c=(unsigned char)cs;
	  }
	  else {
	    if ((j==0) && !(fld.GetShortArray(shortArray))) goto exit;
	    if (j<shortArray.size()) c=shortArray[j];
	    else c=0;
	  }
          if (dbFillBuf(&pBuf,(char *) &c,&bufSize,dataType[i]) == FAILURE)
            goto exit;             
	  break;          
        case dbINT16:
	  if (totRec==1) {
	    if (!fld.GetShort(s)) goto exit;
	  }
	  else {
	    if ((j==0) && !(fld.GetShortArray(shortArray))) goto exit;
	    if (j<shortArray.size()) s=shortArray[j];
	    else s=0;
	  }
	  if (dbFillBuf(&pBuf,(char *) &s,&bufSize,dataType[i]) == FAILURE)
	    goto exit;             
	  break;
		
        case dbUINT8:
	  if (totRec==1) {
	    if (!fld.GetOctet(c)) goto exit;
	  }
	  else {
	    if ((j==0) && !(fld.GetOctetArray(octetArray))) goto exit; 
	    if (j<octetArray.size()) c=octetArray[j];
	    else c=0;
	  }
	  if (dbFillBuf(&pBuf,(char *) &c,&bufSize,dataType[i]) == FAILURE)
	    goto exit;             
	  break;          
	case dbUINT16:
	  if (totRec==1) {
	    if (!fld.GetUShort(us)) goto exit;
	    s=(short)us;
	  }
	  else {
	    if ((j==0) && !(fld.GetUShortArray(uShortArray))) goto exit;
	    if (j<uShortArray.size()) s=uShortArray[j];
	    else s=0;
	  }
          if (dbFillBuf(&pBuf,(char *) &s,&bufSize,dataType[i]) == FAILURE)
	    goto exit;             
	  break;
        case dbINT32:
	  if (totRec==1) {
	    Long tl = l;
	    if (!fld.GetLong(tl)) goto exit;
	    l = tl;
	  }
	  else {
            if ((j==0) && !(fld.GetLongArray(longArray))) goto exit;
	    if (j<longArray.size()) l=longArray[j];
	    else l=0;
	  }
          if (dbFillBuf(&pBuf,(char *) &l,&bufSize,dataType[i]) == FAILURE)
	    goto exit;             
	  break;
        case dbUINT32:
	  if (totRec==1) {
	    if (!fld.GetULong(ul)) goto exit;
	    l = (long)ul;
	  }
	  else {
	    if ((j==0) && !(fld.GetULongArray(uLongArray))) goto exit;
	    if (j<uLongArray.size()) l=uLongArray[j];
	    else l=0;
	  }
          if (dbFillBuf(&pBuf,(char *) &l,&bufSize,dataType[i]) == FAILURE)
            goto exit;             
	  break;
      
        case dbDOUBLE: 
	  if (totRec==1) {
	    if (!fld.GetDouble(d)) goto exit;
	  }
	  else {
	    if ((j==0) && !(fld.GetDoubleArray(doubleArray))) goto exit;
	    if (j<doubleArray.size()) d=doubleArray[j];
	    else d=0;
	  }

          if (dbFillBuf(&pBuf, (char *) &d ,&bufSize,dataType[i]) == FAILURE)
            goto exit;               
	  break;

        case dbFLOAT: 
	  if ((totRec==1) && !(fld.GetFloat(f))) goto exit;
	  else {
	    if ((j==0) && !(fld.GetFloatArray(floatArray))) goto exit;
	    if (j<floatArray.size()) f=floatArray[j];
	    else f=0;
	  }
          if (dbFillBuf(&pBuf,(char *) &f,&bufSize,dataType[i]) == FAILURE)
            goto exit;               
	  break;

        case dbLOGICAL:
          if (totRec==1){
	    if (!fld.GetBoolean(bf)) goto exit;
	    flag = bf;
            if (dbFillBuf(&pBuf,(char *) &flag,&bufSize,dataType[i]) == FAILURE)
              goto exit;               
	  }
	  break;

        case dbBYTES4:
        case dbBYTES8:
        case dbBYTES12:
        case dbBYTES16:
        case dbBYTES20:
        case dbBYTES32:
        case dbBYTES48:
        case dbBYTES64:
        case dbBYTES80:
        case dbBYTES128:
        case dbBYTES256:
	  if (totRec==1) {
	    if (!fld.GetString(string)) goto exit;
	  }
	  else {
	    if ((j==0) && !(fld.GetStringArray(stringArray))) goto exit;
	    if (j<stringArray.size()) string=stringArray[j];
	    else string="";
	  }
	  strcpy(str, string.c_str());
	  if (dbFillBuf(&pBuf,(char *) &str,&bufSize,dataType[i]) == FAILURE)
            goto exit;
	  break;

	default:        
	  goto exit;
	}     
    }    
    
  /******************************
   *       Write values         *
   ******************************/
  if (dbWriteSymbolic(strRecordName.c_str(),strFieldName.c_str(),dataType,
		      userBuf,bufSize,&actual,
		      totRec,attrType,&error) == FAILURE) goto exit;

  return TRUE;  

 exit:
  errCloseStack(&error);
  return FALSE;

}

Boolean CCS::RemoveField(const String &,
			 const String &)
{
  return FALSE;
}

Boolean CCS::GetRecord(const String &,
		       Record &,
		       Boolean,
		       Boolean)
{
  return FALSE;
}
  
Boolean CCS::SetRecord(const String &,
		       const Record &,
		       Boolean,
		       Boolean)
{
  return FALSE;
}

Boolean CCS::RemoveRecord(const String &)
{
  return FALSE;
}

Boolean CCS::GetChildren(const String &,
			 StringArray &)
{
  return FALSE;
}

 }; 

  // ************************************************************************
  //
  // REVISION HISTORY:
  //
  //   $Log: cdbCCS.cpp,v $
  //   Revision 1.28  2011/10/28 15:05:05  hsommer
  //   Manually fixed "no LGPL license text" issue reported by addCopyright.py
  //
  //   Revision 1.27  2006/09/01 02:20:54  cparedes
  //   small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
  //
  //   Revision 1.26  2004/10/14 20:46:18  gchiozzi
  //   Cleaned up logging messages:
  //   - uses now only ACS_* macros and not any more ACE_* macros
  //   - Removed all new line characters
  //   - fixed problem with ACS logging that was not putting proper new line
  //     characters when logging messages on stdout BEFORE the logging itself
  //     was initialised.
  //
  //   Revision 1.25  2003/08/18 12:36:00  rgeorgie
  //   LGPL header added
  //
  //   Revision 1.24  2003/01/28 16:44:04  vltsccm
  //   gchiozzi: patch for cdb module to create lib/endorsed directory, since CVS cannot restore empty directories
  //
  //   Revision 1.23  2003/01/24 10:44:27  vltsccm
  //   cdb1.23
  //
  //   Revision 1.22  2003/01/20 15:12:34  vltsccm
  //   cdb1.22
  //
  //   Revision 1.21  2003/01/20 10:46:07  vltsccm
  //   cdb1.21
  //
  //   Revision 1.20  2002/12/05 16:04:12  vltsccm
  //   cdb1.20
  //
  //   Revision 1.19  2002/11/25 16:05:06  vltsccm
  //   cdb1.19
  //
  //   Revision 1.18  2002/11/13 14:53:18  vltsccm
  //   cdb1.18
  //
  //   Revision 1.17  2002/11/13 10:22:44  vltsccm
  //   cdb1.17
  //
  //   Revision 1.16  2002/11/06 08:37:40  vltsccm
  //   cdb1.16
  //
  //   Revision 1.15.1.23  2002/11/05 16:05:27  vltsccm
  //   cdb1.15.1.23
  //
  //   Revision 1.15.1.22  2002/11/05 13:46:46  vltsccm
  //   cdb1.15.1.22
  //
  //   Revision 1.15.1.21  2002/11/05 10:41:27  vltsccm
  //   cdb1.15.1.21
  //
  //   Revision 1.15.1.20  2002/11/01 12:49:17  vltsccm
  //   cdb1.15.1.20
  //
  //   Revision 1.15.1.19  2002/10/30 07:56:58  vltsccm
  //   cdb1.15.1.19
  //
  //   Revision 1.15.1.18  2002/10/25 12:44:41  vltsccm
  //   cdb1.15.1.18
  //
  //   Revision 1.15.1.17  2002/10/24 13:08:57  vltsccm
  //   cdb1.15.1.17
  //
  //   Revision 1.15.1.16  2002/10/16 11:44:29  vltsccm
  //   cdb1.15.1.16
  //
  //   Revision 1.15.1.15  2002/10/14 22:26:35  vltsccm
  //   cdb1.15.1.15
  //
  //   Revision 1.15.1.14  2002/10/14 12:18:58  vltsccm
  //   cdb1.15.1.14
  //
  //   Revision 1.15.1.13  2002/10/04 16:20:40  vltsccm
  //   cdb1.15.1.13
  //
  //   Revision 1.15.1.12  2002/10/02 12:54:26  vltsccm
  //   cdb1.15.1.12
  //
  //   Revision 1.15.1.11  2002/10/01 10:33:38  vltsccm
  //   cdb1.15.1.11
  //
  //   Revision 1.15.1.10  2002/09/30 13:57:47  vltsccm
  //   cdb1.15.1.10
  //
  //   Revision 1.15.1.9  2002/09/26 14:13:16  vltsccm
  //   cdb1.15.1.9
  //
  //   Revision 1.15.1.8  2002/09/26 07:47:07  vltsccm
  //   cdb1.15.1.8
  //
  //   Revision 1.15.1.7  2002/09/17 16:19:26  vltsccm
  //   cdb1.15.1.7
  //
  //   Revision 1.15.1.6  2002/09/17 11:15:51  vltsccm
  //   cdb1.15.1.6
  //
  //   Revision 1.15.1.5  2002/09/02 09:37:09  vltsccm
  //   cdb1.15.1.5
  //
  //   Revision 1.15.1.4  2002/08/09 09:35:27  vltsccm
  //   cdb1.15.1.4
  //
  //   Revision 1.15.1.3  2002/07/24 07:29:14  vltsccm
  //   cdb1.15.1.3
  //
  //   Revision 1.15.1.2  2002/07/12 09:58:22  vltsccm
  //   cdb1.15.1.2
  //
  //   Revision 1.15+.1.1  2002/07/09 09:40:33  vltsccm
  //   cdb1.15.1
  //
  //   Revision 1.15  2002/02/05 17:50:10  vltsccm
  //   cdb1.15
  //
  //   Revision 1.14  2002/01/14 21:14:20  vltsccm
  //   cdb1.14
  //
  //   Revision 1.13  2001/10/19 09:56:25  vltsccm
  //   cdb1.13
  //
  //   Revision 1.12  2001/09/18 10:07:14  vltsccm
  //   cdb1.12
  //
  //   Revision 1.11  2001/07/12 07:48:31  vltsccm
  //   cdb1.11
  //
  //   Revision 1.10  2001/07/11 09:16:28  vltsccm
  //   cdb1.10
  //
  //   Revision 1.6  2000/12/07 18:00:43  vltsccm
  //   cdb1.6
  //
  //   Revision 1.5  2000/11/17 13:15:01  vltsccm
  //   cdb1.5
  //
  //   Revision 1.4  2000/10/20 13:51:36  vltsccm
  //   cdb1.4
  //
  //   Revision 1.3  2000/10/20 13:51:36  vltsccm
  //   cdb1.3
  //
  //   Revision 1.2  2000/10/20 13:51:36  vltsccm
  //   cdb1.2
  //
  //   Revision 1.1  2000/10/20 13:51:35  vltsccm
  //   cdb1.1
  //
  //   Revision 1.0  2000/10/20 13:51:35  vltsccm
  //   cdb1.0
  //
  //   Revision 1.3  2000/10/13 16:03:04  vltsccm
  //   cdb1.3
  //
  //   Revision 1.2  2000/09/13 14:49:30  vltsccm
  //   cdb1.2
  //
  //   Revision 1.1  2000/09/06 15:42:12  vltsccm
  //   cdb1.1
  //
  //   Revision 1.1  2000/08/09 08:42:53  matej
  //   1st working version
  //
  //   Revision 1.1  2000/06/13 07:26:24  kzagar
  //   CDB, initial commit. Documentation not yet finished.
  //
  // ************************************************************************
