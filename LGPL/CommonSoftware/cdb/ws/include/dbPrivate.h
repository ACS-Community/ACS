/**************************************************************************
* E.S.O. - VLT project
#
# "@(#) $Id: dbPrivate.h,v 1.24 2003/01/28 16:43:51 vltsccm Exp $" 
*
* <dbPrivate.h> -  CCS/ON-LINE DATABASE Internal Interface
*                  This interface is used by the inner most levels 
*                  of the database library.
*
* who        when      what
* ---------  --------  ----------------------------------------------
* rabuter   14/03/96  Added support for MultiRead and MultiWrite LCU commands
* M.COMIN   27/05/93  Preliminary version
* M.COMIN   05/07/93  First official release.
* M.COMIN   09/07/93  Take away Global data section
* M.COMIN   10/08/93  Add extern rtErrno
* rabuter   14/03/96  Added support for MultiRead and MultiWrite LCU commands
* mcomin    06/05/98  dbAddTable,dbLockConfig,dbUnlockConfig moved to 
*                     dbCCS.h for backwards compatibility
* bgilli    05/05/98  dbParseClassAttr moved to dbCCS.h for backwards compat.
*
* ------------------
*
* M.COMIN   20/01/94  Changed according to the CCS design
* M.COMIN   28/03/94  Add global data structure to keep track 
*                     of opened connections 
*
* M.COMIN   12/04/94  Variable numbLinks and linkTables defined as extern
*                     Add routine dbParseIndex()
* M.COMIN   25/04/94  Add data structures for LCU access
*
* G.CHIOZZI 19/10/94  Added new functions to handle static attributes
*                     and point classes. Some of them could become
*                     public.
* T.HERLIN  31/10/94  Added function to resolve remote direct address
*
* ------------------
*
* M.COMIN   25/01/95  Modifications due to db.h splitting into CCS and LCC parts
* J.I.LEON  23/05/95  Add rtDateElemSize for CCS light. 
* B.Gilli   17/11/95  Added dbXrefToAlias.
* T.Ebert   19/04/96  Added dbGetDirAddrOnEnv()
* T.Ebert   05/06/96  Added dbREAD_WRITE
* T.Ebert   18/06/96  Added definition of dbLCU_TIMEOUT
* mcomin    12/09/96  Move definition for data quality to dbCCS.h
*           25/02/97  Move some define to dbCCS.h
* mcomin    27/03/97  Add dbStringToString()
*           11/09/97  Add dbCloseOpen()
*           13/09/97  Add defines for link up, down or new
*           18/03/98  Functions dbParseStatus, dbXrefToAlias and 
*                     dbGetDirAddrOnEnv moved to db.h
*
*************************************************************************/
#ifndef DB_PRIVATE_H
#define DB_PRIVATE_H

#include "msg.h"
#include "lccErrors.h"
#include "ccsRtap.h"

#define LCU_DATABASE   "rdbServer"
#define dbLCU_TIMEOUT  30000


/****************************************************
 *                                                  *
 *  ONLINE DATABASE   : Internal data Structures    *
 *                                                  *
 ****************************************************/

#define  MAX_CONNECTIONS  100
#define  dbLINK_DOWN      0
#define  dbLINK_UP        1
#define  dbLINK_NEW       2
#define  dbLINK_CLOSED    3

typedef struct {
    ccsENVNAME  envName;          /* Environment name                */
    vltUINT8    status;           /* Status of the link : UP/DOWN    */
    ccsENV_TYPE type;             /* Type of environment : WS or LCU */
    dbHANDLE    linkId;           /* Handle to access the database   */
    } dbLinkStatus;

extern vltUINT32  numbLinks; 

/* The first record of the table is for the local database */
extern dbLinkStatus linkTable[MAX_CONNECTIONS];

typedef vltUINT16 dbDIRECT_TYPE;    

typedef enum {
    dbDB_DIRECT,
    dbDB_SYMBOLIC
} dbADDRESS_TYPE;

typedef enum {
    dbSYM_HIERARCHICAL,
    dbSYM_ALIAS,
    dbSYM_UNKNOWN
} dbSYMBOLIC_TYPE;

typedef struct {
    dbDIRECT_TYPE     type;     /* type of direct address  */
    vltUINT16         reserved;
    dbPOINT          *pointId;  /* direct address          */
} dbDIRECT_ADDR;

typedef struct {
    dbSYMBOLIC_TYPE   type;     /* type of symbolic address                 */
    char             *name;     /* symbolic name                            */
} dbSYMBOLIC_ADDR;

typedef struct {
    dbADDRESS_TYPE      type;     /* type of address, direct or symbolic    */
    union {
	dbDIRECT_ADDR   direct;   /* direct address                         */
	dbSYMBOLIC_ADDR symbolic; /* symbolic address                       */
    }                   ds;
} dbDB_ADDRESS;

typedef struct {
    dbDB_ADDRESS      addr;      /* database address                        */
    vltUINT32         quality;   /* quality of value (not used on LCU)      */
    vltUINT16         recordCnt; /* number of records read/written          */
    dbATTRTYPE        attrType;
    vltUINT8          reserved; 
    dbTYPE            *deTypes; 
    vltUINT8          *buffer;   /* data buffer                             */
    vltINT32          size;      /* size of buffer                          */
    vltINT32          actual;    /* actual size of data read/written        */
} dbREAD_WRITE;

/***********************************************************************
 *                                                                     *
 *  ONLINE DATABASE   : Internal Routines                              *
 *                                                                     *
 ***********************************************************************/

/* The application programmer must use routines ccsInit to connect     */
/* to the local database and routine ccsExit to close all connections. */  
/* The connection to a remote database is established at the moment    */
/* the user access it.                                                 */
/*                                                                     */
/* The following two routines should not be used directly by the       */
/* application programmer.                                             */



ccsCOMPL_STAT dbSendCmd(           /* Sends a command to a LCU        */
				   /* to perform database operations  */
         msgCMD          command,  /* ASCII command      */ 
         ccsENVNAME      envName,  /* Environment name   */
         char           *cmdParam, /* Command parameters */
	 msgLENGTH       paramLen, /* Buffer length of cmd parameters */
	 msgCHECKFLAG    flag,     /* Checking options (not used yet) */
	 char          **recvMsg,  /* Pointer to received message     */
	 ccsERROR       *error     /* returned error structure        */
   );


ccsCOMPL_STAT dbParseIndex(        /* Parses the attribute specification and */
                                   /* gives back the associated indexes.     */
   char         *attrName,         /* Attribute name  */
   dbATTRTYPE   attrType,          /* Attribute type : scalar,vector, table */
   vltUINT8     fieldCnt,         
   vltUINT16    recCnt,
   vltUINT16    *startRec,
   vltUINT16    *endRec,
   vltUINT8     *startField,
   vltUINT8     *endField
   );

char *dbPadString (     /* Pad strings with NULL characters */
  dbTYPE      dataType,
  char       *buffer,
  vltINT32    size,
  vltINT32   *newSize
);

ccsCOMPL_STAT dbOpenLink (    /* Re-opens a DB connection  */
   const ccsENVNAME envName,  /* Environment name          */
   int       lkIndex,         /* Entry to table of existing connections */
   ccsERROR  *error
);

ccsCOMPL_STAT dbCloseLink ( 
   int       index,
   ccsERROR *error
);

ccsCOMPL_STAT dbSetLinkStatus ( 
   const ccsENVNAME envName,
   vltUINT8  newStatus 
);

ccsCOMPL_STAT dbStringToString (
   char          *source,
   vltINT32       size,
   unsigned char *buffer
);

dbVIEW dbParseView (                /* Extract the View specifier in the    */
                                    /* symbolic address of a point          */
   const dbSYMADDRESS  fullName,    /* Full point Name with env. specifier  */
         dbSYMADDRESS  **pointName  /* Point address without env. specifier */
   );
   

ccsCOMPL_STAT dbGetAddrType( 
   dbSYMADDRESS     *address,
   dbSYMADDRESS     **pointName,
   rtDbAddress      *dbAddress
   );

/*
 ***********************************************************************
 * Configuration operations to modify the DB structure.                *
 * The Operations allow to add/delete points and attributes,           *
 * copy points with their attributes or entire DB branches.            *
 ***********************************************************************
 */

#ifndef CCS_LIGHT
		
ccsCOMPL_STAT dbAddPoint(            /* Add a point to the database     */
  const dbSYMADDRESS  parentPoint,   /* Reference to the parent point   */
  const dbSYMADDRESS  pointName,     /* Name of the new database point  */
  const dbSYMADDRESS  alias,         /* Alias of the new point          */
        dbRESIDENCE   residence,     /* RAM or DISK resident            */
        ccsERROR      *error         /* returned error structure.       */
    );

ccsCOMPL_STAT dbAddScalar(           /* Add SCALAR attribute to a point */
  const dbSYMADDRESS  pointName,     /* Reference to the database point */
  const dbATTRIBUTE   attrName,      /* Name of the Attribute           */
        dbTYPE        dataType,      /* Attribute data type             */
        ccsERROR      *error         /* returned error structure.       */
    );  

ccsCOMPL_STAT dbAddVector(           /* Add VECTOR attribute to a point  */
  const dbSYMADDRESS  pointName,     /* Reference to the database point  */
  const dbATTRIBUTE   attrName,      /* Name of the attribute            */
        dbTYPE        dataType,      /* Element data type                */
        vltUINT16     elemCnt,       /* Number of elements               */
        ccsERROR      *error         /* returned error structure         */
    );

ccsCOMPL_STAT dbDelAttr(            /* delete one attribute from a point */
  const dbSYMADDRESS pointName,     /* Reference to the database point   */
  const dbATTRIBUTE  attrName,      /* Name of the Attribute             */
        dbATTRTYPE   type,          /* Attribute data type               */
        ccsERROR     *error         /* returned error structure.         */
    );

ccsCOMPL_STAT dbChangeAlias(        /* change alias of a point         */
  const dbSYMADDRESS pointName,     /* Reference to the database point */
  const dbSYMADDRESS newAlias,      /* New Alias Name                  */
        ccsERROR     *error         /* returned error structure.       */
    ); 

ccsCOMPL_STAT dbChangeName(         /* change name  of a point         */
  const dbSYMADDRESS pointName,     /* Reference to the database point */
  const dbSYMADDRESS newName,       /* New Alias Name                  */
        ccsERROR     *error         /* returned error structure.       */
    );

ccsCOMPL_STAT dbChangeAttrName(     /* change name for an attribute    */
  const dbSYMADDRESS pointName,     /* Reference to the database point */
  const dbATTRIBUTE  attrName,      /* Name of the Attribute           */
  const dbATTRIBUTE  newAttrName,   /* New Attribute name              */
        ccsERROR     *error         /* returned error structure.       */
    );

ccsCOMPL_STAT dbCopyAttr(            /* copy one attribute name         */
  const dbATTRIBUTE   attrName,      /* attribute                       */
  const dbSYMADDRESS  origPoint,     /* copy from that point            */
  const dbATTRIBUTE   newName,       /* New attribute name (optional)   */
  const dbSYMADDRESS  destPoint,     /* ... to that other point         */
        ccsERROR      *error         /* returned error structure.       */
    );

ccsCOMPL_STAT dbCopyBranch(          /* copy one complete db branch     */
  const dbSYMADDRESS  origPoint,     /* copy from that point down       */
  const dbSYMADDRESS  destPoint,     /* ... to that other point         */
        ccsERROR      *error         /* returned error structure.       */
    );

ccsCOMPL_STAT dbCopyPoint(           /* copy one complete point         */
  const dbSYMADDRESS  origPoint,     /* copy from that point            */
  const dbSYMADDRESS  destPoint,     /* ... to that other point         */
        ccsERROR      *error         /* returned error structure.       */
    );

ccsCOMPL_STAT dbDelBranch(           /* delete a complete db Branch     */
  const dbSYMADDRESS  origPoint,     /* delete from that point on       */
        ccsERROR      *error         /* returned error structure.       */
    );

ccsCOMPL_STAT dbDelPoint(            /* delete a complete point         */
  const dbSYMADDRESS  origPoint,     /* delete that point               */
        ccsERROR      *error         /* returned error structure.       */
    );

ccsCOMPL_STAT dbMovePoint(           /* move one complete point and child */
  const dbSYMADDRESS  origPoint,     /* move from that point              */
  const dbSYMADDRESS  destPoint,     /* ... to that other point           */
        ccsERROR      *error         /* returned error structure.         */
    );
#endif

ccsCOMPL_STAT dbChangeResidence(     /* change point residence RAM <-> DISK */
  const dbSYMADDRESS  origPoint,     /* point to be changed                 */
        ccsERROR      *error         /* returned error structure.           */
    );

ccsCOMPL_STAT dbGetClass(            /* Retrives from the given point addr   */
  const dbSYMADDRESS  point,         /* the symbolic address for its base    */
        dbSYMADDRESS  classAddr,     /* class.                               */
        ccsERROR      *error
    );

ccsCOMPL_STAT dbTestSymaddr(         /* Tests if the given symbolic address  */
  const dbSYMADDRESS  point,         /* correspond to an existing database   */
	vltLOGICAL   *result,        /* entity                               */
	ccsERROR     *error
    );

ccsCOMPL_STAT dbXrefToAlias( 
    const ccsENVNAME env,
    const DB_XREF    *addr,
    char       *string,
    ccsERROR   *error
    );


ccsCOMPL_STAT dbLcuMultiRead  (dbLISTID listId, ccsERROR *error );
ccsCOMPL_STAT dbLcuMultiWrite (dbLISTID listId, vltUINT8 writemode, 
                               ccsERROR *error);
ccsCOMPL_STAT dbConnectLite   (const ccsENVNAME envName, ccsERROR *error);
ccsCOMPL_STAT dbDisconnectLite(const ccsENVNAME envName, ccsERROR *error);
ccsCOMPL_STAT dbCmdToServer   (msgCMD command, ccsENVNAME envName,
                               char *buffer, msgLENGTH bufLen,
                               char **recParam, ccsERROR *error);

#endif /*!DB_PRIVATE_H*/
