#ifndef _acserr_Legacy_H_
#define _acserr_Legacy_H_
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
* "@(#) $Id: acserrLegacy.h,v 1.12 2012/01/20 22:07:43 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2004-02-13  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <logging.h>
#include <ace/Thread_Manager.h> 
#include <sstream>
#include <acserrS.h>
#include <acscommonC.h>
#include <exception>

// for backward compatibilty reason
namespace ACSErr {
    const ErrorCode ACSErrOK = ACSErrOK;
};


/**
 * Default error severity.
 */
#define DEFAULT_SEVERITY ACSErr::Error

/**
 * Macro helps to create local initial error object (stack) - ACSError object
 * @warning Depricated !!! Use ACS_ERROR(et, ec, ro) instead.
 * @param et - error type - group (#ACSErr::ACSErrType)
 * @param ec - error code (ACSErr::ErrorCode)
 * @param ro - routine info (string)
 *
 */
#define ACS_ERROR_BEGIN(et, ec, ro) ACSError(__FILE__, __LINE__, et, ec, ro)

/**
 *  Macro helps to create ACSError object
 * It can be used as:
 *  - ACS_ERROR() - to create "no error objet" without runtime and source info.
 *  - ACS_ERROR(ro) = #ACS_NO_ERROR(ro) - to create "no error object"
 *  - ACS_ERROR(et, ec, ro) = #ACS_ERROR_BEGIN(et, ec, ro) - to create error object (init error stack)
 *  - ACS_ERROR(et, ec, ro, se) - to create error object (init error stack) with erro severity
 *  - ACS_ERROR(pe, et, ec, ro) - to create new error object adding previous one (making stack) 
 *  - ACS_ERROR(pe, et, ec, ro, se) - to create new error object adding previous one (making stack) with error severity
 *
 * @param pe - previous error object (can be ACSError, ACSErr::ACSException or ACSErr::ErrorTrace)
 * @param et - error type - group (#ACSErr::ACSErrType)
 * @param ec - error code (ACSErr::ErrorCode)
 * @param ro - routine info (string)
 * @param se - error severity (ACSErr::Severity)
 *
 */
#define ACS_ERROR(arg...) ACSError(__FILE__, __LINE__ ,## arg)
//#define ACS_ERROR(pe, et, ec, ro) ACSError(__FILE__, __LINE__, pe, et, ec, ro, )
//#define ACS_ERROR_BEGIN(et, ec, ro) ACSError(__FILE__, __LINE__, et, ec, ro)
//#define ACS_NO_ERROR(ro) ACSError(__FILE__, __LINE__, ro)


 /**
  *Macro helps creating no error object.
  * @warning Depricated !!! Use ACS_ERROR(ro) instead.
  * @param ro - routin info (string)
  *
  */
#define ACS_NO_ERROR(ro) ACSError(__FILE__, __LINE__, ro)

 /**
  * Macro helps creating ACSException objext from ACSError object.
  * It can be used as:
  *  - ACS_EXCEPTION(eo)
  *  - ACS_EXCEPTION(et, ec, ro)
  *  - ACS_EXCEPTION(et, ec, ro, se)
  *  - ACS_EXCEPTION(pex, et, ec, ro)
  *  - ACS_EXCEPTION(pex, et, ec, ro, se)
  *
  * @param pex - previous exception (ACSErr::ACSException)
  * @param eo - error object (ACSError)
  * @param et - error type - group (#ACSErr::ACSErrType)
  * @param ec - error code (ACSErr::ErrorCode)
  * @param ro - routine info (string)
  * @param se - error severity (ACSErr::Severity)
  *
 */
#define ACS_EXCEPTION(arg...) ACSErr::ACSException( ACSError::createErrorTrace(__FILE__, __LINE__, arg) )
//#define ACS_EXCEPTION(eo) ACSErr::ACSException( eo.getErrorTrace() )

/**
 * Macro helps throwing ACSException
 * It can be used as:
 *  - THROW_ACS_EXCEPTION(eo)
 *  - THROW_ACS_EXCEPTION(et, ec, ro)
 *  - THROW_ACS_EXCEPTION(et, ec, ro, se)
 *  - THROW_ACS_EXCEPTION(pex, et, ec, ro)
 *  - THROW_ACS_EXCEPTION(pex, et, ec, ro, se)
 *
 * @param pex - previous exception (ACSErr::ACSException)
 * @param eo - error object (ACSError)
 * @param et - error type - group (#ACSErr::ACSErrType)
 * @param ec - error code (ACSErr::ErrorCode)
 * @param ro - routine info (string)
 * @param se - error severity (ACSErr::Severity)
 *
 */
#define THROW_ACS_EXCEPTION(arg...) ACE_THROW ( ACS_EXCEPTION(arg) )
                                                
/**
 *Macro helps throwing ACSException
 * It can be used as:
 *  - THROW_ACS_EXCEPTION_EX(lab ,eo)
 *  - THROW_ACS_EXCEPTION_EX(lab, et, ec, ro)
 *  - THROW_ACS_EXCEPTION_EX(lab, et, ec, ro, se)
 *  - THROW_ACS_EXCEPTION_EX(lab, pex, et, ec, ro)
 *  - THROW_ACS_EXCEPTION_EX(lab, pex, et, ec, ro, se)
 *
 * @param pex - previous exception (ACSErr::ACSException)
 * @param eo - error object (ACSError)
 * @param et - error type - group (#ACSErr::ACSErrType)
 * @param ec - error code (ACSErr::ErrorCode)
 * @param ro - routine info (string)
 * @param se - error severity (ACSErr::Severity)
 * @param lab - ACE_TRY_EX label (see: http://www.cs.wustl.edu/~schmidt/ACE_wrappers/docs/exceptions.html)
 *
 */
#define THROW_ACS_EXCEPTION_EX(lab, arg...) ACE_THROW_EX ( ACS_EXCEPTION(arg), lab )

/**
 *Macro that helps retrowing ACSException
 * It can be used as:
 *  - RETHROW_ACS_EXCEPTION(lab ,eo)
 *  - RETHROW_ACS_EXCEPTION(lab, et, ec, ro)
 *  - RETHROW_ACS_EXCEPTION(lab, et, ec, ro, se)
 *  - RETHROW_ACS_EXCEPTION(lab, pex, et, ec, ro)
 *  - RETHROW_ACS_EXCEPTION(lab, pex, et, ec, ro, se)
 *
 * @param pex - previous exception (ACSException)
 * @param pex - previous exception (ACSErr::ACSException)
 * @param eo - error object (ACSError)
 * @param et - error type - group (#ACSErr::ACSErrType)
 * @param ec - error code (ACSErr::ErrorCode)
 * @param ro - routine info (string)
 * @param se - error severity (ACSErr::Severity)
 * @param lab - ACE_TRY_EX label (see: http://www.cs.wustl.edu/~schmidt/ACE_wrappers/docs/exceptions.html)
 *
 */
#define RETHROW_ACS_EXCEPTION(arg...) ACE_TRY_THROW ( ACS_EXCEPTION(arg) )

/**
 *Macro that helps retrowing ACSException
 * It can be used as:
 *  - RETHROW_ACS_EXCEPTION_EX(lab ,eo)
 *  - RETHROW_ACS_EXCEPTION_EX(lab, et, ec, ro)
 *  - RETHROW_ACS_EXCEPTION_EX(lab, et, ec, ro, se)
 *  - RETHROW_ACS_EXCEPTION_EX(lab, pex, et, ec, ro)
 *  - RETHROW_ACS_EXCEPTION_EX(lab, pex, et, ec, ro, se)
 *
 * @param pex - previous exception (ACSErr::ACSException)
 * @param eo - error object (ACSError)
 * @param et - error type - group (#ACSErr::ACSErrType)
 * @param ec - error code (ACSErr::ErrorCode)
 * @param ro - routine info (string)
 * @param se - error severity (ACSErr::Severity)
 * @param lab - ACE_TRY_EX label (see: http://www.cs.wustl.edu/~schmidt/ACE_wrappers/docs/exceptions.html)
 *
 */
#define RETHROW_ACS_EXCEPTION_EX(lab, arg...) ACE_TRY_THROW_EX ( ACS_EXCEPTION(arg), lab )

/**
 * Helper class for handling error structure (=ErrorTrace) locally in C++.
 *
 */
class ACSError {
public:

   /**
   * Constructor
   * for creating "no error" object which does not contain runtime and source  information. Instead can be used macro ACS_ERROR(). It just set error code  and type to 0 (e.g. ACSErrOK, ACSErrTypeOK).
   *\todo in case of error exception shall be thrown
   *
   */
  ACSError (); 

 /**
   * Constructor
   * for creating "no error" object which does not contain runtime and source information. Instead can be used macro ACS_ERROR(). It just set error code and type to 0 (e.g. ACSErrOK, ACSErrTypeOK). This constructor is functional equal to constructor ACSError () and it is there just that macro ACS_ERROR () can be used.   
   * @param file it is ignored
   * @param line it is ignored
   */
    ACSError (const char* file, int line); // = ACSError()

  /**
   * Constructor
   * for creating "no error" object. Instead can be used macro ACS_ERROR(ro).
   * @param file  name of the file where the error occured (should be used __FILE__ macro)
   * @param line  line number where the error occured (should be used __LINE__ macro)
   * @param routine name of the routine where the error occured
   *
   */
  ACSError (const char* file, int line, const char* routine);

  /**
   * Constructor
   * for creating error object from errortrace.
   * @param errortrace structure contains error information
   *
   */
  ACSError (const ACSErr::ErrorTrace& errortrace);

  /**
   * Constructor
   * for creating error object from errortrace.
   * @param errortrace structure contains error information
   *
   */
  ACSError (ACSErr::ErrorTrace& errortrace);

/**
   * Constructor
   * for creating error object from pointer errortrace. It makes copy of errortrace. 
   * @param file it is ignored
   * @param line it is ignored
   * @param pointer to errortrace structure contains error information
   * @param flag which indicates if errortrace shall be released - ACSError will take care of memory managment of the errortrace.
   * This constructor is functional equal to constructor ACSError (ACSErr::ErrorTrace* errortrace, bool release=1) and it is there just that macro ACS_ERROR () can be used. 
   *
   */

  ACSError (const char* file, int line, ACSErr::ErrorTrace* errortrace, bool release=1);

  /**
   * Constructor
   * for creating error object from pointer errortrace. It makes copy of errortrace. 
   * @param pointer to errortrace structure contains error information
   * @param flag which indicates if errortrace shall be released - ACSError will take care of memory managment of the errortrace.
   *
   */

  ACSError (ACSErr::ErrorTrace* errortrace, bool release=1);

  /**
   * Constructor
   * for creating  error object. Macro ACS_ERROR(et, ec, ro) should be used instead.
   * @param file  name of the file where the error occured (should be used __FILE__ macro)
   * @param line  line number where error the occured (should be used __LINE__ macro)
   * @param et error type (group)
   * @param ec error code
   * @param routine name of the routine where the error occured
   * @param severity severity of error (default medium = DEFAULT_SEVERITY)
   *
   */
  ACSError (const char* file, int line, ACSErr::ACSErrType et, ACSErr::ErrorCode ec, 
	    const char* routine, ACSErr::Severity severity=DEFAULT_SEVERITY);

  /**
   * Constructor
   * for creating error object which contains also trace to previous error(s). Macro ACS_ERROR(perr, et, ec, ro) should be used instead.
   * @param file  name of the file where the error occured (should be used __FILE__ macro)
   * @param line  line number where error the occured (should be used __LINE__ macro)
   * @param err previos error
   * @param et error type (group)
   * @param ec error code
   * @param routine name of the routine where the error occured
   * @param severity severity of error (default medium = DEFAULT_SEVERITY)
   * @param release
   *
   */
    ACSError (const char* file, int line, ACSError &err, ACSErr::ACSErrType et, ACSErr::ErrorCode ec, 
	      const char *routine, ACSErr::Severity severity=DEFAULT_SEVERITY);

  /**
   * Constructor
   * for creating error object which contains also trace to previous error(s). Macro ACS_ERROR(perr, et, ec, ro) should be used instead.
   * @param file  name of the file where the error occured (should be used __FILE__ macro)
   * @param line  line number where error the occured (should be used __LINE__ macro)
   * @param err previos error
   * @param et error type (group)
   * @param ec error code
   * @param routine name of the routine where the error occured
   * @param severity severity of error (default medium = DEFAULT_SEVERITY)
   * @param release
   *
   */
  ACSError (const char* file, int line, ACSError *err, ACSErr::ACSErrType et, ACSErr::ErrorCode ec, 
	    const char *routine, ACSErr::Severity severity=DEFAULT_SEVERITY, bool release=1);
  /**
   * Constructor
   * for creating error object from IDL exception (=ACSException).
   * @param reference to the exception object  contains error information i.e. ErrorTrace 
   *
   */
  ACSError (ACSErr::ACSException& exception);

  /**
   * Constructor
   * for creating error object which contains also trace to previous error(s) from ACSException object. Macro ACS_ERROR(pe, ed, en, ro) should be used instead.
   * @param file  name of the file where the error occured (should be used __FILE__ macro)
   * @param line  line number where error the occured (should be used __LINE__ macro)
   * @param pex reference to previos exception
   * @param et error type
   * @param ec error code
   * @param routine name of the routine where the error occured
   * @param severity severity of error (default medium = DEFAULT_SEVERITY)
   */
  ACSError (const char* file, int line, ACSErr::ACSException &pex, ACSErr::ACSErrType et,
	    ACSErr::ErrorCode ec, const char *routine,
	    ACSErr::Severity severity=DEFAULT_SEVERITY);

 /**
   * Constructor
   * for creating error object which contains also trace to previous error(s) from errortrace structure (ErrorTrace). Macro ACS_ERROR(pe, ed, en, ro) should be used instead.
   * @param file  name of the file where the error occured (should be used __FILE__ macro)
   * @param line  line number where error the occured (should be used __LINE__ macro)
   * @param pc reference to previos error contained in structure ErrorTrace
   * @param et error type (group)
   * @param ec error code
   * @param routine name of the routine where the error occured
   * @param severity severity of error (default medium = DEFAULT_SEVERITY)
   */
  ACSError (const char* file, int line, ACSErr::ErrorTrace &pc, ACSErr::ACSErrType et,
	    ACSErr::ErrorCode ec, const char *routine,
	    ACSErr::Severity severity=DEFAULT_SEVERITY);

  /**
   * Destructor
   */
  ~ACSError();
  
  /**
   * Gets reference to errortrace structure. User must take care for makeing copy of structure or should use method returnErrorTrace 
   * @return reference to the current errortrace structure
   *\todo if current is NULL than exception should be thrown
   */
   ACSErr::ErrorTrace& getErrorTrace(){ return *current; } // should check if stack is empty
 
  /**
   * Returns copy of errortrace structure (and delete ACSError object)
   * @param deletion flag indicates if ACSError has to be deleted. Default value is true what means that ACSError object will be deleted.
   * @return pointer to the errortrace structure
   */
  ACSErr::ErrorTrace* returnErrorTrace (bool deletion=true);
  
  /**
   * Moves to the next errortrace element and return its pointer (not copy).
   * If there is no next errortrace element NULL will be returned
   * @return pointer to the errortrace structure
   */
  ACSErr::ErrorTrace *getNext();
  
  /**
   * Moves to the first (top) errortrace element and return its pointer (not copy).
   * @return pointer to the errortrace structure
   */
  ACSErr::ErrorTrace *top(){ current = &errorTrace; return current;}

  /**
   * Logs errortrace information into logging system
   */
  void log();

  /**
   * Returns true if errortrace does not represent error otherwise false.
   * @return boolean flag
   */
  bool isOK(){ return (current->errorCode==ACSErrOK && current->errorType==ACSErrTypeOK); }

  /**
   * Adds data to the current error
   * @param name data name
   * @param value string data value 
   */
  void addData (const char* name, const char* value);
    
   /**
    * Adds data (pair of name-value)
    * @param name data name
    * @param value data value
    *
    */
  template<class T>
  void addData (const char* name, T value){
      const char *s;
      std::ostringstream ostr;
      ostr << value << std::ends;
      std::string ts=ostr.str(); // we have to make a temporary string otherwise there is problem with memory:  s = ostr.str().c_str(); does not work
      s = ts.c_str();
      addData (name, s);
  }

  /**
   * Returns copy of description of current error. User have to take care of releasing it!
   * @return pointer to the char (=string)
   */
  char* getDescription();

  /**
    * Returns description of the error
    * @param et error type (group)
    * @param ec error code
    */
  static char* getDescription (ACSErr::ACSErrType et, ACSErr::ErrorCode ec);

  /**
   * Returns file name information of the error
   * @return pointer to char contains file name
   */
  char* getFileName(){ return CORBA::string_dup(current->file); }

  /**
   * Returns line number information of the error
   * @return CORBA::ULong line number
   */
  CORBA::ULong getLineNumber(){ return current->lineNum; }

  /**
   * Returns routine information of tehe error
   * @return pointer to char contains routine name
   */
  char* getRoutine(){ return CORBA::string_dup (current->routine); }

  /**
   * Returns host name information of the error
   * @return pointer to char contains host name
   */
  char* getHostName(){ return CORBA::string_dup (current->host); }

  /**
   * Retruns process information of the error. Its name or process ID.
   * @return pointer to the char points to the copy of process information
   */
  char* getProcess(){ return CORBA::string_dup (current->process); }

  /**
   * Returns thread information of the error. The name of thread or its ID.
   * @return pointer to the char points to the copy of thread information
   */
  char* getThread(){ return CORBA::string_dup (current->thread); }

  /**
   * Returns time stamp of the error in 100th of nanoseconds.
   * @return CORBA::ULongLong time stamp
   */
  CORBA::ULongLong getTimeStamp (){ return current->timeStamp; }

  /**
   * Returns error code
   * @return  ACSErr::ErrorCode
   */
    ACSErr::ErrorCode getErrorCode(){ return current->errorCode; }
//      CORBA::Long t; current->error >>= t; return t; }

  /**
   * Returns error type
   * @return  ACSErr::ACSErrType
   */
    ACSErr::ACSErrType getErrorType(){ return current->errorType; }

  /**
   *Returns error severity
   */
  ACSErr::Severity getSeverity() { return current->severity; }

 /**
   *Returns depth of error stack 
   *@return unsigned int depth
   */
  unsigned int getDepth(){ return depth; }

 /**
   * Sets time stamp of the error in 100th of nanoseconds.
   * @parm time CORBA::ULongLong time stamp
   */
 void setTimeStamp (CORBA::ULongLong time){ current->timeStamp = time; }

 /**
  * Sets file name
  * @param fn file name
  *
  */
  void setFileName(const char* fn){ current->file = CORBA::string_dup (fn); }

 /**
  * Sets line number
  * @param ln line number
  *
  */
  void setLineNumber (CORBA::ULong ln){ current->lineNum = ln; }//? should we delete previos one

 /**
   *Sets current error code
   * @param ty error type (should be of type ACSErr::ACSErrType defined in acserrType.idl)
   * @param ec erro code
   *
   */
  void setError (ACSErr::ACSErrType ty, ACSErr::ErrorCode ec) 
	{ current->errorType=ty; current->errorCode=ec; }

  /**
   *Sets error severity
   *@param: severity errro severity
   *
   */
  void setSeverity(ACSErr::Severity severity) {current->severity = severity; }

  /**
   * Sets host name
   *@param hn host name
   */
  static void hostName (const char* hn);
  
  /**
   * @deprecated
   * Sets process name
   * @param process name
   */
    static void processName (const char *pn); 


   /**
   * Sets process name
   * @param process name
   */
  static void setProcessName (const char *pn); 


  static ACSErr::ErrorTrace createErrorTrace (const char* file, int line,
						    ACSError &er)
{
    ACE_UNUSED_ARG(file); 
    ACE_UNUSED_ARG(line); 
    return er.getErrorTrace(); 
}

  static ACSErr::ErrorTrace createErrorTrace (const char* file, int line, 
						     ACSErr::ACSException &pex, ACSErr::ACSErrType et,
						     ACSErr::ErrorCode ec, const char *routine,
						     ACSErr::Severity severity=DEFAULT_SEVERITY);

  static ACSErr::ErrorTrace createErrorTrace (const char* file, int line, 
						     ACSErr::ACSErrType et, ACSErr::ErrorCode ec,\
						     const char *routine,
						     ACSErr::Severity severity=DEFAULT_SEVERITY); 

    static bool init (int argc, char *argv[]);
    static bool init (CORBA::ORB_ptr _orb);
    static bool init ();
    static bool isInit(){ return initialized; }
    static void done();

protected:
    static bool initialized;
    
    static bool initLog (CORBA::ORB_ptr _orb);
  /**
   * Logs single error
   * @param c errortrace contains error to log
   * @param level of error in erro stack 
   */
  void log (ACSErr::ErrorTrace *c, int level);

  /**
   * Fills errortrace structure
   * @param et error type (group)
   * @param ec error code
   * @param severity severity of error
   * @param file  name of the file where the error occured
   * @param line  line number where the error occured
   * @param routine name of the routine where the error occured
   */
  void fill (ACSErr::ACSErrType et, ACSErr::ErrorCode ec, ACSErr::Severity severity, 
	     const char* file, int line, const char* routine);

  /**
   * Returns time in 100th of nano seconds needed for error time stamp
   *@return time stamp in CORBA::ULongLong (64 bits)
   */
    ACS::Time getTime();

  
  ACSErr::ErrorTrace errorTrace, *current;
  unsigned int depth;

  static CORBA::ORB_var orb;

  static std::unexpected_handler  m_oldUnexpected;
  static std::terminate_handler  m_oldTerminate;

  static const ACSErr::ACSErrType ACSErrTypeOK;   // this constant is redefined here otherwise we depend on code generated from ACSErrTypeOK
 static const ACSErr::ErrorCode ACSErrOK; // this constant is redefined here otherwise we depend on code generated from ACSErrTypeOK

};//class ACSError

#endif /*!_H*/
