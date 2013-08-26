#ifndef _ACSERR__H_
#define _ACSERR__H_
/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: acserr.h,v 1.81 2010/05/31 09:36:51 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almamgr 2004-03-02 added last() to ErrorTraceHelper
* almamgr 2004-03-02 added isErrorFree, log and getErrorTraceHelper to CompletionImpl
* bjeram 2003-07-07 added std:: prefix (gcc 3.2.x)
* bjeram 2003-03-06 added strstream #include
1* bjeram 2003-03-06 added ACSErr prefix to types defined in the idl files
* bjeram 2002-06-05 added setTimeStamp
* bjeram 2002-06-05 added ACSError (const char* file, int line, ACSError &err, ACSErrType et, ACSErr::ErrorCode ec, const char *routine, ACSErr::Severity severity);
* bjeram 2002-06-04 moved getDescription(type, code) to public and made it static
* bjeram 2002-06-04 fixed ML in addData<T>
* bjeram 2002-02-13 added ACSError() and ACS_ERROR() for creating no-error object w/o runtime and source info
* bjeram 2001-10-18 overloaded ACS_EXCEPTION and (RE)THROW_ACS_EXCEPTIONS(_EX) macros
* bjeram 2001-10-18 added RETHROW_ACS_EXCEPTION_EX
* bjeram 2001-10-18 added  THROW_ACS_EXCEPTION_EX
* bjeram 2001-10-18 overloaded ACS_ERROR macro
* almamgr  20/06/01  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif


#include "acserrLegacy.h"
#include <sstream>

// forward declaration
class ACSLogImpl;

namespace ACSErr
{

/**
 * Helper class for CORBA error trace (#ACSErr::ErrorTrace)
 *
 */

class ErrorTraceHelper
{
  protected:
    friend class CompletionImpl;
    friend class ::ACSLogImpl;
    friend class ::ACSError;

    ErrorTraceHelper(ACSErr::ErrorTrace &et);

// for OK cases and wrapping error completion
     void setErrorTrace(ACSErr::ErrorTrace &et, int depth);

// default constructor (OK completion)
    ErrorTraceHelper()
	: m_errorTracePtr(0), m_current(0), m_depth(0)
	{}

// create new error trace
    ErrorTraceHelper (ACSErr::ACSErrType et, ACSErr::ErrorCode ec,
		      const char* file, int line, const char* routine, const char* sd,
		      ACSErr::Severity severity,
		      ACSErr::ErrorTrace &errortrace);

// adding previos error trace
    ErrorTraceHelper (const ACSErr::ErrorTrace &pet,
		      ACSErr::ACSErrType et, ACSErr::ErrorCode ec,
		      const char* file, int line, const char* routine, const char* sd, 
		      ACSErr::Severity severity,
		      ACSErr::ErrorTrace &errortrace);

    /**
     * assigning ACSErr::ErrorTrace directly
     */
    ErrorTraceHelper& operator=(ACSErr::ErrorTrace& eth);

  public:


    /**
     * Logs the complete error trace into the logging system.
     * For each such exception in the trace, a separate log entry 
     * is created, in accordance with the ACS error logging specification.
     * @param priorty: priorty with which the error trace will be logged
     * If priorty is not specified the completion/error trace will 
     * be logged with ERROR priorty.
     * The priority can be used to log an important error trace at LM_DEBUG level
     * before sending it to a remote client.
     * The responsibility of ultimately decide is an error trace 
     * shall be logged or closed is ultimately left to the higher level client,
     * but a bad behaving client could "forget" to log.
     * In order to make sure potentially critical information is not lost,
     * the servant application can decide to log anyway the error trace
     * as debug information before sending it to the client.
     */
    void log(ACE_Log_Priority priorty=LM_ERROR);

    /**
     * Returns a string describing the error on top of the error trace.  
     */
    std::string toString();

    /**
     * Adds data to the current error trace.
     * If the name apperas more than one time it adds it more times.
     * @param name data name, which should be shorter than 255 characters otherwise is truncated. If name is NULL the method return w/o setting anything.
     * @param value string data value , which should be shorter than 255 characters, otherwise is truncated. If value is NULL the method return w/o setting anything.
     */
    void addData (const char* name, const char* value);
    
    /**
     * Variation of addData for non cons char* value, otherwise the template versioin is taken.
     * It just pass work to addData(const char* name, const char* value).
     */
    void addData (const char* name, char* value)
        {
            addData (name, (const char*)value);
        }//addData

    /**
     * Templated version of #addData adds data (pair of name-value)
     * It works for all types that can be put to std::ostringstream.
     * @param name data name, which should be shorter than 255 characters otherwise is truncated. If name is NULL the method return w/o setting anything.
     * @param value data value. The stringified length of value should not exceed 255 characters, otherwise it is truncated. If value is NULL the method return w/o setting anything.
     *
     */
    template<typename T>
    void addData (const char* name, T value)
	{
	    const char *s;
	    std::ostringstream ostr;
	    ostr << value << std::ends;
	    std::string ts=ostr.str(); // we have to make a temporary string otherwise there is problem with memory:  s = ostr.str().c_str(); does not work
	    s = ts.c_str();
	    addData (name, s);
	}//addData

    /**
     * Sets member value of current error. This method is mainly used by generated code.
     * If this function is used the fist time it adds new name-value pair. 
     * @param name data name. If name or value are NULL it returns w/o setting anything.
     * @param value string data value 
     */
    void setMemberValue (const char* name, const char* value);

    /**
     * Version of setMember that takes value as ACE_CString. This method is mainly used by generated code.
     * If this function is used the fist time it adds new name-value pair. 
     * @param name data name. If name is NULL it returns w/o setting anything.
     * @param value string data value 
     */
    void setMemberValue (const char* name, ACE_CString &value);

    /**
     * Sets member value. Template version of #setMemberValue. This method is mainly used by generated code.
     * It works for all types that can be put to std::ostringstream like: int, string and double.
     * @param name member name. If name is NULL it returns w/o setting anything.
     * @param value member value  
     */
    template<typename T>
    void setMemberValue (const char* name, T value)
	{ 
	    const char *s;
	    if (name==NULL) return;

	    std::ostringstream ostr;
	    ostr << value << std::ends;
	    std::string ts=ostr.str(); // we have to make a temporary string otherwise there is problem with memory
	    s = ts.c_str();
	    setMemberValue (name, s);
	}//setMemberValue

    /**
     * Gets data value for data name (first occurrence). If data for name is not found it returns an empty string.
     * @param name data name. If name is NULL an empty string is returned.
     * @return value string data value 
     */
    ACE_CString getData (const char* name);

    /**
     * Gets data value for data name (first occurrence). This method is mainly used by generated code.
     * If data for name is not found it returns an empty string.
     * @param name data name
     * @param value string data value 
     */
    void getMemberValue(const char* name, char*& value)
	{
	    value = CORBA::string_dup(getData(name).c_str());
	}
    
    /**
     * Template version of #getMemberValue. Gets data value for data name (first occurrence).
     * It works for all types that can be retreived from  std::istringstream like: int, string and double.
     * If data name is not found it returns empty string.
     * @param name data name
     * @param value data value 
     */
    template<typename T>
    void getMemberValue (const char* name, T &value)
	{
	    std::istringstream istr(getData(name).c_str());
	    istr >> value;
	}//getMemberValue

    /**
     * Another version of getMemberValue, which is more elegant since it returns value. This function is mainly used by generated code.
     * It works for all types that can be retreived from  std::istringstream like: int, string and double.
     * @param name data name
     * @return data vlaue of right type
     */
    template<typename T>
    T getMemberValue (const char* name);

    /**
     * Returns copy of short (!) description of current error. User have to take care of releasing it!
     * @return pointer to the char (=string).
     * TBD: should return description if it is possible to retreive it. To be changed or removed.
     * short description should be retreived using #getShortDescription
     */
    char* getDescription();

    /**
     * Returns copy of short description of current error. User have to take care of releasing it!
     * @return pointer to the char (=string)
     */
    char* getShortDescription();

  /**
   * Returns file name information of the error
   * @return pointer to char contains file name
   */
  char* getFileName(){ return CORBA::string_dup(m_current->file); }

  /**
   * Returns line number information of the error
   * @return CORBA::ULong line number
   */
  CORBA::ULong getLineNumber(){ return m_current->lineNum; }

  /**
   * Returns routine information of tehe error
   * @return pointer to char contains routine name
   */
  char* getRoutine(){ return CORBA::string_dup (m_current->routine); }

  /**
   * Returns host name information of the error
   * @return pointer to char contains host name
   */
  char* getHostName(){ return CORBA::string_dup (m_current->host); }

  /**
   * Retruns process information of the error. Its name or process ID.
   * @return pointer to the char points to the copy of process information
   */
  char* getProcessName(){ return CORBA::string_dup (m_current->process); }

  /**
   * Returns thread information of the error. The name of thread or its ID.
   * @return pointer to the char points to the copy of thread information
   */
  char* getThread(){ return CORBA::string_dup (m_current->thread); }

  /**
   * Returns source object information of the error.
   * @return pointer to the char points to the copy of the source object (caller has to delete it).
   */
  char* getSourceObject(){ return CORBA::string_dup (m_current->sourceObject); }
  /**
   * Returns time stamp of the error in 100th of nanoseconds.
   * @return CORBA::ULongLong time stamp
   */
  CORBA::ULongLong getTimeStamp (){ return m_current->timeStamp; }

  /**
   * Returns error code
   * @return  ACSErr::ErrorCode
   */
    ACSErr::ErrorCode getErrorCode(){ return m_current->errorCode; }

  /**
   * Returns error type
   * @return  ACSErr::ACSErrType
   */
    ACSErr::ACSErrType getErrorType(){ return m_current->errorType; }

  /**
   *Returns error severity
   */
  ACSErr::Severity getSeverity() { return m_current->severity; }

 /**
   *Returns depth of error stack 
   *@return unsigned int depth
   */
  unsigned int getDepth(){ return m_depth; }

 /**
   * Sets time stamp of the error in 100th of nanoseconds.
   * @parm time CORBA::ULongLong time stamp
   */
 void setTimeStamp (CORBA::ULongLong time){ m_current->timeStamp = time; }

 /**
  * Sets source object
  * @param source object
  *
  */
  void setSourceObject(const char* so){ m_current->sourceObject = CORBA::string_dup (so); }

 /**
  * Sets file name
  * @param fn file name
  *
  */
  void setFileName(const char* fn){ m_current->file = CORBA::string_dup (fn); }

 /**
  * Sets line number
  * @param ln line number
  *
  */
  void setLineNumber (CORBA::ULong ln){ m_current->lineNum = ln; }//? should we delete previos one

 /**
   *Sets current error code
   * @param ty error type (should be of type ACSErr::ACSErrType defined in acserrType.idl)
   * @param ec erro code
   *
   */
  void setError (ACSErr::ACSErrType ty, ACSErr::ErrorCode ec) 
	{ m_current->errorType=ty; m_current->errorCode=ec; }

  /**
   *Sets error severity
   *@param: severity errro severity
   *
   */
  void setSeverity(ACSErr::Severity severity) {m_current->severity = severity; }

  /**
   * Sets host name
   *@param hn host name
   */
  static void setHostName (const char* hn);
  
  /**
   * Sets process name
   * @param process name
   */
  static void setProcessName (const char *pn); 

  /**
   * Moves to the next errortrace element and return its pointer (not copy).
   * If there is no next errortrace element NULL will be returned
   * @return pointer to the errortrace structure
   */
  ACSErr::ErrorTrace *getNext();

    /**
     * Tells if we are at the end of error trace. This method should be used with combination of getNext to find out when the end of the error trace is reached.
     * @return true if we reach the end of error trace otherwise false
     */
    bool last() { return (m_depth==0 || m_current->previousError.length()==0); }
  
  /**
   * Moves to the first (top) errortrace element and return its pointer (not copy).
   * @return pointer to the errortrace structure
   */
  ACSErr::ErrorTrace *top(){ m_current = m_errorTracePtr; return m_current;}

   /**
   * Gets reference to errortrace structure. User must take care for makeing copy of structure or should use method returnErrorTrace 
   * @return reference to the current errortrace structure
   *\todo if current is NULL than exception should be thrown
   */
    ACSErr::ErrorTrace& getErrorTrace(){ return *m_current; } // should check if stack is empty
    
    static ACS::Time getTime();
    
    /**
     * Return pointer to ErrorTraceHelper what is itself (=this).
     * It is here just to make generated code simpler.
     * @return pointer to itself
     */
    ErrorTraceHelper*  getErrorTraceHelper() { return this; }

  protected:

    void fill (ACSErr::ACSErrType et, ACSErr::ErrorCode ec, ACSErr::Severity severity,
	       const char* file, int line, const char* routine, const char* sd);

   /**
     * Logs just a single part (item) of an errortrace into the logging system.
     * This method is used internaly by the #log method
     * @param c pointer to error trace structure
     * @param level error trace level
     * @param priorty: priorty with which the error trace will be logged
     * If priorty is not specified the completuion/error trace will be logged with ERROR priorty.
     */
    void log (ACSErr::ErrorTrace * c,
	      int level, char *stackId,
	      ACE_Log_Priority priorty=LM_ERROR);

    void toString (ACSErr::ErrorTrace * c, int level, std::ostringstream& oss);

    ACSErr::ErrorTrace *m_errorTracePtr;
//& m_errorTraceRef;
    ACSErr::ErrorTrace *m_current;
    unsigned int m_depth;
    
    static char m_hostName[64];
    static char m_processName[64];
    static const unsigned int m_maxDepth;
};

/*******************************************************************************************/
/**
 * Class for initalizing #ACSErrCompletion
 */

class CompletionInit : public ACSErr::Completion
{
  public:
    CompletionInit(const ACSErr::Completion &c);

    CompletionInit(ACSErr::CompletionType t, ACSErr::CompletionCode c, bool initTrace=true);
    
  /**
   * Returns completion code
   * @return  ACSErr::CompletionCode
   */
    ACSErr::CompletionCode getCode(){ return code; }

  /**
   * Returns completion type
   * @return  ACSErr::ACSErrType
   */
    ACSErr::CompletionType getType(){ return type; }

  /**
   * Returns completion time stamp in 100th (1/100) of nanoseconds.
   * @return CORBA::ULongLong time stamp
   */
  CORBA::ULongLong getTimeStamp (){ return timeStamp; }
};




/**************************************************************************************/
/**
 * Implementation / Helper class for handling #ACSErr::Completion
 */

class CompletionImpl : public CompletionInit 
{
  public:
// default constructor
    CompletionImpl();

    CompletionImpl (ACSErr::ACSErrType t, ACSErr::ErrorCode c) :
	CompletionInit(t, c, false)/*,
	m_errorTraceHelper(previousError[0], previousError.length())
*/
	{
	}

    CompletionImpl (ACSErr::ACSErrType t, ACSErr::ErrorCode c,
		      const char* file, int line, const char* routine, const char* sd,
		      ACSErr::Severity severity) : 
	CompletionInit(t, c),
	m_errorTraceHelper(t, c, file, line, routine, sd, severity, previousError[0])
	{}

// adding previous (remote or local) with reference 
    CompletionImpl (const ACSErr::Completion &pc, ACSErr::ACSErrType t, ACSErr::ErrorCode c,
		      const char* file, int line, const char* routine, const char* sd,
		      ACSErr::Severity severity) :
	CompletionInit(t, c),
	m_errorTraceHelper(pc.previousError[0], t, c, file, line, routine, sd, severity, previousError[0])
	{}
// adding previous remote completion as pointer
    CompletionImpl (ACSErr::Completion *pc, ACSErr::ACSErrType t, ACSErr::ErrorCode c,
		      const char* file, int line, const char* routine, const char* sd,
		      ACSErr::Severity severity) :
	CompletionInit(t, c),
	m_errorTraceHelper(pc->previousError[0], t, c, file, line, routine, sd, severity, previousError[0])
	{ delete pc; }

// adding previous completion as pointer
    CompletionImpl (CompletionImpl *pc, ACSErr::ACSErrType t, ACSErr::ErrorCode c,
		      const char* file, int line, const char* routine, const char* sd,
		      ACSErr::Severity severity) :
	CompletionInit(t, c),
	m_errorTraceHelper(pc->previousError[0], t, c, file, line, routine, sd, severity, previousError[0])
	{ delete pc; }

// adding error trace
    CompletionImpl (const ACSErr::ErrorTrace &et, ACSErr::ACSErrType t, ACSErr::ErrorCode c,
		      const char* file, int line, const char* routine, const char* sd,
		      ACSErr::Severity severity) :
	CompletionInit(t, c),
	m_errorTraceHelper(et, t, c, file, line, routine, sd, severity, previousError[0])
	{}

//wrapping remote (CORBA) Completion
    /**
     * Wrapper constructor for remote (CORBA) completions (#ACSErr::Completion).
     * @param c remote CORBA completion
     * @parm del flag which indiactes if the completion should be deleted. Its default value is true, so the remote completion is deleted.
     */
    CompletionImpl(ACSErr::Completion* c, bool del=true);

   /**
     * Wrapper constructor for remote (CORBA) completions (#ACSErr::Completion) that is contained in a #Completion_var.
     * @param c reference to #Completion_var
     * #Completion from #Completion_var is copied, so after that the #Completion_var still contains the completion.
     */
    CompletionImpl(ACSErr::Completion_var& c);

    CompletionImpl (const ACSErr::Completion &c);

    /**
       Copy constructor
    */
    CompletionImpl (const CompletionImpl &c);

    /**
       Destructor
     */
    virtual ~CompletionImpl(){}
    
  /**
   * Returns copy of completion structure (and delete #CompletionImpl object).
   * This method should be used in a method when we retrun #ACSErr::Completion 
   * (when we go from local (C++) to remote (CORBA)
   * @param deletion flag indicates if #CompletionImpl has to be deleted. 
   * Default value is true what means that #CompletionImpl object will be deleted.
   * DO NOT use true (default) value when #CompletionImpl is allocated on the stack !!!
   * @return pointer to the CORBA #ACSErr::Completion structure.
   */
    ACSErr::Completion* returnCompletion (bool deletion=true)
	{
	    ACSErr::Completion *tmp = new ACSErr::Completion(*this);
	    if (deletion) delete this;
	    return tmp;
	}//returnCompletion

    /**
     * Returns copy of completion structure (#ACSErr::Completion) which can be assign to #ACSErr::Completion_out.
     * This method should be used when #ACSErrCompletion is used as out(put) parameter of a method (#ACSErr::Completion_out).
     * DO NOT use this method on client side !!
     * @param deletion flag indicates if #CompletionImpl has to be deleted. 
     * Default value is false what means that #CompletionImpl object will not be deleted 
     * (oposit than in #returnCompeltion()).
     * DO NOT use true value when #CompletionImpl is allocated on the stack !!!
     * @return pointer to the CORBA #ACSErr::Completion structure.
     */
    ACSErr::Completion* outCompletion(bool del=false) { return this->returnCompletion(del); }

    /**
     * Tells if CompletionImpl is error-free or not (i.e. if is it contain error trace or not)
     @return true if CompletionImpl is error-free otherwise false.
     */

    bool isErrorFree(){ return (previousError.length() == 0); }

  /**
   * Returns pointer to the ErrorTraceHelper. If there is no error trace it returns NULL.
   * @return pointer to the errortrace helper object
   */
    ErrorTraceHelper*  getErrorTraceHelper(){ return (ErrorTraceHelper*)( (previousError.length() > 0) ? &m_errorTraceHelper : NULL); }
    
    /**
     * Logs Completion/errortrace into the logging system
     * @param priorty priorty with which the completion/error trace will be logged
     * If priorty is not specified the completuion/error trace will be logged with ERROR priorty.
     */
    void log(ACE_Log_Priority priorty=LM_ERROR);

    template<typename T>
    void addData (const char* name, T value)
	{
	    if (!isErrorFree())
		{
		m_errorTraceHelper.addData(name, value);
		}
	}


    CompletionImpl& operator=(CompletionImpl&);

    /**
     * assignment for remote (CORBA) completions.
     * @param c pointer to the remote (CORBA) completion
     * It takes over the memory managment of #Completion, so the Completion is deleted.
     */
    CompletionImpl& operator=(Completion* c);

     /**
     * assignment for remote (CORBA) completion that is contained in a #Completion_var.
     * @param c pointer to the  #Completion_var.
     * This assigment makes a copy of #Completion that is contained inside the #Completion_var, so after the #Completion_var still conatins #Completion.
     */
    CompletionImpl& operator=(Completion_var& c);



  protected:
    ErrorTraceHelper m_errorTraceHelper;
};//CompletionImpl

/*************************************************************************************/
//template implementation
//

// specialization for strings
template<>
char * ErrorTraceHelper::getMemberValue<char*> (const char* name);

template<>
ACE_CString ErrorTraceHelper::getMemberValue<ACE_CString> (const char* name);

template<typename T>
T ErrorTraceHelper::getMemberValue (const char* name)
{
    T value;
    std::istringstream istr(getData(name).c_str());
    istr >> value;
    return value;
}//getMemberValue

}

// these two lines are just for backward compatibility and should be removed
// ... with next release
typedef ACSErr::CompletionInit CompletionInit;
typedef ACSErr::CompletionImpl CompletionImpl;
typedef ACSErr::ErrorTraceHelper ErrorTraceHelper;

#endif
