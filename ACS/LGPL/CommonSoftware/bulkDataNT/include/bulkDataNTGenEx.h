#ifndef BULK_DATA_NT_EX_H
#define BULK_DATA_NT_EX_H
/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) AUI - Associated Universities Inc., 2013
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************
 * 
 * "@(#) $Id: bulkDataNTGenEx.h,v 1.1 2013/02/11 18:37:33 rbourtem Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * jpisano   2007-12-20  created
 */

//
// System stuff
//
#include <string>

//
// ACS stuff
//
#include "acserrC.h"

//
// ICD stuff
//
//#include <CorrErr.h>

//
// it throws a BDNTEx exception with the passed text
//
#define BDNT_EX(ex, format, ...)                                        \
    do {                                                                \
        char dummy[256];                                                \
        snprintf(dummy, 255, format, ##__VA_ARGS__);                    \
        ex = BDNTEx(std::string(dummy), std::string(__FILE__), __LINE__); \
    }while(0)
#define BDNT_EX_THROW_EX(format, ...)                                   \
    do {                                                                \
        char dummy[256];                                                \
        snprintf(dummy, 255, format, ##__VA_ARGS__);                    \
        throw BDNTEx(std::string(dummy), std::string(__FILE__), __LINE__); \
    }while(0)

//
// declares a BDNTCompletion<T> variable in a compact way.
//
/*#define BDNT_COMPLETION_EX(type, var, format, ...)                      \
    char _dummy[256];                                                   \
    snprintf(_dummy, 255, format, ##__VA_ARGS__);                       \
    BDNTCompletion<CorrErr::type ## Completion> var(__FILE__, __LINE__, __func__, _dummy)
#define BDNT_COMPLETION_STACK_EX(type, var, ex, format, ...)            \
    char _dummy[256];                                                   \
    snprintf(_dummy, 255, format, ##__VA_ARGS__);                       \
    BDNTCompletion<CorrErr::type ## Completion> var(ex, __FILE__, __LINE__, __func__, _dummy)
*/

//
// helper macro: catches a BDNTEx, logs the message and continues
//
#define BDNT_EX_CATCH_AND_LOG(call, format, ...)                        \
    do {                                                                \
    try                                                                 \
    {                                                                   \
        call;                                                           \
    }                                                                   \
    catch ( BDNTEx &ex )                                                \
    {                                                                   \
        char dummy[256];                                                \
        snprintf(dummy, 255, format, ##__VA_ARGS__);                    \
        ACS_SHORT_LOG((LM_ERROR, "%s", ex.asString().c_str()));         \
        ACS_SHORT_LOG((LM_ERROR, "%s", dummy));                         \
    }                                                                   \
    catch ( ... )                                                       \
    {                                                                   \
        char dummy[256];                                                \
        snprintf(dummy, 255, format, ##__VA_ARGS__);                    \
        ACS_SHORT_LOG((LM_ERROR, "unknown exception on " #call));       \
        ACS_SHORT_LOG((LM_ERROR, "%s", dummy));                         \
    }                                                                   \
    } while(0)

/** This class defines a generic C++ exception to use with non-CORBA classes
 */

class BDNTEx
{
public:
    /** Default constructor.
     */
    BDNTEx():
        m_isError(false)
	{}

    /** Parameterized constructor defines a message, filename & linenumber
     ** @param pMsg The error message - usually the cause of the exception
     ** @param pFileName The filename location of the error  -- use C macro __FILE__
     ** @param lineNumber The line number of the file -- use C macro __LINE__
     */
    BDNTEx(const std::string &msg, const std::string &fileName, int lineNumber):
	m_errMsg(msg),
        m_errFileName(fileName),
        m_errLineNumber(lineNumber),
        m_isError(true)
	{}

    /** Parameterized constructor defines an error code  message, filename
     ** and linenumber. The error code is assumed to be a code in CorrErrC.h,
     ** which would then be used for creating a corresponding Impl exception
     ** that the client could use for throwing a CORBA exception to a remote
     ** invokation of some related interface method.
     ** @param pMsg The error message - usually the cause of the exception
     ** @param pFileName The filename location of the error  -- use C macro __FILE__
     ** @param lineNumber The line number of the file -- use C macro __LINE__
     */
    BDNTEx(ACSErr::ErrorCode errorCode,
           const std::string &msg,
           const std::string &fileName,
           int lineNumber):
        m_errorCode(errorCode),
        m_errMsg(msg),
        m_errFileName(fileName),
        m_errLineNumber(lineNumber),
        m_isError(true)
    {}

    /** returns a string containing the full possible description of the exception
     */
    std::string asString() const;

    /** returns a string containing the full possible description of the exception
     */
    const char *asCString() const;

    /** Get the error message of the exception
     */
    void getMessage(std::string &msg) const
    {
        msg = m_errMsg;
    }
    
    /** Get the file name where the exception occurred.
     */
    void getFileName(std::string &fileName) const
    {
        fileName = m_errFileName;
    }

    /** Get the line number where the exception occurred.
     */
    int getLineNumber() const
    {
        return m_errLineNumber;
    }

    /** Get error flag.
     */
    bool isError() const
    {
        return m_isError;
    }

protected:
    ACSErr::ErrorCode m_errorCode;
    std::string m_errMsg;
    std::string m_errFileName;
    int m_errLineNumber;
    bool m_isError;
};

/** An ACS completion class that let's compact its instantiation.
 */
template<class T>
class BDNTCompletion: public T
{
public:

    //
    // default constructor just to make possible declaring instances without
    // context information.
    //
    BDNTCompletion():
        T("", -1, "")
    {
    }

    BDNTCompletion(const char *fileName, int lineNumber, const char *funcName, const char *cause):
        T(fileName, lineNumber, funcName)
    {
        T::addData("Cause", cause);
    }

    BDNTCompletion(const ACSErr::ErrorTrace &et, const char *fileName, int lineNumber, const char *funcName, const char *cause):
        T(et, fileName, lineNumber, funcName)
    {
        T::addData("Cause", cause);
    }

    std::string getCause()
    {
        if ( T::isErrorFree() )
        {
            return "";
        }

        return std::string(T::getErrorTraceHelper()->getData("Cause").c_str());
    }

    BDNTCompletion<T> & operator=(BDNTCompletion<T> &other)
    {
        if ( this != &other )
        {
            T::operator=(other);
        }

        return *this;
    }
};

#endif

/*___oOo___*/
