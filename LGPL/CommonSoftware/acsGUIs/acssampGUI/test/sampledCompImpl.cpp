#include <vltPort.h>
static char *rcsId=(char *)"@(#) $Id: sampledCompImpl.cpp,v 1.2 2008/08/29 21:25:20 rtobar Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "sampledCompImpl.h"

using namespace baci;

/* Constructor */
sampledCompImpl::sampledCompImpl(const ACE_CString& name, maci::ContainerServices *containerServices) :
       CharacteristicComponentImpl(name,containerServices)
      ,m_RWdouble_sp(this)
      ,m_ROdouble_sp(this)
      ,m_RWfloat_sp(this)
      ,m_ROfloat_sp(this)
      ,m_RWstring_sp(this)
      ,m_ROstring_sp(this)
      ,m_RWlong_sp(this)
      ,m_ROlong_sp(this)
      ,m_RWlongLong_sp(this)
      ,m_ROlongLong_sp(this)
{
	const char * _METHOD_ = (char *)"sampledCompImpl::sampledCompImpl";
	ACS_TRACE(_METHOD_);
	component_name = name.c_str();
}

void sampledCompImpl::initialize() throw (acsErrTypeLifeCycle::LifeCycleExImpl){
	m_RWdouble_sp   = new RWdouble  ( (component_name + std::string(":my_RWdouble")).c_str(), getComponent());
	m_RWfloat_sp    = new RWfloat   ( (component_name + std::string(":my_RWfloat")).c_str(), getComponent());
	m_RWstring_sp   = new RWstring  ( (component_name + std::string(":my_RWstring")).c_str(), getComponent());
	m_RWlong_sp     = new RWlong    ( (component_name + std::string(":my_RWlong")).c_str(), getComponent());
	m_RWlongLong_sp = new RWlongLong( (component_name + std::string(":my_RWlongLong")).c_str(), getComponent());
	m_ROdouble_sp   = new ROdouble  ( (component_name + std::string(":my_ROdouble")).c_str(), getComponent());
	m_ROfloat_sp    = new ROfloat   ( (component_name + std::string(":my_ROfloat")).c_str(), getComponent());
	m_ROstring_sp   = new ROstring  ( (component_name + std::string(":my_ROstring")).c_str(), getComponent());
	m_ROlong_sp     = new ROlong    ( (component_name + std::string(":my_ROlong")).c_str(), getComponent());
	m_ROlongLong_sp = new ROlongLong( (component_name + std::string(":my_ROlongLong")).c_str(), getComponent());
}

/* Destructor */
sampledCompImpl::~sampledCompImpl()
{
	const char * _METHOD_ = (char *)"sampledCompImpl::~sampledCompImpl";
	ACS_TRACE(_METHOD_);
}

/* Properties returning */

ACS::ROdouble_ptr sampledCompImpl::my_ROdouble() throw (CORBA::SystemException)
{
	if( m_ROdouble_sp == 0 ){
		return ACS::ROdouble::_nil();
	}
	ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_ROdouble_sp->getCORBAReference());
	return prop._retn();
}

ACS::RWdouble_ptr sampledCompImpl::my_RWdouble() throw (CORBA::SystemException)
{
	if( m_RWdouble_sp == 0 ){
		return ACS::RWdouble::_nil();
	}
	ACS::RWdouble_var prop = ACS::RWdouble::_narrow(m_RWdouble_sp->getCORBAReference());
	return prop._retn();
}

ACS::ROfloat_ptr sampledCompImpl::my_ROfloat() throw (CORBA::SystemException)
{
	if( m_ROfloat_sp == 0 ){
		return ACS::ROfloat::_nil();
	}
	ACS::ROfloat_var prop = ACS::ROfloat::_narrow(m_ROfloat_sp->getCORBAReference());
	return prop._retn();
}

ACS::RWfloat_ptr sampledCompImpl::my_RWfloat() throw (CORBA::SystemException)
{
	if( m_RWfloat_sp == 0 ){
		return ACS::RWfloat::_nil();
	}
	ACS::RWfloat_var prop = ACS::RWfloat::_narrow(m_RWfloat_sp->getCORBAReference());
	return prop._retn();
}

ACS::ROstring_ptr sampledCompImpl::my_ROstring() throw (CORBA::SystemException)
{
	if( m_ROstring_sp == 0 ){
		return ACS::ROstring::_nil();
	}
	ACS::ROstring_var prop = ACS::ROstring::_narrow(m_ROstring_sp->getCORBAReference());
	return prop._retn();
}

ACS::RWstring_ptr sampledCompImpl::my_RWstring() throw (CORBA::SystemException)
{
	if( m_RWstring_sp == 0 ){
		return ACS::RWstring::_nil();
	}
	ACS::RWstring_var prop = ACS::RWstring::_narrow(m_RWstring_sp->getCORBAReference());
	return prop._retn();
}

ACS::ROlong_ptr sampledCompImpl::my_ROlong() throw (CORBA::SystemException)
{
	if( m_ROlong_sp == 0 ){
		return ACS::ROlong::_nil();
	}
	ACS::ROlong_var prop = ACS::ROlong::_narrow(m_ROlong_sp->getCORBAReference());
	return prop._retn();
}

ACS::RWlong_ptr sampledCompImpl::my_RWlong() throw (CORBA::SystemException)
{
	if( m_RWlong_sp == 0 ){
		return ACS::RWlong::_nil();
	}
	ACS::RWlong_var prop = ACS::RWlong::_narrow(m_RWlong_sp->getCORBAReference());
	return prop._retn();
}

ACS::ROlongLong_ptr sampledCompImpl::my_ROlongLong() throw (CORBA::SystemException)
{
	if( m_ROlongLong_sp == 0 ){
		return ACS::ROlongLong::_nil();
	}
	ACS::ROlongLong_var prop = ACS::ROlongLong::_narrow(m_ROlongLong_sp->getCORBAReference());
	return prop._retn();
}

ACS::RWlongLong_ptr sampledCompImpl::my_RWlongLong() throw (CORBA::SystemException)
{
	if( m_RWlongLong_sp == 0 ){
		return ACS::RWlongLong::_nil();
	}
	ACS::RWlongLong_var prop = ACS::RWlongLong::_narrow(m_RWlongLong_sp->getCORBAReference());
	return prop._retn();
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(sampledCompImpl)
/* ----------------------------------------------------------------*/

/*___oOo___*/
