#include <vltPort.h>
static char *rcsId=(char *)"@(#) $Id: sampledCompImpl.cpp,v 1.8 2010/08/11 15:03:10 ntroncos Exp $";
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
	component_name = name;
}

void sampledCompImpl::initialize() {
	m_RWdouble_sp   = new RWdouble  ( (component_name + ":my_RWdouble").c_str(), getComponent());
	m_RWfloat_sp    = new RWfloat   ( (component_name + ":my_RWfloat").c_str(), getComponent());
	m_RWstring_sp   = new RWstring  ( (component_name + ":my_RWstring").c_str(), getComponent());
	m_RWlong_sp     = new RWlong    ( (component_name + ":my_RWlong").c_str(), getComponent());
	m_RWlongLong_sp = new RWlongLong( (component_name + ":my_RWlongLong").c_str(), getComponent());
	m_ROdouble_sp   = new ROdouble  ( (component_name + ":my_ROdouble").c_str(), getComponent());
	m_ROfloat_sp    = new ROfloat   ( (component_name + ":my_ROfloat").c_str(), getComponent());
	m_ROstring_sp   = new ROstring  ( (component_name + ":my_ROstring").c_str(), getComponent());
	m_ROlong_sp     = new ROlong    ( (component_name + ":my_ROlong").c_str(), getComponent());
	m_ROlongLong_sp = new ROlongLong( (component_name + ":my_ROlongLong").c_str(), getComponent());
}

/* Destructor */
sampledCompImpl::~sampledCompImpl()
{
	const char * _METHOD_ = (char *)"sampledCompImpl::~sampledCompImpl";
	ACS_TRACE(_METHOD_);
}

/* Properties returning */

ACS::ROdouble_ptr sampledCompImpl::my_ROdouble()
{
	if( m_ROdouble_sp == 0 ){
		return ACS::ROdouble::_nil();
	}
	ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_ROdouble_sp->getCORBAReference());
	return prop._retn();
}

ACS::RWdouble_ptr sampledCompImpl::my_RWdouble()
{
	if( m_RWdouble_sp == 0 ){
		return ACS::RWdouble::_nil();
	}
	ACS::RWdouble_var prop = ACS::RWdouble::_narrow(m_RWdouble_sp->getCORBAReference());
	return prop._retn();
}

ACS::ROfloat_ptr sampledCompImpl::my_ROfloat()
{
	if( m_ROfloat_sp == 0 ){
		return ACS::ROfloat::_nil();
	}
	ACS::ROfloat_var prop = ACS::ROfloat::_narrow(m_ROfloat_sp->getCORBAReference());
	return prop._retn();
}

ACS::RWfloat_ptr sampledCompImpl::my_RWfloat()
{
	if( m_RWfloat_sp == 0 ){
		return ACS::RWfloat::_nil();
	}
	ACS::RWfloat_var prop = ACS::RWfloat::_narrow(m_RWfloat_sp->getCORBAReference());
	return prop._retn();
}

ACS::ROstring_ptr sampledCompImpl::my_ROstring()
{
	if( m_ROstring_sp == 0 ){
		return ACS::ROstring::_nil();
	}
	ACS::ROstring_var prop = ACS::ROstring::_narrow(m_ROstring_sp->getCORBAReference());
	return prop._retn();
}

ACS::RWstring_ptr sampledCompImpl::my_RWstring()
{
	if( m_RWstring_sp == 0 ){
		return ACS::RWstring::_nil();
	}
	ACS::RWstring_var prop = ACS::RWstring::_narrow(m_RWstring_sp->getCORBAReference());
	return prop._retn();
}

ACS::ROlong_ptr sampledCompImpl::my_ROlong()
{
	if( m_ROlong_sp == 0 ){
		return ACS::ROlong::_nil();
	}
	ACS::ROlong_var prop = ACS::ROlong::_narrow(m_ROlong_sp->getCORBAReference());
	return prop._retn();
}

ACS::RWlong_ptr sampledCompImpl::my_RWlong()
{
	if( m_RWlong_sp == 0 ){
		return ACS::RWlong::_nil();
	}
	ACS::RWlong_var prop = ACS::RWlong::_narrow(m_RWlong_sp->getCORBAReference());
	return prop._retn();
}

ACS::ROlongLong_ptr sampledCompImpl::my_ROlongLong()
{
	if( m_ROlongLong_sp == 0 ){
		return ACS::ROlongLong::_nil();
	}
	ACS::ROlongLong_var prop = ACS::ROlongLong::_narrow(m_ROlongLong_sp->getCORBAReference());
	return prop._retn();
}

ACS::RWlongLong_ptr sampledCompImpl::my_RWlongLong()
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
