/*
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2003 
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
*/

#include "baciPcontImpl_T.h"
#include "baciPcommonImpl_T.i"

template<ACS_P_C> 
PcontImpl<ACS_P_TL>::PcontImpl(const ACE_CString& name, BACIProperty *property_p, BACIComponent* component_p, DevIO<TM>* devIO, bool flagdeldevIO) 
{
  ACS_TRACE("baci::PcontImpl&lt;&gt;::PcontImpl");
  ACE_UNUSED_ARG(component_p);
  ACE_UNUSED_ARG(devIO);
  ACE_UNUSED_ARG(flagdeldevIO);

  // read static data
  if (readCharacteristics(property_p->getCharacteristicModel())==false) 
      {
      ACS_LOG(LM_RUNTIME_CONTEXT, "baci::PcontImpl&lt;&gt;::PcontImpl",
	      (LM_ERROR, "Failed to read static data for '%s'", name.c_str()));
      return;
      }
  ACS_DEBUG("baci::PcontImpl&lt;&gt;::PcontImpl", "Successfully created.");
}
 
template<ACS_P_C> PcontImpl<ACS_P_TL>::~PcontImpl()
{
  ACS_TRACE("baci::PcontImpl&lt;&gt;::~PcontImpl");
}

template<ACS_P_C> 
bool PcontImpl<ACS_P_TL>::readCharacteristics(CharacteristicModelImpl *model)
{

  DAONode* dao = model->getDAONode();
  if (!dao)
      return false;
  
  try
      {

      // NOTE: var is always a scalar value
#define READ_VALUE(name, var) \
      { \
      CORBA::String_var str = dao->get_string( name ); \
      std::istringstream is(str.in()); \
      (istream&) is >> var ; \
      if (!is) \
	  throw cdbErrType::WrongCDBDataTypeEx(); \
      }

      READ_VALUE("min_step", min_step_m);
      READ_VALUE("min_delta_trig", min_delta_trig_m);
      READ_VALUE("graph_min", graph_min_m);
      READ_VALUE("graph_max", graph_max_m);

#undef READ_VALUE

      return true;
      }
  catch (ACSErr::ACSbaseExImpl& ex)
      {
      ex.log();
      return false;
      }
  catch (...)
      {
      return false;
      }

}

/* ---------------------- [ CORBA interface ] ---------------------- */


template<ACS_P_C>
TS PcontImpl<ACS_P_TL>::min_delta_trigger ()
  throw (CORBA::SystemException)
{

  return CORBAMem<TS, TSM>::retn(min_delta_trig_m);
}

template<ACS_P_C> 
TS PcontImpl<ACS_P_TL>::graph_min ()
  throw (CORBA::SystemException)
{

  return CORBAMem<TS, TSM>::retn(graph_min_m);
}

template<ACS_P_C> 
TS PcontImpl<ACS_P_TL>::graph_max ()
  throw (CORBA::SystemException)
{

  return CORBAMem<TS, TSM>::retn(graph_max_m);
}

template<ACS_P_C> 
TS PcontImpl<ACS_P_TL>::min_step ()
  throw (CORBA::SystemException)
{

  return CORBAMem<TS, TSM>::retn(min_step_m);
}

/*___oOo___*/












