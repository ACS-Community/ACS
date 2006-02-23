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
* "@(#) $Id: baciSmartPropertyPointer.i,v 1.5 2005/01/15 00:20:38 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2004-04-06  created 
*/

#include "vltPort.h"

#include <baciCharacteristicComponentImpl.h>
#include <baciPropertyImpl.h>

using namespace baci;

template<class T>
SmartPropertyPointer<T>::SmartPropertyPointer(CharacteristicComponentImpl* component_p):
    SmartServantPointer<T>(),
    component_mp(component_p)
{
}

template<class T>
SmartPropertyPointer<T>::SmartPropertyPointer():
	SmartServantPointer<T>()
{
}

template<class T>
SmartPropertyPointer<T>::SmartPropertyPointer(T* prop,CharacteristicComponentImpl* component_p):
    SmartServantPointer<T>(prop),
    component_mp(component_p)
{
    init();
}

template<class T>  
SmartPropertyPointer<T>::~SmartPropertyPointer()
{
}

template<class T>
void SmartPropertyPointer<T>::init() 
{
    if (GetImpl(*this)==NULL || component_mp==NULL) {
	return;
    } else {
	component_mp->addPropertyToDesc(ACS::Property::_narrow((*this)->getCORBAReference()));
    }
}

template<class T>
SmartPropertyPointer<T>* SmartPropertyPointer<T>::operator=(const T* prop) {
    Reset(*this,(T*)prop);
    init();
    return this;
}

/*___oOo___*/
