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

// ************************************************************************
//
// $Id: baciRegistrar.i,v 1.93 2008/06/03 09:15:41 bjeram Exp $
//
// Copyright (c) 2000 by Klemen Zagar
//
// GROUP    =  Data Structures
// AUTHOR  --- Klemen Zagar
//
// ************************************************************************

// ----------------[ Registrar: constructor/destructor ]-------------------

template<class Handle, class T>
Registrar<Handle,T>::Registrar(Handle hOffset, int nCapacity) :
  nCapacity_m(0), nSize_m(0), pData_mp(0),
  hfirst_m(0), hlast_m(0), hOffset_m(hOffset)
{
  setCapacity(nCapacity);
}

#ifndef MAKE_VXWORKS
template<class Handle, class T>
Registrar<Handle,T>::~Registrar<Handle,T>()
{
  delete[] pData_mp;
}
#endif

// -----------------------[ Registrar: capacity ]--------------------------

template<class Handle, class T>
bool Registrar<Handle,T>::setCapacity(int nCapacity)
{
  int i;

  // Account for the extra element with handle 0.
  ++nCapacity;

  // It is quite difficult to reduce the size of the registrar, because
  // doing that could invalidate some of the outstanding handles. Instead
  // of inventing science, we chose to fail if shrinking is requested.
  if(nCapacity < nCapacity_m) return false;

  // Requested capacity equals current? Do nothing.
  if(nCapacity == nCapacity_m) return true;

  // allocate memory for nCapacity elements.
  Element *pData_p = new Element[nCapacity];
  if(pData_p == 0) return false;

  // Initialize the newly added elements so that they form a doubly linked
  // list.
  for(i = nCapacity_m; i < nCapacity; ++i)
    {
      pData_p[i].hNext = Handle(i+1);
      pData_p[i].hPrev = Handle(i-1);
      pData_p[i].bFree = true;
    }

  // Copy existing elements.
  for(i = 0; i < nCapacity_m; ++i)
      pData_p[i] = pData_mp[i];

  if(nCapacity_m != 0)
  {
    // Join the newly added elements linked-list to the linked list of
    // free elements.
    pData_p[pData_p[0].hPrev].hNext = Handle(nCapacity_m);
    pData_p[nCapacity_m].hPrev = pData_p[0].hPrev;
    pData_p[nCapacity-1].hNext = 0;
    pData_p[0].hPrev = Handle(nCapacity-1);

    // Release the memory held by the old copy of the elements.
    delete[] pData_mp;
  }
  else
  {
    pData_p[0].hPrev = Handle(nCapacity-1);
    pData_p[nCapacity-1].hNext = Handle(0);
  }

  pData_mp = pData_p;

  nCapacity_m = nCapacity;

  return true;
}

// ----------------[ Registrar: allocation/deallocation ]------------------

template<class Handle, class T>
Handle Registrar<Handle,T>::allocate(Handle h)
{
  // Special case: if h == 0, then the Registrar must assign the handle.
  if(h == 0 || h == hOffset_m)
    h = pData_mp[0].hNext;
  else
    h -= hOffset_m;

  // Is the capacity exceeded? If so, double it, or assure that it just
  // accomodates the desired handle!
  if(h == 0)
    {
      if(setCapacity(2*(nCapacity_m-1))==false) return Handle(0);
      h = pData_mp[0].hNext;
    }
  else
    if(int(h) >= nCapacity_m)
      if(setCapacity(int(h))==false) return Handle(0);

  // The requested handle isn't free -- bail out.
  if(pData_mp[int(h)].bFree==false) return Handle(0);

  // Remove h-th element from the free element chain.
  pData_mp[int(pData_mp[h].hPrev)].hNext = pData_mp[int(h)].hNext;
  pData_mp[int(pData_mp[h].hNext)].hPrev = pData_mp[int(h)].hPrev;

  // If this is the first element to get allocated, remember it.
  if(hfirst_m == 0)
    hfirst_m = h;

  // Add h-th element to the end of the allocated element chain.
  pData_mp[h].hNext = 0;
  pData_mp[h].hPrev = hlast_m;

  if(hlast_m != 0)
    pData_mp[hlast_m].hNext = h;

  hlast_m = h;

  // Mark it as allocated.
  pData_mp[h].bFree = false;

  ++nSize_m;

  return Handle(h);
}

template<class Handle, class T>
Handle Registrar<Handle,T>::deallocate(Handle h)
{
  h -= hOffset_m;

  if(pData_mp[int(h)].bFree==true)
    return 0;

  Handle rv = next(h);

  if(pData_mp[int(h)].hPrev != 0)
    pData_mp[pData_mp[int(h)].hPrev].hNext = pData_mp[int(h)].hNext;
  else
    hfirst_m = pData_mp[int(h)].hNext;

  if(pData_mp[int(h)].hNext != 0)
    pData_mp[pData_mp[int(h)].hNext].hPrev = pData_mp[int(h)].hPrev;
  else
    hlast_m = pData_mp[int(h)].hPrev;

  pData_mp[int(h)].bFree = true;

  pData_mp[pData_mp[0].hPrev].hNext = h;
  pData_mp[int(h)].hPrev = pData_mp[0].hPrev;

  pData_mp[int(h)].hNext = 0;
  pData_mp[0].hPrev = h;

  --nSize_m;

  return rv;
}

template<class Handle, class T>
bool Registrar<Handle,T>::isAllocated(Handle h)
{
  if(int(h) >= nCapacity_m || int(h) <= 0)
    return false;
  return !pData_mp[int(h)].bFree;
}

// --------------------[ Registrar: miscellaneous ]------------------------

template<class Handle, class T>
int Registrar<Handle,T>::checkIntegrity()
{
  int h = 0, nFree = 0, nallocated = 0;

  do
  {
    if(h > nCapacity_m || h < 0) return 1;
    if(pData_mp[pData_mp[h].hNext].hPrev != h) return 2;
    if(pData_mp[h].bFree==false) return 3;
    h = next(h);
    ++nFree;
  } while(h != 0 && nFree < nCapacity_m);

  h = first();
  while(h != 0)
  {
    if(h > nCapacity_m || h < 0) return 1;
    if(pData_mp[h].hNext != 0 && 
       pData_mp[pData_mp[h].hNext].hPrev != h) return 2;
    if(pData_mp[h].bFree==true) return 4;
    h = next(h);
    ++nallocated;
  }

  if(nFree + nallocated != nCapacity_m || nallocated != nSize_m)
    return 5;

  return 0;
}
















