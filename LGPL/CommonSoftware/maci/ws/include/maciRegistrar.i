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
* "@(#) $Id: maciRegistrar.i,v 1.81 2011/10/28 14:45:10 hsommer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2011-10-28  created
*/

// ----------------[ Registrar: constructor/destructor ]-------------------

template<class Handle, class T>
Registrar<Handle,T>::Registrar(Handle hOffset, int nCapacity) :
  m_nCapacity(0), m_nSize(0), m_pData(0),
  m_hFirst(0), m_hLast(0), m_hOffset(hOffset)
{
  SetCapacity(nCapacity);
}

#ifndef MAKE_VXWORKS
template<class Handle, class T>
Registrar<Handle,T>::~Registrar<Handle,T>()
{
  delete[] m_pData;
}
#endif

// -----------------------[ Registrar: capacity ]--------------------------

template<class Handle, class T>
bool Registrar<Handle,T>::SetCapacity(int nCapacity)
{
  int i;

  // Account for the extra element with handle 0.
  ++nCapacity;

  // It is quite difficult to reduce the size of the registrar, because
  // doing that could invalidate some of the outstanding handles. Instead
  // of inventing science, we chose to fail if shrinking is requested.
  if(nCapacity < m_nCapacity) return false;

  // Requested capacity equals current? Do nothing.
  if(nCapacity == m_nCapacity) return true;

  // Allocate memory for nCapacity elements.
  Element *pData = new Element[nCapacity];
  if(pData == 0) return false;

  // Initialize the newly added elements so that they form a doubly linked
  // list.
  for(i = m_nCapacity; i < nCapacity; ++i)
    {
      pData[i].hNext = Handle(i+1);
      pData[i].hPrev = Handle(i-1);
      pData[i].bFree = true;
    }

  // Copy existing elements.
  for(i = 0; i < m_nCapacity; ++i)
      pData[i] = m_pData[i];

  if(m_nCapacity != 0)
  {
    // Join the newly added elements linked-list to the linked list of
    // free elements.
    pData[pData[0].hPrev].hNext = Handle(m_nCapacity);
    pData[m_nCapacity].hPrev = pData[0].hPrev;
    pData[nCapacity-1].hNext = 0;
    pData[0].hPrev = Handle(nCapacity-1);

    // Release the memory held by the old copy of the elements.
    delete[] m_pData;
  }
  else
  {
    pData[0].hPrev = Handle(nCapacity-1);
    pData[nCapacity-1].hNext = Handle(0);
  }

  m_pData = pData;

  m_nCapacity = nCapacity;

  return true;
}

// ----------------[ Registrar: allocation/deallocation ]------------------

template<class Handle, class T>
Handle Registrar<Handle,T>::Allocate(Handle h)
{
  // Special case: if h == 0, then the Registrar must assign the handle.
  if(h == 0 || h == m_hOffset)
    h = m_pData[0].hNext;
  else
    h -= m_hOffset;

  // Is the capacity exceeded? If so, double it, or assure that it just
  // accomodates the desired handle!
  if(h == 0)
    {
      if(!SetCapacity(2*(m_nCapacity-1))) return Handle(0);
      h = m_pData[0].hNext;
    }
  else
    if(int(h) >= m_nCapacity)
      if(!SetCapacity(int(h))) return Handle(0);

  // The requested handle isn't free -- bail out.
  if(!m_pData[int(h)].bFree) return Handle(0);

  // Remove h-th element from the free element chain.
  m_pData[int(m_pData[h].hPrev)].hNext = m_pData[int(h)].hNext;
  m_pData[int(m_pData[h].hNext)].hPrev = m_pData[int(h)].hPrev;

  // If this is the first element to get allocated, remember it.
  if(m_hFirst == 0)
    m_hFirst = h;

  // Add h-th element to the end of the allocated element chain.
  m_pData[h].hNext = 0;
  m_pData[h].hPrev = m_hLast;

  if(m_hLast != 0)
    m_pData[m_hLast].hNext = h;

  m_hLast = h;

  // Mark it as allocated.
  m_pData[h].bFree = false;

  ++m_nSize;

  return Handle(h);
}

template<class Handle, class T>
Handle Registrar<Handle,T>::Deallocate(Handle h)
{
  h -= m_hOffset;

  if(m_pData[int(h)].bFree)
    return 0;

  Handle rv = Next(h);

  if(m_pData[int(h)].hPrev != 0)
    m_pData[m_pData[int(h)].hPrev].hNext = m_pData[int(h)].hNext;
  else
    m_hFirst = m_pData[int(h)].hNext;

  if(m_pData[int(h)].hNext != 0)
    m_pData[m_pData[int(h)].hNext].hPrev = m_pData[int(h)].hPrev;
  else
    m_hLast = m_pData[int(h)].hPrev;

  m_pData[int(h)].bFree = true;

  m_pData[m_pData[0].hPrev].hNext = h;
  m_pData[int(h)].hPrev = m_pData[0].hPrev;

  m_pData[int(h)].hNext = 0;
  m_pData[0].hPrev = h;

  --m_nSize;

  return rv;
}

template<class Handle, class T>
bool Registrar<Handle,T>::IsAllocated(Handle h)
{
  if(int(h) >= m_nCapacity || int(h) <= 0)
    return false;
  return !m_pData[int(h)].bFree;
}

// ----------------[ Registrar: allocation/deallocation ]------------------

template<class Handle, class T>
Handle Registrar<Handle,T>::Preallocate(Handle h)
{
  // Special case: if h == 0, then the Registrar must assign the handle.
  if(h == 0 || h == m_hOffset)
    h = m_pData[0].hNext;
  else
    h -= m_hOffset;

  // Is the capacity exceeded? If so, double it, or assure that it just
  // accomodates the desired handle!
  if(h == 0)
    {
      if(!SetCapacity(2*(m_nCapacity-1))) return Handle(0);
      h = m_pData[0].hNext;
    }
  else
    if(int(h) >= m_nCapacity)
      if(!SetCapacity(int(h))) return Handle(0);

  // The requested handle isn't free -- bail out.
  if(!m_pData[int(h)].bFree) return Handle(0);

  // Remove h-th element from the free element chain.
  m_pData[int(m_pData[h].hPrev)].hNext = m_pData[int(h)].hNext;
  m_pData[int(m_pData[h].hNext)].hPrev = m_pData[int(h)].hPrev;

  // Mark it as allocated.
  m_pData[h].bFree = false;

  ++m_nSize;

  return Handle(h);
}





// ----------------[ Registrar: allocation/deallocation ]------------------

template<class Handle, class T>
Handle Registrar<Handle,T>::AckAllocate(Handle h)
{

  // If this is the first element to get allocated, remember it.
  if(m_hFirst == 0)
    m_hFirst = h;

  // Add h-th element to the end of the allocated element chain.
  m_pData[h].hNext = 0;
  m_pData[h].hPrev = m_hLast;

  if(m_hLast != 0)
    m_pData[m_hLast].hNext = h;

  m_hLast = h;

  return Handle(h);
}




template<class Handle, class T>
Handle Registrar<Handle,T>::Depreallocate(Handle h)
{
  h -= m_hOffset;

  m_pData[int(h)].bFree = true;

  m_pData[m_pData[0].hPrev].hNext = h;
  m_pData[int(h)].hPrev = m_pData[0].hPrev;

  m_pData[int(h)].hNext = 0;
  m_pData[0].hPrev = h;

  --m_nSize;

  return h;
}


// --------------------[ Registrar: miscellaneous ]------------------------

template<class Handle, class T>
int Registrar<Handle,T>::CheckIntegrity()
{
  int h = 0, nFree = 0, nAllocated = 0;

  do
  {
    if(h > m_nCapacity || h < 0) return 1;
    if(m_pData[m_pData[h].hNext].hPrev != h) return 2;
    if(!m_pData[h].bFree) return 3;
    h = Next(h);
    ++nFree;
  } while(h != 0 && nFree < m_nCapacity);

  h = First();
  while(h != 0)
  {
    if(h > m_nCapacity || h < 0) return 1;
    if(m_pData[h].hNext != 0 && 
       m_pData[m_pData[h].hNext].hPrev != h) return 2;
    if(m_pData[h].bFree) return 4;
    h = Next(h);
    ++nAllocated;
  }

  if(nFree + nAllocated != m_nCapacity || nAllocated != m_nSize)
    return 5;

  return 0;
}


