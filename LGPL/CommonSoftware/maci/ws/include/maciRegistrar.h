#ifndef maciRegistrar_h
#define maciRegistrar_h

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
* "@(#) $Id: maciRegistrar.h,v 1.80 2011/10/28 14:45:10 hsommer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2011-10-28  created
*/

// ==========================[ class Registrar ]===========================

//
// NAME: Registrar
//
// DESCRIPTION: Data structure for maintaining a collection of elements
// that can be referred to using handles.
//
// Stores data elements in an array-like structure. Individual elements are
// addressed using handles, which can be though of as indices in the
// array. The registrar fulfills these requirements:
//
//  1.  Allocation: A new element can be added to the registrar and is
//  assigned a unique handle. Allocation is an O(1) operation.
//
//  2.  Deallocation: An element can be removed from the registrar. The
//  handle is freed, and can be assigned to another element during
//  allocation at a later time. Deallocation is an O(1) operation.
//
//  3.  Retrieval: A reference to the element can be retrieved from the
//  registrar for reading and writing. Retrieval is an O(1) operation.
//
//  4. Enumeration: Elements stored in the registrar can be traversed from
//  first to last.  Costs of acquiring first, last, next and previous
//  element of the array are O(1).
//
//  5. Unlimited storage: There is no limit other than amount of available
//  memory to store elements.
//
// The registrar data structure is suitable for enumerating resources that
// are frequently allocated, retrieved and deallocated without losing large
// amounts of memory and/or time.
//
// PARAMETERS:
//
//   Handle - Type that represents the handle.  Usually an integral type.
//            Must be castable to 0.
//
//   T      - Type of data that the registrar should store.
//
// EXAMPLE:
//
//       // A registrar that maintains strings.
//       Registrar<int, string> reg;
//       // Allocate a new string and remember its handle.
//       int h = reg.Allocate();
//       // Set string's value.
//       reg[h] = "a string";
//       // Get string's value.
//       cout << reg[h] << endl;
//       // Deallocate the string.
//       reg.Deallocate(h);
//
// INTERNAL: The registrar is essentially a doubly-linked list of elements,
// which are placed in an array.  Each element has assigned a handle (the
// index in the array), and handles of the elements that come previous and
// next to it. There are actually two chains of elements: the free element
// chain, which contains all elements that have not yet been allocated, and
// the allocated element chain. Free element chain is cyclic (passing the
// end resumes at the beginning), and contains the element with the handle
// 0. The allocated element chain is not cyclic: it starts with the element
// that was first allocated, and ends with the one that was last allocated.
//
template<class Handle, class T>
class Registrar
{
  struct Element
  {
    int  hPrev, hNext;  // Previous and next elements with the same bFree.
    T    tData;         // The actual data.
    bool bFree;         // *true* if the element is still unallocated.
  };

  int      m_nCapacity;   // Capacity  of the registrar.
  int      m_nSize;       // Number of elements currently in the registrar.
  Element *m_pData;       // Pointer to the first element in the registrar.
  Handle   m_hFirst;      // Handle of the first non-free element.
  Handle   m_hLast;       // Handle of the last non-free element.
  Handle   m_hOffset;     // Amount by which to offset all handles.

 public:
  //
  // DESCRIPTION: Construct the registrar.
  //
  // Creates a registrar and allocates enough space to hold nCapacity
  // elements.
  //
  // PARAMETERS:
  //   hOffset   - Amount by which to offset all handles.
  //   nCapacity - Initial capacity of the registrar.
  // 
  Registrar(Handle hOffset = 0, int nCapacity = 128);

  //
  // DESCRIPTION: Destruct the registar, freeing all its elements.
  //
#ifndef MAKE_VXWORKS
  ~Registrar();
#else
  ~Registrar() { delete[] m_pData; }
#endif

  //
  // DESCRIPTION: Gives a reference to the h-th element in the array.
  //
  // NOTE: No checking is performed whether an element corresponding to
  // handle h actually exists or not, n either whether h-th element is
  // actually within capacity limits of the registrar.
  //
  // PARAMETERS:
  //   h - Handle of the element to return the reference to.
  //
  // RETURN VALUE: Reference (or const-reference) to the requested element.
  //
  T& operator[](Handle h) { return m_pData[h-m_hOffset].tData; }
  const T& operator[](Handle h) const { return m_pData[h-m_hOffset].tData; }

  // ----------------------------------------------------------------------
  // GROUP = Registrar capacity
  // ----------------------------------------------------------------------

  //
  // DESCRIPTION: Sets the maximum amount of elements the registrar can
  // hold before resizing itself.
  //
  // The registar is made to hold elements with handles of up to nCapacity.
  //
  // NOTE: If nCapacity is smaller than the current capacity, the operation
  // fails.
  //
  // PARAMETERS:
  //   nCapacity - The requested capacity of the registrar.
  //
  // RETURN VALUE: If the capacity has been successfully set, a return
  // value of *true* is returned. In case of an error, such as lack of
  // memory or too small capacity, *false* is returned.
  //
  bool SetCapacity(int nCapacity);

  //
  // DESCRIPTION: Get the capacity of the registrary.
  //
  // RETURN VALUE: Returns the capacity of the registrar, i.e. the maximum
  // number of elements that the registrar can hold before resizing itself.
  //
  int  GetCapacity() { return m_nCapacity - 1; }

  //
  // DESCRIPTION: Get the number of elements currently in the registrar.
  //
  // RETURN VALUE: The number of elements currently in the registar.
  //
  int GetSize() { return m_nSize; }

  // ----------------------------------------------------------------------
  // GROUP = Allocation/Deallocation
  // ----------------------------------------------------------------------

  //
  // DESCRIPTION: Allocate an element in the registrar.
  //
  // Assures that the registrar is capable of holding yet another element
  // and returns a handle to it. If h is specified, then the function tries
  // to make the returned handle
  //
  // NOTE: Do not use too high a value for h, because Allocate assures that
  // capacity of the registar becomes at least h, resulting in a huge
  // consumption of memory.
  //
  // PARAMETERS:
  //   h - Requests the registrar to use h as the handle of the allocated
  //       element. If 0 is passed (the default), then the handle is
  //       assigned automatically by the registrar.
  //
  // RETURN VALUE: If 0 is returned, then the allocation was not
  // successful, either because the handle is already allocated, or because
  // the system ran out of memory.
  //
  // SEE ALSO: Deallocate
  //
  Handle Allocate(Handle h = 0);

  Handle Preallocate(Handle h = 0);
  Handle AckAllocate(Handle h);

  //
  // DESCRIPTION: Deallocate an element with the given handle.
  //
  // The element and its corresponding handle can be reused at a later call
  // to Allocate.
  //
  // PARAMETERS:
  //   h - The handle of the element to deallocate.
  //
  // RETURN VALUE: Returns 0 if the element is not yet allocated. In case
  // of success, the handle of the next element in the registrar is
  // returned, which can also be 0 if h represents the last element in the
  // array.
  //
  // EXAMPLE: This example shows how to deallocate all elements in the
  // array.
  //
  //       Registrar<Handle, T> reg;
  //        . . .
  //       Handle h = reg.First();
  //       while(h) h = reg.Deallocate(h);
  //
  Handle Deallocate(Handle h);

  Handle Depreallocate(Handle h);

  //
  // DESCRIPTION: Determines whether a given handle has already been
  // allocated.
  //
  // PARAMETERS:
  //   h - The handle in question.
  //
  // RETURN VALUE: Returns *true* if an element with handle h already
  // exists in the registrar and false otherwise.
  //
  bool IsAllocated(Handle h);

  // ----------------------------------------------------------------------
  // GROUP = Enumeration
  // ----------------------------------------------------------------------

  //
  // DESCRIPTION: Return the handle of the first element in the array.
  //
  // RETURN VALUE: Returns the handle of the element that was the first one
  // to get allocated and has not yet been deallocated.  Useful for
  // determining the starting point when enumerating the entire registry.
  //
  // SEE ALSO: Next, Previous
  //
  Handle First() { return m_hFirst + m_hOffset; }

  //
  // DESCRIPTION: Return the handle of the last element in the array.
  //
  // RETURN VALUE: Returns the handle of the element that was the last one
  // to get allocated and has not yet been deallocated.  Useful for
  // determining the starting point when enumerating the entire registry.
  //
  // SEE ALSO: Next, Previous
  //
  Handle Last() { return m_hLast + m_hOffset; }

  //
  // DESCRIPTION: Return the handle of the next element in the array.
  //
  // PARAMETERS:
  //   h - Handle of the element whose sucessor's handle we wish to
  //       acquire.
  //
  // RETURN VALUE: Returns the handle of the element that follows the one
  // whose handle is h. If h is the last element, 0 is returned.
  //
  Handle Next(Handle h) { return m_pData[h-m_hOffset].hNext + m_hOffset; }

  //
  // DESCRIPTION: Return the handle of the previous element in the array.
  //
  // PARAMETERS:
  //   h - Handle of the element whose predecessor's handle we
  //       wish to acquire.
  //
  // RETURN VALUE: Returns the handle of the element that precedes the one
  // whose handle is h. If h is the first element, 0 is returned.
  //
  Handle Previous(Handle h) { return m_pData[h-m_hOffset].hPrev + m_hOffset; }

  // ----------------------------------------------------------------------
  // GROUP = Miscellaneous
  // ----------------------------------------------------------------------

  //
  // DESCRIPTION: Check whether the registrar is in a consistent state.
  //
  // Performs several consistency checks on the registar.
  //
  // RETURN VALUE:
  //   0 - The registrar is  in a consistent state.
  //   1 - Doubly-linked list  exceeds the capacity  of the registrar; some
  //       of its elements are not within the array managed by the
  //       registrar.
  //   2 - Doubly-linked list is inconsistent (element's next's previous is
  //       not the element).
  //   3 - An element is  in  the free element  chain, but it  is marked as
  //       allocated.
  //   4 - An  element in the allocated element chain, but  it is marked as
  //       free.
  //   5 - The two doubly-linked  lists don't span the  entire capacity of
  //       the registrar.
  //
  int CheckIntegrity();
};

#include <maciRegistrar.i>

#endif // maciRegistrar_h

