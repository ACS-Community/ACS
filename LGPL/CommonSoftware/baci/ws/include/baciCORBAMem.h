#ifndef _baciCORBAMem_H_
#define _baciCORBAMem_H_

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

/** 
 * @file 
 * Header file BACI CORBA Memory.
 */

template<class O, class I=O>
class CORBAMem {
  public:
    static O addToArry(I&v){ return v; }
    static O retn(I&v){ return v; } 
#ifndef XXXMAKE_VXWORKS
//    static O retn(I&); 
#else
//    static double retn(double& v){ return v; }
#endif //MAKE_VXWORKS
};

template<class O, class I>
class CORBAMem <O*, I>{
  public:
    static O* retn(I& v){ return new O(v);}
    static O addToArry(I& v){ return v;}
};

template<>
class CORBAMem <char*, ACE_CString>{
  public:
    static char* retn(ACE_CString& v){ return CORBA::string_dup(v.c_str()); }
    static char* addToArry(ACE_CString& v){ return CORBA::string_dup(v.c_str()); }
};

#if 0
template<class O, class I>
O CORBAMem<O, I>::retn(I& v)
{ 
    return v; 
}
/*
template<>
char* CORBAMem<char*, ACE_CString>::toHistory(ACE_CString& v);
*/
template<class O, class I>
O CORBAMem<O*, I>::toHistory(I& v)
{ 
    return v; 
}

template<class O, class I>
O* CORBAMem<O*, I>::retn(I& v)
{
    return new O(v);
}
/*
template<>
char* CORBAMem<char*, ACE_CString>::retn(ACE_CString& v);
*/


#endif //MAKE_VXWORKS

#endif
