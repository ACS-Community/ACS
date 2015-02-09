//* -*- C++ -*- */
// $Id: config-vxworks6.7.h 84971 2009-03-25 13:03:44Z johnnyw $

// The following configuration file is designed to work for VxWorks
// 6.7 platforms using one of these compilers:
// 1) The GNU g++ compiler that is shipped with VxWorks 6.7
// 2) The Diab compiler that is shipped with VxWorks 6.7

#ifndef ACE_CONFIG_VXWORKS_6_7_H
#define ACE_CONFIG_VXWORKS_6_7_H
#include /**/ "ace/pre.h"

#define ACE_NEEDS_HUGE_THREAD_STACKSIZE 102400
#define ACE_USE_RCSID 1 //added for ACS
#define ACE_LACKS_RAND_R    // rand_r exists just for rtp (rand for both)
#define ACE_LACKS_STD_WSTRING 

#if !defined (ACE_VXWORKS)
# define ACE_VXWORKS 0x670
#endif /* ! ACE_VXWORKS */

#include "ace/config-vxworks6.6.h"

#undef ACE_HAS_NONCONST_INET_ADDR

#include /**/ "ace/post.h"
#endif /* ACE_CONFIG_VXWORKS_6_7_H */

