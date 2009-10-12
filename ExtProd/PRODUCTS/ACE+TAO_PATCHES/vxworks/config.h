//* -*- C++ -*- */
// $Id: config.h,v 1.9 2009/10/12 07:38:04 bjeram Exp $

// The following configuration file is designed to work for VxWorks
// 6.6 platforms using one of these compilers:
// 1) The GNU g++ compiler that is shipped with VxWorks 6.6
// 2) The Diab compiler that is shipped with VxWorks 6.6

#ifndef ACE_CONFIG_VXWORKS_6_7_H
#define ACE_CONFIG_VXWORKS_6_7_H
#include /**/ "ace/pre.h"

#define ACE_NEEDS_HUGE_THREAD_STACKSIZE 102400
#define ACE_USE_RCSID 1

#if !defined (ACE_VXWORKS)
# define ACE_VXWORKS 0x670
#endif /* ! ACE_VXWORKS */

#include "ace/config-vxworks6.6.h"

#if defined (ACE_HAS_PENTIUM)
# define ACE_LACKS_LOG2
#endif

#if !defined (__RTP__)
# undef ACE_HAS_IOCTL_INT_3_PARAM
#endif

#include /**/ "ace/post.h"
#endif /* ACE_CONFIG_VXWORKS_6_7_H */

