# $Id: platform_macros.GNU,v 1.11 2009/10/12 07:38:04 bjeram Exp $
# VxWorks 6.7

debug ?= 0
optimize ?= 1
threads ?= 1
exceptions ?= 1
rtp ?= 0
pthread ?= 0
xerces ?= 1
shared_libs ?= 0
static_libs ?= 1

# we have to set this here otherwise we get in trouble because we set debug to 0
# (see also wrapper_macros.GNU)
CPPFLAGS += -DACE_USE_RCSID=1

ifeq ($(VXWORKS_VERSION_FLAG),)
  VXWORKS_VERSION_FLAG = -DACE_VXWORKS=0x670
endif # VXWORKS_VERSION_FLAG

include $(ACE_ROOT)/include/makeinclude/platform_vxworks6.6.GNU

ifeq ("$(WIND_HOST_TYPE)","sun4-solaris2")
  override RM=rm -f
endif # sun4-solaris2

# Disable strict aliasing warnings, these are given on some VxWorks system
# header files and has been reported to WindRiver
ifeq ("$(TOOL_FAMILY)","gnu")
  CPPFLAGS += -fno-strict-aliasing
endif

