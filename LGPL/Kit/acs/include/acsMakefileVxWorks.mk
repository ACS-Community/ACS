#******************************************************************************
# E.S.O. - ALMA project
#
#
#*******************************************************************************
# ALMA - Atacama Large Millimeter Array
# Copyright (c) ESO - European Southern Observatory, 2014
# (in the framework of the ALMA collaboration).
# All rights reserved.
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#*******************************************************************************

ifeq (,$(MAKE_VXWORKS))
    $(error This file should only be imported if MAKE_VXWORKS is set)
endif # MAKE_VXWORKS

# MAKE_VXWORKS is defined
########################################################################################
###### VXWORKS DEFINITIONS #############################################################
########################################################################################

VW = /vw

# If CPU not set, select default
ifeq (,$(CPU))
    export CPU:=PPC604
endif

$(warning $(WIND_PLATFORM))
# version must match WIND_PLATFORM.
# Override WIND_ settings below based on external CPU/version not needed in alma.
override VX_VERSION:=$(strip $(subst vxworks-,,$(WIND_PLATFORM)))

ifneq ($(strip $(ACE_ROOT_VW)),)
    override ACE_ROOT=$(ACE_ROOT_VW)
else
    #  ACE_ROOT_DIR not defined, required to build ACE_ROOT for VxWorks
    ACE_ROOT=
    unexport ACE_ROOT
endif

# VX_CPU_FAMILY is strictly derived from CPU and indicates a known CPU.
# Therefore must ignore default from environment/command line.
override VX_CPU_FAMILY:=

#    ifeq ($(CPU),MC68000)

#       ifneq ($(VX_VERSION), 5.5)
#          $(eval $(call vw5.5Definitions))	
#       endif 
#    endif

#   ifeq ($(CPU),MC68040)
# 68k not needed in alma, and does not work anyway without or with wrong 
# WIND_ related environment variable replacement.

# $(warning 1 CPU=$(CPU) VX_VERSION=$(VX_VERSION) VX_CPU_FAMILY=$(VX_CPU_FAMILY))

#
# CPU must be one of the supported types:
ifeq ($(CPU),PPC604)
    VX_CPU_FAMILY = ppc
    ifeq ($(VX_VERSION), 6.7) 
        # this was taken from ALMA
        CPU_CFLAGS = -DCPU=$(CPU) -mlongcall -mcpu=604 -mcpu=604 -DTOOL=gnu
    endif	
    ifeq ($(VX_VERSION), 6.9)
        # -DINET centralized at final version/CPU independent settings
        CPU_CFLAGS= -mcpu=604 -mhard-float -D_WRS_HARDWARE_FP -mstrict-align -mlongcall -fno-implicit-fp -fno-builtin -DCPU=_VX_PPC604 -DTOOL_FAMILY=gnu -DTOOL=gnu -D_WRS_KERNEL
    endif
    # If CPU_FEATURE set, allow only altivec and only for VxWorks 6.9
    #  (supported by mvme6100 and vpf1;
    #   task context, underflow exceptions not automatically handled 
    #   by VLTSW kernels before Vx 6.9;
    #   gcc 4.3 with -O3 uses altivec automatically when profitable even 
    #   for scalar math.)
    # By not setting VX_CPU_FAMILY on mismatch force later error
    # -DCPU_VARIANT=_745x (equivalent CPU=PPC32 CPU_VARIANT=_ppc604_745x) enables
    # additional general and altivec control registers and bits, see ppc604.h
    # 7445/7455 and later have additional registers by sprg4_7.h, 7400/7410 have 
    # different altivec control registers,
    # compatibility check CPU_TYPE by vxPvrGet() against CPU_TYPE_xxx values, 
    # see BSP and ppc604.h
    # For altivec check sym altivecProbe presence and returning OK. 
    # Already lcuboot should refuse to load __ALTIVEC__ builds if not available.
    ifeq (,$(CPU_FEATURE))
        override VX_CPU_FAMILY := ppc
    else
        ifeq ($(CPU_FEATURE),altivec)
            ifeq ($(VX_VERSION), 6.9)
                override VX_CPU_FAMILY := ppc
                CPU_CFLAGS += -maltivec -mabi=altivec -DCPU_VARIANT=_745x
            endif
        endif
    endif
    #Todo!!!! PPC603 nowhere used, neither APEX nor VLTSW
    ifeq ($(CPU),PPC603)
        override VX_CPU_FAMILY := ppc
        CPU_CFLAGS = -DCPU=$(CPU) -mlongcall
    endif
    ifeq (,$(VX_CPU_FAMILY))
        #
        # force a syntax error, so that ACSMake stops on a
        $(error UNSUPPORTED CPU type)
    endif

    #
    # GNU-VXWORKS environment
    ifeq ($(VX_VERSION), 6.9)
        # centralized -fno-builtin -mstrict-align to OPTIMIZE
        # -pipe might just make the compiler faster, not creating faster code.
        # Could be applied generally, not only for TAO.
        #   Relocated to general CC setting.
        CC       = cc$(VX_CPU_FAMILY) -ansi -pipe
    endif	
    ifeq ($(VX_VERSION), 6.7)
        CC       = cc$(VX_CPU_FAMILY) -ansi -fno-builtin -mstrict-align
    endif	

    ifeq ($(VX_VERSION), 6.9)
        # centralized -fno-builtin -mstrict-align to OPTIMIZE, added -MG
        # -pipe make compiler faster, not generated code. Consistent with CC
        CXX      = cc$(VX_CPU_FAMILY) -ansi -pipe
        CCDEP    = cc$(VX_CPU_FAMILY) -MM -MG -ansi
    else
        CXX      = cc$(VX_CPU_FAMILY) -ansi -fno-builtin
        CCDEP    = cc$(VX_CPU_FAMILY) -MM -ansi -fno-builtin
    endif

    ifeq ($(VX_VERSION), 6.9)
        # -DMAKE_VXWORKS centralized after acsmakefileVxWorks.mk override, 
        # -DTOOL_FAMILY see CPU_CFLAGS, -c duplicate?
        CC_FLAGS = $(CPU_CFLAGS) -Wall
    endif

    ifeq ($(VX_VERSION), 6.7)
        CC_FLAGS = $(CPU_CFLAGS) -DTOOL_FAMILY=gnu -DMAKE_VXWORKS -Wall -c 
    endif	

    ifeq ($(VX_VERSION), 6.9)
        ifdef MAKE_NO_WARNINGS
            C_ONLY_FLAGS = -Wno-pointer-sign
            CC_FLAGS   +=  -Wno-write-strings
        endif
        ifdef MAKE_ALL_WARNINGS
            C_ONLY_FLAGS += -Wmissing-prototypes
            CC_FLAGS += -Wshadow               \
                        -Wcast-qual            \
                        -Wcast-align           \
                        -Wextra                \
                        -Wno-unused-parameter  \
                        -Wpointer-arith        \
                        -Wwrite-strings        \
                        -Wmissing-declarations
        endif
    else
        # all warnings left out for the default (see above) must be added here
        ifdef MAKE_ALL_WARNINGS
            CC_FLAGS   := $(CC_FLAGS) -Wshadow     \
                        -Wcast-qual            \
                        -Wcast-align           \
#                        -W                     
                        -Wno-unused            \
                        -Wpointer-arith        \
                        -Wwrite-strings        
#                        -Wmissing-prototypes   \
#                        -Wmissing-declarations 
        endif
    endif
endif

LD       = ld$(VX_CPU_FAMILY)
AR       = ar$(VX_CPU_FAMILY)
RANLIB   = ranlib$(VX_CPU_FAMILY)
NM       = nm$(VX_CPU_FAMILY)

# Todo: CPU test not really necessary.
# If not set would have bailed out above on not having set VX_CPU_FAMILY
ifdef CPU
     BINDIR   = bin/$(CPU)
     LIBDIR   = lib/$(CPU)
else
    #
    # force a syntax error, so that ACSMake stops
    $(error CPU not defined (required when MAKE_VXWORKS is defined))
endif

ifneq ($(VX_VERSION), 6.9)
    # -r is actually a linker flag, not needed for compilation
    C_PLUS_PLUS_FLAGS = -r
else
    # -fcheck-new unnecessary, because C++ raises an exception if new would return NULL.
    # However keep for now. Relocated from ACE/TAO to not apply for C (e.g. ctdt.c)
    C_PLUS_PLUS_FLAGS =-fcheck-new
endif

# Todo: In ALMA/Apex ACS_GXX_4_OR_BETTER=1 is always fullfilled! 
# Only one reference - should remove legacy branch
CXX_FOR_VERSION_TEST := cc$(VX_CPU_FAMILY)

ACS_CXX_VERSION := $(shell $(CXX_FOR_VERSION_TEST) -dumpversion) 
ifeq (cmd,$(findstring cmd,$(SHELL))) 
    ACS_CXX_MAJOR_VERSION := $(shell $(CXX_FOR_VERSION_TEST) -dumpversion | sed -e "s/[^0-9\.]//g" | sed -e "s/\..*$$//") 
else 
    ACS_CXX_MAJOR_VERSION := $(shell $(CXX_FOR_VERSION_TEST) -dumpversion | sed -e 's/[^0-9\.]//g' | sed -e 's/\..*$$//') 
endif 
ifeq ($(findstring $(ACS_CXX_MAJOR_VERSION),1 2 3),$(ACS_CXX_MAJOR_VERSION)) 
   ACS_GXX_4_OR_BETTER := 0
else 
   ACS_GXX_4_OR_BETTER := 1
endif  

# For VxWorks 6.9 use new OPTIMIZE/DEBUG settings, otherwise keep legacy.
ifeq (6.9,$(VX_VERSION))
    OPTIMIZE_VXWORKS=on
endif

ifneq (,$(OPTIMIZE_VXWORKS))
    # Actually optimization/debug options have more requirements as just -O
    # and are CPU dependend.
    # So far define only PPC604 and differentiate only up to OPTIMIZE=3
    # -O4 and above has the same effect as -O3,
    # and OPTIMIZE=4 and above makes a difference only if it sets additional options beyond -O3.
    # -g does not affect code generation, except that it sets also -O0 if not overridden.
    # -O is equivalent to -O1
    # traditionally in VLT/ALMA/APEX, if OPTIMIZE is given without a value, it defaults to -O1
    # if OPTIMIZE was not given, on Linux it was defaulting to -O1, and on VxWorks to -O0
    # if OPTIMIZE was not given, -g was always set.
    # On VxWorks additional care -g is needed (recompiling and keep source visible) to be useful

    # auto inlining and auto altivec use is done only with -O3
    # -fvolatile only helps if an application is polling for a change of
    #  a value at a pointer testination without any call to an external function in between.
    # -O0 only for hardware drivers on PowerPC reduces the likelyhood
    #  that a bug in I/O synchronization handling has an effect.
    # So setting -O3 as default could be considered.

    CC_OPTIMIZE_CPU_0 = -O0 -fvolatile
    CC_OPTIMIZE_CPU_1 = -O1 -fstrength-reduce
    CC_OPTIMIZE_CPU_2 = -O2 -fstrength-reduce
    CC_OPTIMIZE_CPU_3 = -O3 -fstrength-reduce

    # For now keep default to optimization 0, and optimization enabled with default/unknown value to 1

    O_LEVEL=$(CC_OPTIMIZE_CPU_0)
    ifneq (,$(OPTIMIZE))
        O_LEVEL=$(CC_OPTIMIZE_CPU_1)
        ifneq (,$(filter 0 1 2 3,$(OPTIMIZE)))
            O_LEVEL = $(CC_OPTIMIZE_CPU_$(OPTIMIZE))
        endif
        ifneq (,$(filter 4 5 6 7 8 9,$(OPTIMIZE)))
            O_LEVEL = $(CC_OPTIMIZE_CPU_3)
        endif
    endif

    ifndef DEBUG
        CFLAGS = $(CC_FLAGS) $(O_LEVEL)
    else
        CFLAGS = $(CC_FLAGS) -DDEBUG -g $(O_LEVEL)
    endif

else #not OPTIMIZE_VXWORKS, legacy treat VxWorks like Linux

    # the user can ask one out of: optimized/non optimized/debuggable code
    ifdef DEBUG
        CFLAGS = $(CC_FLAGS) -g3 -ggdb3 -DDEBUG                
    endif

    ifdef OPTIMIZE
        O_LEVEL = -O
        ifeq ($(OPTIMIZE),0)
            O_LEVEL = -O0
        endif
        ifeq ($(OPTIMIZE),1)
            O_LEVEL = -O1
        endif
        ifeq ($(OPTIMIZE),2)
            O_LEVEL = -O2
        endif
        ifeq ($(OPTIMIZE),3)
            O_LEVEL = -O3
        endif
        ifeq ($(OPTIMIZE),4)
            O_LEVEL = -O4
        endif
        ifeq ($(OPTIMIZE),5)
            O_LEVEL = -O5
        endif
        ifeq ($(OPTIMIZE),6)
            O_LEVEL = -O6
        endif
        ifeq ($(OPTIMIZE),7)
            O_LEVEL = -O7
        endif
        ifeq ($(OPTIMIZE),8)
            O_LEVEL = -O8
        endif
        ifeq ($(OPTIMIZE),9)
            O_LEVEL = -O9
        endif
        ifdef DEBUG
            CFLAGS = $(CC_FLAGS) -g -DDEBUG $(O_LEVEL)
        else
            CFLAGS = $(CC_FLAGS) $(O_LEVEL)
        endif
    endif

    # default case, used during development, depends on OS
    ifndef DEBUG
        ifndef OPTIMIZE
            ifdef MAKE_VXWORKS
                CFLAGS = $(CC_FLAGS) -g -DDEBUG 
            else
                CFLAGS = $(CC_FLAGS) -g -DDEBUG -O
            endif
        endif
    endif

endif # OPTIMIZE_VXWORKS

# VxWorks specific settings.
ifeq ($(strip $(VX_VERSION)),6.9)
    # !!! The VX_IPNET settings are VxWorks version and CPU specific. Verify on change!!!
    # In principle available since VxWorks 6.5
    # However do not packport to Apex VxWorks 6.7 and PPC604 beyond VxWorks 6.9 is impossible.
    # Note: On using with filename_CFLAGS, the options are not seen in the project wide settings
    #   with make all PRINTFCONFIG=on or make printConfig
    # For using ifLib.h zbufLib.h zbufSockLib.h and other backwards compatibility headers
    # or for (primarily meant for kernel internal use) new ip*.h APIs
    # How to find out the correct settings:
    # Goto a VxWorks kernel configurator created project for the correct CPU/version
    # Manually edit the autogenerated Makefile.mk (or Makefile, only without Workbench compliant conversion).
    # Add a line
    # .PRECIOUS : ./ipcom_ipdomain
    # Build the kernel
    # Now a file default/ipcom_ipdomain has survived
    # This contains equivalent settings to the following
    # settings for Internal/unavailable/notSelected subcomponents can be removed.
    # IPNET abd IP_PORT_VXWORKS might not be included in ipcom_ipdomain.
    #
    # !!! VX_IPNET must be defined before CFLAGS assignment with ":=",
    # because this kill lazy evaluation of Macros referenced in Module Makefile.
    ifeq ($(strip $(CPU)),PPC604)
        VX_IPNET_CFLAGS_ARCH = -DIPARCH_powerpc -DIPCOM_USE_ASM -DIP_BIG_ENDIAN
    else
        $(error VX_IPNET_CFLAGS and VX_IPNET_INC not supported in acsMakefile for CPU=$(CPU) VX_VERSION=$(VX_VERSION). Need to review!)
    endif # PPC604
    VX_IPNET_CFLAGS = $(VX_IPNET_CFLAGS_ARCH) \
         -DIPNET -DIPDHCPC -DIPDHCPR -DIPDHCPS -DIPDNSC -DIPFTPC -DIPFTPS \
         -DIPMCRYPTO -DIPPPP -DIPRIP -DIPSNTP -DIPTCP -DIPTFTPC -DIPTFTPS \
         -DVXCOMPAT -DVXCOREIP -DVXMUX \
         -DIP_PORT_VXWORKS=69
    VX_IPNET_INC_PREFIX=-I$(WIND_COMPONENTS)/$(COMP_IPNET2)/
    VX_IPNET_INC = \
         $(VX_IPNET_INC_PREFIX)ipappl/config \
         $(VX_IPNET_INC_PREFIX)ipappl/include \
         $(VX_IPNET_INC_PREFIX)ipcom/config \
         $(VX_IPNET_INC_PREFIX)ipcom/include \
         $(VX_IPNET_INC_PREFIX)ipcom/port/vxworks/config \
         $(VX_IPNET_INC_PREFIX)ipcom/port/vxworks/include \
         $(VX_IPNET_INC_PREFIX)ipdhcpr/config \
         $(VX_IPNET_INC_PREFIX)ipdhcpr/include \
         $(VX_IPNET_INC_PREFIX)ipdhcps/config \
         $(VX_IPNET_INC_PREFIX)ipdhcps/include \
         $(VX_IPNET_INC_PREFIX)ipmcrypto/config \
         $(VX_IPNET_INC_PREFIX)ipmcrypto/include \
         $(VX_IPNET_INC_PREFIX)ipnet2/config \
         $(VX_IPNET_INC_PREFIX)ipnet2/include \
         $(VX_IPNET_INC_PREFIX)ipppp/config \
         $(VX_IPNET_INC_PREFIX)ipppp/include \
         $(VX_IPNET_INC_PREFIX)iprip/config \
         $(VX_IPNET_INC_PREFIX)iprip/include \
         $(VX_IPNET_INC_PREFIX)ipsntp/config \
         $(VX_IPNET_INC_PREFIX)ipsntp/include \
         $(VX_IPNET_INC_PREFIX)iptcp/config \
         $(VX_IPNET_INC_PREFIX)iptcp/include \
         $(VX_IPNET_INC_PREFIX)vxcompat/config \
         $(VX_IPNET_INC_PREFIX)vxcompat/include \
         $(VX_IPNET_INC_PREFIX)vxcoreip/config \
         $(VX_IPNET_INC_PREFIX)vxcoreip/include \
         $(VX_IPNET_INC_PREFIX)vxmux/config \
         $(VX_IPNET_INC_PREFIX)vxmux/include
endif # vx6.9

# VX_IPNET not needed until vx 6.4, ignore for 6.7, not clarified for others.
ifeq (,$(findstring $(strip $(VX_VERSION)),5.5 6.4 6.7 6.9))
    $(error VX_IPNET_CFLAGS and VX_IPNET_INC not supported in acsMakefile for VX_VERSION=$(VX_VERSION). Need to review!)
endif

# Only if MAKE_VXWORKS_IPNET defined, apply to all object files.
# Otherwise only if VX_IPNET_CFLAGS/VX_IPNET_INC explicitely referenced in filename_CLFLAGS.
ifneq (,$(MAKE_VXWORKS_IPNET))
    VX_IPNET_CFLAGS_ALLOBJECTS=$(VX_IPNET_CFLAGS)
    VX_IPNET_INC_ALLOBJECTS=$(VX_IPNET_INC)
endif

# Similar options valid also for older VxWorks versions,
# but in Apex for vx6.7 keep legacy values which are empty here.
ifeq ($(strip $(VX_VERSION)),6.9)
   # -I- compiler flag not supported. Workaround see VLTSW2013+

    # By convention VLTSW wants to pass e.g. MAKE_VXWORKS to the sourcecode,
    # But this is not related to platform specific CFLAGS above.

    # version specific IP_PORT_VXWORKS needed only with VX_IPNET_CFLAGS_ALLOBJECTS
    # and now included within.
    # VX_IPNET_CFLAGS and VX_IPNET_INC maybe part of CFGLAGS by file specific objectname_CFLAGS.

    # For VxWorks 6.5 and newer always select support for ipv4 (-DINET) or 
    # ipv6(-DINET6) or both (-DINET -DINET6)
    # This affects available definitions and sizes of datastructures (e.g. sockunion.h)
    # and must match the kernel configuration.
    # We use the prebuilt kernel libraries (optional since vx6.7) which do not support 
    # inet6only and we do not select -inet6 on kernel project creation.

    CFLAGS := $(CFLAGS) -DMAKE_VXWORKS $(VX_IPNET_CFLAGS_ALLOBJECTS) -DINET

endif # VxWorks 6.9

#
#___oOo___

