#! /bin/ksh
#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: acsBUILDBeforeInstall.sh,v 1.130 2005/04/05 08:58:32 gchiozzi Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# psivera  2003-02-19 link for seq* are made only if $VLTROOT does not exist
# gchiozzi 2002-12-12 Added link for seqSh and seqWish
# gchiozzi 2002-12-12 Added copying echo script in ACSROOT
# psivera  2002-10-25 added creation of ACSROOT which was forgotten
# psivera  2002-10-24 both ERRFILE and LOGFILE are written
# psivera  2002-10-23 added case ACSROOT=INSTROOT
# almamgr  2001-05-28 Renamed module acsBUILD.
#

echo "$SEPARATOR"
echo "Checking ACS environment . . ."

#
# evaluateAcsVersionNow
#
# This function evaluates the ACS 
# version currently installed, if any
# and if not already set.
#
# Values are store in the variables:
#   ACS_VERSION_NOW 
#   ACS_PATCH_LEVEL_NOW 
#   ACS_TAG_NOW
#
function evaluateAcsVersionNow {

   if [ "X$ACS_VERSION_NOW" = X ]
   then
      if [ -e $ACSROOT/ACS_VERSION ]
      then
         ACS_VERSION_NOW=`cat $ACSROOT/ACS_VERSION`
      else
         ACS_VERSION_NOW="UNKNOWN"
      fi
      
      if [ -e $ACSROOT/ACS_PATCH_LEVEL ]
      then
         ACS_PATCH_LEVEL_NOW=`cat $ACSROOT/ACS_PATCH_LEVEL`
      else
         ACS_PATCH_LEVEL_NOW="UNKNOWN"
      fi
      
      if [ -e $ACSROOT/ACS_TAG ]
      then
         ACS_TAG_NOW=`cat $ACSROOT/ACS_TAG`
         case "$ACS_TAG_NOW" in
           WARNING*) ACS_TAG_NOW="UNKNOWN";;
         esac
      else
         ACS_TAG_NOW="UNKNOWN"
      fi
   fi

echo "===== AcsVersionNow"
echo "ACS_VERSION_NOW = $ACS_VERSION_NOW"
echo "ACS_PATCH_LEVEL_NOW = $ACS_PATCH_LEVEL_NOW"
echo "ACS_TAG_NOW = $ACS_TAG_NOW"
} # end evaluateAcsVersionNow

#
# evaluateAcsrootBackupFileNames
#
# This function evaluates the proper file names
# for the backup of ACSROOT directory
# by chekcing for the version files in the current
# installation.
#
# The file names are stored in the variable
# ACSROOT_BACKUP
#

function evaluateAcsrootBackupFileNames {

   evaluateAcsVersionNow

   ACSROOT_BACKUP=$ACSROOT.$ACS_VERSION_NOW.$ACS_PATCH_LEVEL_NOW
   if [ -e $ACSROOT_BACKUP ]
   then
       if [ "$ACS_TAG_NOW" != "UNKNOWN" ]
       then
           ACSROOT_BACKUP=$ACSROOT.$ACS_TAG_NOW
       fi
       if [ -e $ACSROOT_BACKUP ]
       then
           ACSROOT_BACKUP=$ACSROOT.$$
       fi
   fi

} # end evaluateAcsrootBackupFileNames

function evaluateAcsdataBackupFileNames {

   evaluateAcsVersionNow

   ACSDATA_BACKUP=$ACSDATA.$ACS_VERSION_NOW.$ACS_PATCH_LEVEL_NOW
   if [ -e $ACSDATA_BACKUP ]
   then
       if [ "$ACS_TAG_NOW" != "UNKNOWN" ]
       then
           ACSDATA_BACKUP=$ACSDATA.$ACS_TAG_NOW
       fi
       if [ -e $ACSDATA_BACKUP ]
       then
           ACSDATA_BACKUP=$ACSDATA.$$
       fi
   fi

} # end evaluateAcsdataBackupFileNames

#
# Before any change to the current installation,
# evaluates what is really installed.
#
evaluateAcsVersionNow

#
# Checks for the ACE/TAO installation
#
if [ "X$ACE_ROOT" = X ]
then
    echo "    . . . \$ACE_ROOT is not defined."
    USE_ACE=no
    exit 1
else
    if [ -d "$ACE_ROOT" ]
    then
        echo "    . . . \$ACE_ROOT is defined as $ACE_ROOT."
        USE_ACE=yes
    else
        echo "    ACE_ROOT is defined as $ACE_ROOT but it is not a directory."
        echo "    Please fix it. Installation cannot be done. "
        exit 1
    fi
fi

#
# current compiler
#
gcc -v

TCLVER=`tcl -c "puts [ info tclversion ] "`
echo "    . . . tcl version: " $TCLVER

#
# Checks first if the installation has to be done in an
# $INTROOT or in the $ACSROOT
#
# If $INTROOT is defined
#    
echo "$SEPARATOR"
echo ""

# ACSROOT *must* be present
if [ "X$ACSROOT" = "X" ]
then
    echo " "
    echo "*** ACSROOT is not defined. !!!"
    echo " "
    exit 1
fi

# INTROOT definition
if [ "X$INTROOT" = "X" ] && [ "X$INTLIST" != "X" ] 
then
   INTROOT=`echo $INTLIST | awk 'BEGIN {FS=":"} {print $1}'` 
fi

if [ "$INTROOT" != "" ]
then 
    echo ""
    echo "WARNING: \$INTROOT is defined as $INTROOT"
    echo "         This may affect the installation procedure."
fi

if [ "X$INTROOT" != "X" ] && [ "X$INTROOT" != "X$ACSROOT" ]
then 
    echo "WARNING: \$INTROOT defined. " 
    echo "         Installing in $INTROOT. " 
    if [ -d "$INTROOT" ]
	then
	    echo "    INTROOT directory structure already exists." 
    elif [ -f "$INTROOT" ]
	then
	    echo "    \$INTROOT is defined as $INTROOT but it is not a directory." 
	    echo "    Please remove it. Installation cannot be done. " 
	    exit 1
    else
        echo "creating INTROOT=$INTROOT ..."
        sh Kit/acstempl/src/getTemplateForDirectory INTROOT $INTROOT
    fi 
elif [ "X$INTROOT" = "X$ACSROOT" ]
then 
        echo "WARNING: \$INTROOT defined identical to \$ACSROOT. " 
        if [ -d "$ACSROOT" ]
        then
            echo "WARNING: \$ACSROOT is defined as $ACSROOT - already exists." 
            evaluateAcsrootBackupFileNames
            echo "         It will be moved in $ACSROOT_BACKUP"
            mv $ACSROOT $ACSROOT_BACKUP
        elif [ -f "$ACSROOT" ]
        then
            echo "    \$ACSROOT is defined as $ACSROOT but it is not a directory." 
            echo "    Please remove it. Installation cannot be done. " 
            exit 1
        fi 
	touch $ACSROOT > /dev/null 2>&1
	if [ "$?" != 0 ]
	then
	    echo "Cannot create $ACSROOT!!!!" 
	    echo "Please check the permissions of the directory where" 
	    echo "you want to install the software and start again the build" 
	    exit 5
	else
	    rm $ACSROOT
	fi
        echo "creating ACSROOT=$INTROOT ..."
        sh Kit/acstempl/src/getTemplateForDirectory ACSROOT $ACSROOT
	NEW_ACSROOT=1
#
# Otherwise installs in $ACSROOT
#    
else
    if [ "X$ACSROOT" = X ]
    then 
	echo "ERROR: \$ACSROOT not defined. " 
	exit 1
    else
	if [ -d "$ACSROOT" ]
	then
            echo "    WARNING: ACSROOT is defined as $ACSROOT - already exists." 
            evaluateAcsrootBackupFileNames
            echo "    It will be moved in $ACSROOT_BACKUP" 
            mv $ACSROOT $ACSROOT_BACKUP
	elif [ -f "$ACSROOT" ]
	then
	    echo "    ACSROOT is defined as $ACSROOT but it is not a directory." 
	    echo "    Please remove it. Installation cannot be done. " 
	    exit 1
	fi
    fi 
    
    echo "creating ACSROOT=$ACSROOT ..."
    sh Kit/acstempl/src/getTemplateForDirectory ACSROOT $ACSROOT
    NEW_ACSROOT=1
     
fi


if [ "$NEW_ACSROOT" = "1" ]
then
    echo "$SEPARATOR"
    echo "Configure new ACSROOT"
    build_OS=`uname -s`
    if [ "$build_OS" = "Linux" ]
    then
      echo "Linux OS ..."
      echo "... creating links for panel and echo commands"
      # The $VLTROOT/bin/panel interferes with the gnome panel. As in the path 
      # the ACSROOT comes before the VLTROOT and the VLTROOT panel is not used,
      # used, we just add the following link to 
      # be sure that always the gnome panel is used.
      ln -s /usr/bin/panel $ACSROOT/bin/panel

      # On Linux we also need to replace the echo command with
      # a script that passes always the -e option to the original echo
      cp acsBUILD/src/echo $ACSROOT/bin
      chmod +x $ACSROOT/bin/echo
    fi

    # When CCS is not available, we need to replace seqSh and
    # seqWish with the original TCL shells.
    # TODO: to be cleaned up
    if [ -f "$ACSROOT/bin/seqSh" ] || [ -h "$ACSROOT/bin/seqSh" ] || [ -f "$VLTROOT/bin/seqSh" ]
    then
          echo "... seqSh already available."
    else
          echo "... Replacing seqSh and SeqWish with standard tcl shells" 
          TCL=`which tcl 2> /dev/null`
          if [ "$?" = "1" ]
          then         
             echo "    tcl shell not found"
          else
             echo "    creating link from $TCL to $ACSROOT/seqSh"
             ln -s $TCL $ACSROOT/bin/seqSh
          fi
          WISH=`which wish 2> /dev/null`
          if [ "$?" = "1" ]
          then         
             echo "    tcl wish shell not found"
          else
             echo "    creating link from $WISH to $ACSROOT/seqWish"
             ln -s $WISH $ACSROOT/bin/seqWish
          fi
      fi
fi

#
# Creates a new ACSDATA
#
echo "$SEPARATOR"
echo "Create new ACSDATA"
if [ "X$ACSDATA" = X ]
    then 
    echo "ERROR: \$ACSDATA not defined. " 
    exit 1
else
    if [ -d "$ACSDATA" ]
	then
	echo "    WARNING: ACSDATA is defined as $ACSDATA - already exists." 
        evaluateAcsdataBackupFileNames
	echo "    It will be moved in $ACSDATA_BACKUP" 
	mv $ACSDATA $ACSDATA_BACKUP
    elif [ -f "$ACSDATA" ]
	then
	echo "    ACSDATA is defined as $ACSDATA but it is not a directory." 
	echo "    Please remove it. Installation cannot be done. " 
	exit 1
    fi
  
    echo "creating ACSDATA=$ACSDATA ..."
    sh Kit/acstempl/src/getTemplateForDirectory ACSDATA $ACSDATA
fi

#
# Dump complete environment
#
echo "$SEPARATOR"
echo "Dump complete user environment for reference . . ."
env
echo " . . . Before Install completed"
echo "$SEPARATOR"

# commented out. Why was this here?
# exit 0

#
#___oOo___
