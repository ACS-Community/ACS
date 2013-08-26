Summary:	Tcl/tk Tools for ACS
Name:		acsTcltk
Version:	%{acs_ver}
Release:	%{acs_rel}
License:	LGPL
Group:		ACS
Packager:	%{packager}
Source0:	%{name}-%{version}.tar.gz
URL:		http://www.eso.org/~almamgr/AlmaAcs/
AutoReqProv:    no
#
# We do not set a BuildRoot, since we need to build in the
# standard installation place.
# But then, because of a bug in RPM, we need also
# to disable __check_files
#
# BuildRoot:	%{_tmppath}/%{name}-%{version}-root-%(id -u -n)

%define __check_files %{nil}

%description
Test, nothing yet.

%prep
%setup -q

# Preparation of a FAKE /alma/ACS-X.Y directory as a compile farm.
TOOLS_INSTALL_DIR="$RPM_BUILD_ROOT/alma/%{group}-%{version}/tcltk"
rm -rf $TOOLS_INSTALL_DIR
mkdir -p $TOOLS_INSTALL_DIR

%build
# Load the Environment vars.
# Since we build in /alma/%{group}-%{version} we do not need to
# set any special variable like TCLTK_ROOT or ACSDATA
source .bash_profile.acs

# Make the bootstrap (prepare target)
cd INSTALL/
./buildTcltk

######
###### I think all these can be removed!
######
###### link Fix
#####rm -rf $TCLTK_ROOT/bin/procheck
#####rm -rf $TCLTK_ROOT/bin/procomp
#####rm -rf $TCLTK_ROOT/bin/prodebug
#####rm -rf $TCLTK_ROOT/bin/prowrap
#####rm -rf $TCLTK_ROOT/lib/iwidgets
#####rm -rf $TCLTK_ROOT/lib/libtclx8.4.a
#####rm -rf $TCLTK_ROOT/lib/libtclx8.4.so
#####
#####cd $TCLTK_ROOT/bin/
#####ln -s ../TclPro/linux-ix86/bin/procheck procheck
#####ln -s ../TclPro/linux-ix86/bin/procomp procomp
#####ln -s ../TclPro/linux-ix86/bin/prodebug prodebug
#####ln -s ../TclPro/linux-ix86/bin/prowrap prowrap
#####
#####cd $TCLTK_ROOT/lib/
#####ln -s tclx8.4/libtclx8.4.a libtclx8.4.a
#####ln -s tclx8.4/libtclx8.4.so libtclx8.4.so


%clean

rm -rf $RPM_BUILD_ROOT/alma/%{group}-%{version}/tcltk/
rm -rf $RPM_BUILD_ROOT/alma/%{group}-%{version}/acsdata/

%files 
%defattr(-,-,-)
/alma/%{group}-%{version}/tcltk/
/alma/%{group}-%{version}/acsdata/

%define date	%(echo `LC_ALL="C" date +"%a %b %d %Y"`)
# This changelog is "sick-defined", please chage it!
%changelog
* %{date} ACS-UTFSM Team <acs@listas.inf.utfsm.cl>
  First RPM Build release


