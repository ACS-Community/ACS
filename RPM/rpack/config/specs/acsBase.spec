Summary:	Alma Common Software Development Kit
Name:		acsBase
Version:	%{acs_ver}
Release:	%{acs_rel}
License:	LGPL
Group:		ACS
Packager:	%{packager}
Source0:	%{name}-%{version}.tar.gz
URL:		http://www.eso.org/~almamgr/AlmaAcs/
BuildRequires:	pdksh
Requires:	pdksh
#####BuildRoot:	%{_tmppath}/%{name}-%{version}-root-%(id -u -n)

%define __check_files %{nil}

%description
Test, no description available.

%prep
%setup -q

# Preparation of a FAKE /alma/ACS-X.Y directory as a compile farm.
# Actually I only need to cleanup, since the directories are
# created by the build procedure.
TOOLS_INSTALL_DIR="$RPM_BUILD_ROOT/alma/%{group}-%{version}"
rm -rf $TOOLS_INSTALL_DIR/ACSSW $TOOLS_INSTALL_DIR/acsdata

%build
source .bash_profile.acs

# Make the bootstrap (prepare target)
make MODULES="LGPL/Kit/vlt LGPL/Kit/doc LGPL/Kit/acs LGPL/Kit/acstempl LGPL/Tools/doxygen LGPL/acsBUILD" \
clean_log checkModuleTree prepare

# Build and "install" the Kit modules
make MODULES="LGPL/Kit/vlt LGPL/Kit/doc LGPL/Kit/acs LGPL/Kit/acstempl LGPL/acsBUILD " \
update

%install

%clean

rm -rf $RPM_BUILD_ROOT/alma/ACS-%{version}/ACSSW/
rm -rf $RPM_BUILD_ROOT/alma/ACS-%{version}/acsdata

%files 
%defattr(-,-,-)
/alma/ACS-%{version}/ACSSW/
/alma/ACS-%{version}/acsdata/

%define date	%(echo `LC_ALL="C" date +"%a %b %d %Y"`)
# This changelog is "sick-defined", please chage it!
%changelog
* %{date} ACS-UTFSM Team <acs@listas.inf.utfsm.cl>
  Fixed wrong symbolic link creation
* %{date} ACS-UTFSM Team <acs@listas.inf.utfsm.cl>
  First RPM Build release
