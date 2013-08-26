Summary:	ACS/TAO Tools for ACS
Name:		acsTao
Version:	%{acs_ver}
Release:	%{acs_rel}
License:	LGPL
Group:		ACS
Packager:	%{packager}
Source0:	%{name}-%{version}.tar.gz
URL:		http://www.eso.org/~almamgr/AlmaAcs/
BuildRequires:  acsGnu
AutoReqProv:    no
#BuildRoot:	%{_tmppath}/%{name}-%{version}-root-%(id -u -n)

%define __check_files %{nil}

%description
Test, nothing yet.

%prep
%setup -q

# Preparation of a FAKE /alma/ACS-X.Y directory as a compile farm.
TOOLS_INSTALL_DIR="$RPM_BUILD_ROOT/alma/%{group}-%{version}/TAO"
rm -rf $TOOLS_INSTALL_DIR
mkdir -p $TOOLS_INSTALL_DIR

%build
# Load the Environment vars.
ALMASW_ROOTDIR=$RPM_BUILD_ROOT/alma/
ALMASW_RELEASE=%{group}-%{version}
ACE_ROOT=$RPM_BUILD_ROOT/alma/%{group}-%{version}/TAO/ACE_wrappers/build/linux
source .bash_profile.acs

# Make the bootstrap (prepare target)
cd INSTALL/
./buildTAO

%clean

rm -rf $RPM_BUILD_ROOT/alma/%{group}-%{version}/TAO/

%files 
%defattr(-,-,-)
/alma/%{group}-%{version}/TAO/

%define date	%(echo `LC_ALL="C" date +"%a %b %d %Y"`)
# This changelog is "sick-defined", please chage it!
%changelog
* %{date} ACS-UTFSM Team <acs@listas.inf.utfsm.cl>
  First RPM Build release


