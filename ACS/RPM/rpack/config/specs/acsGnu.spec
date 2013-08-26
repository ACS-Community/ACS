Summary:	GNU Tools for ACS
Name:		acsGnu
Version: 	%{acs_ver}
Release:	%{acs_rel}
License:	LGPL
Group:		ACS
Packager:	%{packager}
Source0:	%{name}-%{version}.tar.gz
URL:		http://www.eso.org/~almamgr/AlmaAcs/
#####BuildRoot:	%{_tmppath}/%{name}-%{version}-root-%(id -u -n)
#####BuildRoot:	/
AutoReqProv:    no

%define __check_files %{nil}

%description
Test, nothing yet.

%prep
%setup -q

# Preparation of a FAKE /alma/ACS-X.Y directory as a compile farm.
TOOLS_INSTALL_DIR="$RPM_BUILD_ROOT/alma/%{group}-%{version}/gnu"
rm -rf $TOOLS_INSTALL_DIR
mkdir -p $TOOLS_INSTALL_DIR

%build
# Load the Environment vars.
GNU_ROOT="$RPM_BUILD_ROOT/alma/%{group}-%{version}/gnu"
source .bash_profile.acs
echo $GNU_ROOT

# Make the bootstrap (prepare target)
cd INSTALL/
./buildGNU

%install

%clean

rm -rf $RPM_BUILD_ROOT/alma/%{group}-%{version}/gnu

%files 
%defattr(-,-,-)
# RPM_BUILD_ROOT defines where to look for
# the files.
# In this way it should put everything (I hope) from:
# /alma/ACS-%{version}/gnu/
/alma/%{group}-%{version}/gnu

%define date	%(echo `LC_ALL="C" date +"%a %b %d %Y"`)
# This changelog is "sick-defined", please chage it!
%changelog
* %{date} ACS-UTFSM Team <acs@listas.inf.utfsm.cl>
  First RPM Build release


