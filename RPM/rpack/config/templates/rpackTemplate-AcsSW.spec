Summary:	ACS_PACKAGE_SUMMARY ACS_PACKAGE_PNAME
Name:		ACS_PACKAGE_NAME
Version:	ACS_VERSION
Release:	ACS_RELEASE
License:	LGPL
Group:		ACS
Packager:	ACS-UTFSM Team <acs@listas.inf.utfsm.cl>       
Source0:	%{name}-%{version}.tar.gz
URL:		http://www.eso.org/~almamgr/AlmaAcs/
BuildRequires:	ACS_PACKAGE_DEPENDENCIES
Requires:	ACS_PACKAGE_REQUIREMENTS
BuildRoot:	%{_tmppath}/%{name}-%{version}-root-%(id -u -n)
AutoReqProv:	no

%description
ACS_PACKAGE_DESCRIPTION

%prep
%setup -q

%build
# Load Environment Vars.
source LGPL/acsBUILD/config/.acs/.bash_profile.acs

# Defines a INTROOT for a clean compilation
export INTROOT="$RPM_BUILD_ROOT/alma/ACS-%{version}/ACSSW"
export ACSDATA="$RPM_BUILD_ROOT/alma/ACS-%{version}/acsdata"
mkdir -p $INTROOT

# Make the INTROOT directory structure
export ACS_BASE_DIRECTORY="/alma/ACS-%{version}/ACSSW"
$ACS_BASE_DIRECTORY/bin/getTemplateForDirectory INTROOT $INTROOT
$ACS_BASE_DIRECTORY/bin/getTemplateForDirectory ACSDATA $ACSDATA

# Exports some paths (and LD paths) for dependency 
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH$INTROOT/lib:
export PATH=$INTROOT/bin:$PATH

# Build TOO much modules
make MODULES=ACS_PACKAGE_MODULES checkModuleTree update

# Removes the logs from the instalation
rm $INTROOT/vltMakeInstall.config -rf
rm $INTROOT/vltMakeInstall.log -rf

%install

%clean
rm -rf $RPM_BUILD_ROOT

%files 
%defattr(-,almamgr,alma,-)
/alma/ACS-%{version}/ACSSW/
/alma/ACS-%{version}/acsdata/

%define date	%(echo `LC_ALL="C" date +"%a %b %d %Y"`)
%changelog
* %{date} ACS-UTFSM Team <acs@listas.inf.utfsm.cl>
  First RPM Build release
