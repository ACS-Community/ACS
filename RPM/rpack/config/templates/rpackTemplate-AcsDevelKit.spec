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

%description
ACS_PACKAGE_DESCRIPTION

%prep
%setup -q

# Preparation of a FAKE /alma/ACS-X.Y directory as a compile farm.
TOOLS_INSTALL_DIR="$RPM_BUILD_ROOT/alma/%{group}-%{version}"
rm -rf $RPM_BUILD_ROOT
mkdir -p $TOOLS_INSTALL_DIR
cd $TOOLS_INSTALL_DIR

# We asume that External Tools are installed in the following directories
ln -s /alma/%{group}-%{version}/ant    $TOOLS_INSTALL_DIR/ant
ln -s /alma/%{group}-%{version}/gnu    $TOOLS_INSTALL_DIR/gnu
ln -s /alma/%{group}-%{version}/JacORB $TOOLS_INSTALL_DIR/JacORB
ln -s /alma/%{group}-%{version}/java   $TOOLS_INSTALL_DIR/java
ln -s /alma/%{group}-%{version}/mico   $TOOLS_INSTALL_DIR/mico
ln -s /alma/%{group}-%{version}/Python $TOOLS_INSTALL_DIR/Python
ln -s /alma/%{group}-%{version}/TAO    $TOOLS_INSTALL_DIR/TAO
ln -s /alma/%{group}-%{version}/tcltk  $TOOLS_INSTALL_DIR/tcltk
ln -s /alma/%{group}-%{version}/Tomcat $TOOLS_INSTALL_DIR/Tomcat

%build
# Load the Environment vars.
source LGPL/acsBUILD/config/.acs/.bash_profile.acs

# Make the bootstrap (prepare target)
make MODULES=ACS_PACKAGE_MODULES \
clean_log checkModuleTree prepare

# Build and "install" the Kit modules
make MODULES=ACS_PACKAGE_MODULES \
update

%install
# The following commands set some symlinks to the propper files because the scripts make them with absolute paths
rm -rf $RPM_BUILD_ROOT/alma/ACS-%{version}/ACSSW/bin/seqSh
ln -s /alma/ACS-%{version}/tcltk/bin/tcl $RPM_BUILD_ROOT/alma/ACS-%{version}/ACSSW/bin/seqSh 
rm -rf $RPM_BUILD_ROOT/alma/ACS-%{version}/ACSSW/bin/seqWish 
ln -s /alma/ACS-%{version}/tcltk/bin/wish $RPM_BUILD_ROOT/alma/ACS-%{version}/ACSSW/bin/seqWish
%clean

rm -rf $RPM_BUILD_ROOT

%files 
%defattr(-,almamgr,alma,-)
/alma/ACS-%{version}/ACSSW/
/alma/ACS-%{version}/acsdata/

%define date	%(echo `LC_ALL="C" date +"%a %b %d %Y"`)
# This changelog is "sick-defined", please chage it!
%changelog
* %{date} ACS-UTFSM Team <acs@listas.inf.utfsm.cl>
  Fixed wrong symbolic link creation
* %{date} ACS-UTFSM Team <acs@listas.inf.utfsm.cl>
  First RPM Build release


