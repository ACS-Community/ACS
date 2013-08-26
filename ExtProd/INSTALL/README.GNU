For gcc and emacs on Linux for ACS versions older than ACS 3.0: 
we use the native version that can be found on Linux RedHat 7.2: 
gcc 2.96 and emacs 20.7.1 and the same for binutils: we do not need 
to compile them. 
On SUN: we need to do all the 3 things.

IMPORTANT: 
A SUN native compiler is not required.
Or the path to a pre-installed gcc compiler is 
specified using the 
     GCC_PATH 
environment variable or
this script will try to install an old version of gcc,
available as binary.
But this requires that the directory /vlt exist and is writable
(binaries of a gcc compiler can be only installed in
the hardcoded absolute path where they have been generated)
This is enough to start the installation process.

buildGNU: 
the verification should be updated!
