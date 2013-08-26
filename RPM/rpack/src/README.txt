- SSH public key authentication
  RPM commands need to be executed as root.
  In order to make switching to root transparent, it is possible
  to use public key authentication.
  Look at the following wiki page for details:
     http://almasw.hq.eso.org/almasw/bin/view/SE/LinuxLKMOpsToolsDocs
  In short:
   * The following example demonstrates creating a keypair for almamgr.
     [almamgr@its01]$ ssh-keygen -t dsa
     Generating public/private dsa key pair.
     Enter file in which to save the key (/users/almamgr/.ssh/id_dsa):
     Enter passphrase (empty for no passphrase):
     Enter same passphrase again:
     Your identification has been saved in /users/almamgr/.ssh/id_dsa.
     Your public key has been saved in /users/almamgr/.ssh/id_dsa.pub.
     The key fingerprint is:
     1e:73:59:96:25:93:3f:8b:50:39:81:9e:e3:4a:a8:aa almamgr@its01
     [intusr@its01]$ chmod a+rx ~/.ssh
     [intusr@its01]$ chmod a+r ~/.ssh/id_dsa.pub

   * The two chmod commands above are necessary if the 
     home directory for almamgr exists on an NFS export 
     that is exported without the no_root_squash option.
   * These keyfiles are stored in the .ssh subdirectory 
     of the almamgr home directory.
     View the contents of that directory.
     The file named "id_dsa" is your private key, and "id_dsa.pub" is the public key.
   * The public key must be appended to the set of authotized keys for root:
     [root@its01]# cat ~intusr/.ssh/id_dsa.pub >> ~/.ssh/authorized_keys
