import os,sys, time, datetime, tarfile

################### Support functions ################

def getDestinationName():
    '''Return the name of the directory where netbeans
    has to be installed into
    This is the name of the directory with a trailing '/'
    
    In this version, the function returns the home directory
    but we should discuss if and how follow the ACS hyerarchy
    of INTROOT, INTLIST and ACSROOT
    '''
    homeDir = os.environ['HOME']
    if len(homeDir)==0:
        homeDir="~/"
    elif homeDir[len(homeDir)-1]!='/':
        homeDir+='/'
    return homeDir

def checkDestination(destName,remove):
    '''Check if the destination directory already exists.
    If the destName already exist and remove is false, this 
    function tries to rename the old directory to create a valid backup.
    If remove is True and destName exists, it will be deleted.
    
    Return True if the process can use destName as the destination
           for the installation
           False means that the process must abort the installation
    '''
    # Check if destdir already exists
    if not os.access(destName,os.F_OK):
        return True
    
    if remove:
        # Delete the previous installation
        print "Removing old installation:",destName
        ret = os.system("rm -rf "+destName)
        if ret!=0:
            print "Error removing",destName
            return False
    else:
        # Make a backup of the old dir
        now = datetime.datetime(2000,11,8)
        now = now.fromtimestamp(time.time())
        nowStr = "%(year)d%(month)02d%(day)02d_%(hour)02d%(minute)02d%(second)02d" % \
            { 'year': now.year, 'month': now.month, 'day': now.day, \
              'hour': now.hour, 'minute': now.minute, 'second': now.second }
        backupName = destName+".backup_"+nowStr
        print "Making a backup of",destName,"to",backupName
        try:
            os.rename(destName,backupName)
        except:
            print "Error making the backup"
            return False
    return True

######################### MAIN #######################

# The tar source is in the command line
if len(sys.argv)<2:
    print "Wrong command line"
    sys.exit(-1)
sourceTarFile=sys.argv[1]
if not os.access(sourceTarFile,os.R_OK):
    print sourceTarFile,"not found"
    sys.exit(-1)
if not tarfile.is_tarfile(sourceTarFile):
    print "Invalid tar-zip",sourceTarFile
    sys.exit(-1)

# Read some variable from the environment
homeDir = os.environ['HOME']

# Get the name of the dest folder
# Here we'll unpack the tar
installationDir = getDestinationName()

# Get the destination folder name from the tar file
mainDir = None
tarFile = tarfile.open(sourceTarFile,"r:gz")
for tarinfo in tarFile:
    if mainDir==None and tarinfo.isdir():
        mainDir = tarinfo.name
        break
destDir = installationDir+mainDir
if destDir[len(destDir)-1]=='/':
    destDir=destDir[0:len(destDir)-1]

if not checkDestination(destDir,False):
    sys.exit(-1)
    
# Uncompress the archive
for tarinfo in tarFile:
    tarFile.extract(tarinfo,installationDir)
tarFile.close()

print "\nInstalling Netbeans for ACS in",destDir

print "Done"
