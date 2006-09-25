#!/usr/bin/env python

'''
Start the netbeans GUI for alarm
The jarfile are copied into ~/.netbeans/3.5

The script launchs netbeans  without using runide.sh because
in this way it is possible to set all the propertiess diredctly
in the command line.

The command line is always shown befor launching the GUI.
'''

import os, os.path, exceptions, sys, socket, re

####################### Useful methods #####################

def copyFile(src,dst):
    '''Copy src in dst'''
    if not os.access(src,os.R_OK):
        raise exceptions.IOError, (src+" not found")
    #Open the files
    inF = file(src,"rb")
    outF= file(dst,"w+b")
    bufSize = 1024
    done = False
    while not done:
        buf=inF.read(bufSize)
        if len(buf)>0:
            outF.write(buf)
        else:
            done = True
    outF.flush()
    inF.close()
    outF.close()

def NormalizeDirName(name):
    '''Add a trailing '/' to the dir name, if not already present'''
    if name[len(name)-1]!='/':
            name+='/'
    return name

def getNetbeansHome():
    '''Return the home directory of netbeans
    
    It can be NBPLATFORM_HOME env variable or $ALMASW_INSTDIR/netbeans
    In future it can be different'''
    if os.environ.has_key('NBPLATFORM_HOME'):
        nbHome = os.environ['NBPLATFORM_HOME']
    elif os.environ.has_key('ALMASW_INSTDIR'):
        nbHome = os.environ['ALMASW_INSTDIR']
    else:
        print "Netbeans installation not found."
        print "Install netbeans and setup NBPLATFORM_HOME or ALMASW_INSTDIR"
        print "In ACS netbeans has to be installed by almamgr"
        sys.exit(-1)
    nbHome=NormalizeDirName(nbHome)
    nbHome+='netbeans/'
    if not os.path.exists(nbHome):
        raise exceptions.IOError, 'Netbeans installation directory not found'
    return nbHome

def getJarPath(jarName,dirs):
    '''Return the full path name of the given jar searching in the dirs.
    In this way it is possible to implement the ACS hierarchy of dirs
    
    jarName: the name of the jar file
    dirs: a list of directories to look for the jar file'''
    for dir in dirs:
        dir=NormalizeDirName(dir)+"lib/"
        if os.access(dir+jarName,os.F_OK):
            return dir+jarName
    raise IOError, ("JAR file not found: "+jarName)

def getInstance(managerCORBALoc):
    '''Return the instance of ACS in use
    If managerCORBALoc is not None it means that the user passed 
    the corbaloc of the manager in the command line. in that case
    we calc the instance from the corbaloc'''
    if managerCORBALoc!=None:
        strs = managerCORBALoc.split(':')
        portStr =  strs[len(strs)-1]
        portStr = portStr[0:len(portStr)-8]
        portNum = int(portStr)
        if portNum==3000:
            return 0
        else:
            return (portNum - 3000)/100
    else:
        try:
            instance = os.environ['ACS_INSTANCE']
        except:
            # The instance is 0 if ACS_INSTANCE is not defined
            return 0
        return int(instance)

def getAllJarsInFolder(folderName):
    '''Return the list of the java jar files i.e.
    the jar files in $JAVA_HOME/lib'''
    folderName=NormalizeDirName(folderName)
    jars = []
    files=os.listdir(folderName)
    for filename in files:
        if filename.endswith(".jar"):
            jars.append(folderName+filename)
    return jars

def getFoldersForJars(introot,acsroot,intlist,jacorb):
    '''Return a list of directories that can contain jar files
    This list is ordered for the priority as stated by ACS rules:
        ../lib,INTROOT, INTLIST, ACSROOT'''
    folders = []
    if os.access(os.path.abspath("../lib"), os.F_OK):
        folders.append(NormalizeDirName(os.path.abspath("../lib")))
    if not introot == None:
        folders.append(introot)
    if not intlist == None:
        dirs = intlist.split(':')
        for dir in dirs:
            if len(dir)>0:
                folders.append(NormalizeDirName(dir))
    folders.append(acsroot)
    folders.append(jacorb)
    return folders

def getACSProps(instance,managerCORBALoc):
    '''Return the string with ACS properties taking into account the instance
    If the user set the corbaloc of the manager in the command line then
    NOTE: istnace is correct also in case of corbaloc in the command line'''
    global javaHome, acsData
    managerPort = 3000+instance*100
    nameServicePort = 3001+instance*100
    if managerCORBALoc==None:
        localIP = getLocalIP()
        managerCORBALoc= 'corbaloc::%(ip)s:%(port)d/Manager' % \
            { 'ip': localIP, 'port': managerPort }
        NSCORBALoc= 'corbaloc::%(ip)s:%(port)d/NameService' % \
            { 'ip': localIP, 'port': nameServicePort }
    else:
        # we have to cal the NSCORBALoc from the manager CORBA LOC and the given instance
        strs = managerCORBALoc.split(':')
        print strs
        NSCORBALoc="corbaloc::"+strs[2]+":"+str(nameServicePort)+"/NameService"
        
    properties = [ 
             "-Djdk.home="+javaHome, 
             "-Dorg.omg.CORBA.ORBClass=org.jacorb.orb.ORB", 
             "-Dorg.omg.CORBA.ORBSingletonClass=org.jacorb.orb.ORBSingleton",
             "-Duser.timezone=UTC", 
             "-Dabeans.home="+acsData+"config/abeans/Config", 
             "-DACS.manager="+managerCORBALoc, 
             "-DORBInitRef.NameService="+NSCORBALoc, 
             "-DACS.tmp="+acsData+"tmp/ACS_INSTANCE."+str(instance),
             "-DACS.baseport="+str(instance), 
             "-DACS.data="+acsData,
             "-Djava.util.logging.manager=alma.acs.logging.AcsLogManager" ]
             
             
    temp = ""
    for prop in properties:
        temp+=prop+" "
    return temp

def getNetbeansProps(userNBFolder):
    global netbeansHomeDir, homeDir
    # "-Dnetbeans.osenv=/tmp/nbenv.20411", \
    
    properties = [ \
                   "-Dnetbeans.user="+userNBFolder, \
                   "-Dnetbeans.osenv.nullsep=true", \
                   "-Dnetbeans.home="+netbeansHomeDir,\
                   "-Djava.security.policy="+netbeansHomeDir+"bin/ide.policy"]
    temp = ""
    for prop in properties:
        temp+=prop+" "
    return temp

def getLocalIP():
    # Return the local IP
    name = socket.gethostname()
    ip =  socket.gethostbyname(name)
    return ip

def checkNetbeansModulesFolder(userNBFolder):
    '''Check if the netbeans local folder of the user is ready
    It usually is ~/.netbeans/3.5/ and it should contain the following
    folders:
        autoload
        eager
        modules
          laser'''
    dirs = [    userNBFolder, \
                userNBFolder+"autoload", \
                userNBFolder+"eager", \
                userNBFolder+"modules", \
                userNBFolder+"modules/laser" ]
    for dir in dirs:
        if not os.access(dir,os.F_OK):
            os.makedirs(dir)
            

def cleanNetbeansModules(userNBFolder,modJars):
    '''Clean the modules folder of user netbeans folder before installing 
    the new jars
    It removes all the old jars and prepare that lasr dir (empty)'''
    checkNetbeansModulesFolder(userNBFolder)
    modulesDir = userNBFolder+"modules/"
    for jar in modJars:
        if os.access(modulesDir+jar,os.F_OK):
            os.remove(modulesDir+jar)
    laserModDir = modulesDir+"laser/"
    if os.access(laserModDir,os.F_OK):
        jars = os.listdir(laserModDir)
        for jar in jars:
            os.remove(laserModDir+jar)
    else:
        os.mkdir(laserModDir)
        
def setupNetbeansModules(userNBFolder,modJars,laserJars,dirs):
    '''Copy the jar files in the local NB folder of the user,
    i.e ~/.netbeans/3.5/modules and ~/.netbeans/3.5/modules/laser'''
    for jar in modJars:
        try:
            src = getJarPath(jar,dirs)
        except:
            print jar,"NOT found in",dirs
            print "Check your laser installation before launching the GUI"
            sys.exit(-1)
        copyFile(src,userNBFolder+"modules/"+jar)
    for jar in laserJars:
        try:
            src = getJarPath(jar,dirs)
        except:
            print jar,"NOT found in",dirs
            print "Check your laser installation before launching the GUI"
            sys.exit(-1)
        copyFile(src,userNBFolder+"modules/laser/"+jar)
        
def usage():
    '''Print the usage message'''
    print "Usage:"
    print sys.argv[0],"[-h|<manger_CORBA_loc>]"
    print "  -h: print help string"
    print "  <manger_CORBA-loc>: the CORBA loc of the manager"
    print "If the CORBA loc of the manager is not present"
    print " the script assumes the local computer as host and"
    print "instance is read from ACS_INSTANCE (0 if ACS_INSTANCE"
    print "is not defined)."
    print

####################### MAIN ###############################

# The list of ACS jar files required to run the GUI
acsRequiredJars= [ \
           "jACSUtil.jar", \
           "acserrj.jar", \
           "acsjlog.jar", \
           "concurrent.jar", \
           "backport-util-concurrent.jar", \
           "jcont.jar" , \
           "jacorb.jar", \
           "maci.jar", \
           "jcontnc.jar", \
           "acsnc.jar", \
           "acscomponent.jar", \
           "ACSErrTypeJavaNative.jar", \
           "cdbDAL.jar", \
           "castor.jar", \
           "commons-logging.jar", \
           "log4j-1.2.8.jar", \
           "jakarta-oro-2.0.5.jar", \
           "selector-1.1.jar"]
           
# The properties for the Java Virtual Machine
jvmProps = [ 
             "-Xms24m", \
             "-Xmx96m" , \
             "-Xverify:none" , \
             "-Xdebug", \
             "-Xnoagent" , \
             "-Xrunjdwp:transport=dt_socket,server=y,address=8000,suspend=n" ]

# The jars to install in netbeans/modules
modulesJars = [ "laserguiplatform.jar"]

# The jars in netbeans/modules/laser
laserJars = [  "log4j-1.2.8.jar", \
              "castor.jar", \
              "commons-logging.jar", \
              "acsjms.jar", \
              "alarmsysteminterface.jar", \
              "cmwmom.jar", \
              "gp.jar", \
              "gpopenide.jar", \
              "jms.jar", \
              "laserclient.jar", \
              "laserconsole.jar", \
              "lasercore.jar", \
              "laserdefinition.jar", \
              "laserutil.jar", \
              "ACSJMSMessageEntity.jar", \
              "AlarmSystem.jar", \
              "ACSAlarmMessage.jar" ]
              
# All the jars are installed in this directory of the user
# before launching the GUI
homeDir = os.environ['HOME']
nbUserDir = homeDir+"/.netbeans/3.5/"
print "User folder (local) for netbeans:",nbUserDir

if len(sys.argv)>2:
    usage()
    sys.exit(-1)
# The program accepts only one parameter: the CORBALOC of the manager or -h
elif len(sys.argv)==2:
    if sys.argv[1]=='-h':
        usage()
        sys.exit(0)
    else:
        managerCORBA=sys.argv[1]
        # Some checks before going on
        # The regular expression is not perfect but enough good I hope ;-)
        regExp = re.compile("corbaloc::[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+:[0-9]+/Manager")
        if not regExp.match(managerCORBA):
            print "Wrong CORBA loc for remote manager",managerCORBA
            sys.exit(-1)
else:
    # Check if the manager reference is in the env variable
    if os.environ.has_key('MANAGER_REFERENCE'):
        managerCORBA=os.environ['MANAGER_REFERENCE'].strip()
    else:
        managerCORBA=None

print "Manager:",
if managerCORBA!=None:
    print managerCORBA
else:
    print "UNDEFINED (will try the local host)"

# Get some useful variables from the environment
if os.environ.has_key('INTROOT'):
    introot = NormalizeDirName(os.environ['INTROOT'])
else:
    introot = None
if os.environ.has_key('INTLIST'):
    intlist = os.environ['INTLIST'].strip()
else:
    intlist = None

print "INTROOT:",introot
print "INTLIST:",intlist
    
try:
    # These variables must be defined otherwise there something wrong 
    # in the environment
    acsroot = NormalizeDirName(os.environ['ACSROOT'])
    acsData = NormalizeDirName(os.environ['ACSDATA'])
    homeDir = NormalizeDirName(os.environ['HOME'])
    javaHome= NormalizeDirName(os.environ['JAVA_HOME'])
    jacorbHome=NormalizeDirName(os.environ['JACORB_HOME'])
except:
    print "Error getting environment variables"
    sys.exit(-1)
    
try:
    netbeansHomeDir=getNetbeansHome()
except exceptions.IOError, e:
    print "Error:",e
    sys.exit(-1)
    
print "Netbeans installation folder:",netbeansHomeDir
    
# The java command with the path
javaExe = javaHome+"bin/java"

# The java class to exceture
javaMainClass = "org.netbeans.Main"

try:
    acsInstance = getInstance(managerCORBA)
except exceptions.IOError, e:
    print "Error:",e
    sys.exit(-1)

print "Using instance",acsInstance

# Get the directories that can contains jar files
searchDirs = getFoldersForJars(introot,acsroot,intlist,jacorbHome)

# Get jar files
javaJars = getAllJarsInFolder(javaHome+"lib")
netbeansExtJars = getAllJarsInFolder(netbeansHomeDir+"lib/ext")
acsJars = []
for jarName in acsRequiredJars:
    acsJars.append(getJarPath(jarName,searchDirs))
    
# Build the classpath
classpath =" -classpath "
allJars = netbeansExtJars+javaJars+acsJars
for jar in allJars:
    classpath+=jar+":"
classpath = classpath[0:len(classpath)-1]
classpath+=":"+acsData+"config"

# Build the string with props for JVM
JVMPropString = ""
for prop in jvmProps:
    JVMPropString+=prop+" "

print "Configuring netbeans"
cleanNetbeansModules(nbUserDir,modulesJars)
setupNetbeansModules(nbUserDir,modulesJars,laserJars,searchDirs)

# Build the command 
command = "%(exe)s %(classpath)s %(jvmprops)s %(acsprops)s %(nbprops)s %(main)s " % \
    { 'exe': javaExe, 
      'classpath':classpath, 
      'jvmprops':  JVMPropString, 
      'acsprops': getACSProps(acsInstance,managerCORBA), 
      'nbprops': getNetbeansProps(nbUserDir), 
      'main': javaMainClass }

print "Executing: ",command,"\n"

# Launch the GUI
os.system(command)


