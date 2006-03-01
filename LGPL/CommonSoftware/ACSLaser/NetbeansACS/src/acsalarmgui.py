'''Start the netbeans GUI for alarm'''

import os, os.path, exceptions, sys, socket, re

####################### Useful methods #####################

def NormalizeDirName(name):
    '''Add a trailing '/' to the dir name, if not already present'''
    if name[len(name)-1]!='/':
            name+='/'
    return name

def getNetbeansHome():
    '''Return the home directory of netbeans
    
    It can be NBPLATFORM_HOME env variable or ~/netbeans
    In future it can be different'''
    global homeDir
    if os.environ.has_key('NBPLATFORM_HOME'):
        nbHome = os.environ['NBPLATFORM_HOME']
    else:
        nbHome = homeDir
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
        print localIP
        managerCORBALoc= 'corbaloc::%(ip)s:%(port)d:/Manager' % \
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

def getNetbeansProps():
    global netbeansHomeDir, homeDir
    # "-Dnetbeans.osenv=/tmp/nbenv.20411", \
    
    properties = [ \
                   "-Dnetbeans.user="+homeDir+".netbeans/3.5" , \
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

# The program accepts only one parameter: the CORBALOC of the manager
if len(sys.argv)==2:
    managerCORBA=sys.argv[1]
    # Some checks before going on
    # The regular expression is not perfect but enough good I hope ;-)
    regExp = re.compile("corbaloc::[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+:[0-9]+/Manager")
    if not regExp.match(managerCORBA):
        print "Wrong CORBA loc for remote manager",managerCORBA
        sys.exit(-1)
else:
   managerCORBA=None 

# Get some useful variables from the environment
if os.environ.has_key('INTROOT'):
    introot = NormalizeDirName(os.environ['INTROOT'])
else:
    introot = None
if os.environ.has_key('INTLIST'):
    intlist = os.environ['INTLIST'].strip()
else:
    intlist = None
    
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
    
# The java command with the path
javaExe = javaHome+"bin/java"

# The java class to exceture
javaMainClass = "org.netbeans.Main"

print "Using netbeans in", netbeansHomeDir

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
classpath+=acsData+"config"

# Build the string with props for JVM
JVMPropString = ""
for prop in jvmProps:
    JVMPropString+=prop+" "

# Build the command 
command = "%(exe)s %(classpath)s %(jvmprops)s %(acsprops)s %(nbprops)s %(main)s" % \
    { 'exe': javaExe, 
      'classpath':classpath, 
      'jvmprops':  JVMPropString, 
      'acsprops': getACSProps(acsInstance,managerCORBA), 
      'nbprops': getNetbeansProps(), 
      'main': javaMainClass }

print "\n",command,"\n"
#os.system(command)
getLocalIP()


