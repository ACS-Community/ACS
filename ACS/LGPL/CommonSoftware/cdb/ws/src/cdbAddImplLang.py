#!/usr/bin/env python

import sys
import shutil
import os
import getopt
from xml.parsers import expat

class Xml2Container(object):

    def StartElement(self, name, attributes):
	if attributes.has_key('Container') and attributes[u'Container'] == self.container:
	    code = attributes['Code']
	    if code.find("alma.")==0:
            	self.lang = "java"
            elif code.find('.') > 0 :
                self.lang = "py"
            else:
                self.lang = "cpp"
	 
    def Parse(self, filename, contName):
	Parser = expat.ParserCreate()
	Parser.StartElementHandler = self.StartElement
	self.container = contName
	self.lang = None
	ParserStatus = Parser.Parse(open(filename).read(),1)
	return self.lang

class Xml2Component(object):

    def StartElement(self, name, attributes):
	if attributes.has_key('Container') and not attributes.has_key('ImplLang'):
	    code = attributes['Code']
	    if code.find("alma.")==0:
            	lang = "java"
            elif code.find('.') > 0 :
                lang = "py"
            else:
                lang = "cpp"
	    self.lang = None
	    self.isModified = True
    	    lastidx1 = self.filestr.find(">",self.Parser.ErrorByteIndex+self.offset) 
	    lastidx2 = self.filestr.find("/>",self.Parser.ErrorByteIndex+self.offset) 
	    if lastidx1 == -1 and lastidx2 == -1:
                sys.stderr.write("ERROR: '>' and /> not found?? \n")
	    elif lastidx1 == -1:
        	lastidx = lastidx2
	    elif lastidx2 == -1:
                lastidx = lastidx1
	    elif lastidx1 < lastidx2:
		lastidx = lastidx1
	    else:
		 lastidx = lastidx2

    	    self.filestr = self.filestr[:lastidx] + " ImplLang=\""+lang+"\" " +self.filestr[lastidx:]
	    self.offset += len(" ImplLang=\""+lang+"\" ")
	    #print "Component", attributes['Name']," ", self.filestr[self.Parser.ErrorByteIndex:self.Parser.ErrorByteIndex+20]
	 
    def Parse(self, filename):
	self.Parser = expat.ParserCreate()
	self.Parser.StartElementHandler = self.StartElement
	self.offset = 0
	self.isModified = False
	print filename
	self.filestr = open(filename).read()
	ParserStatus = self.Parser.Parse(open(filename).read(),1)
	return  self.filestr

verbose = False
execute = True
backup = False
containers = False
components = False

def haveImplLang(rootContainers, filename):

    filestr = open(rootContainers+"/"+filename, "r").read()
    idx = filestr.find("ImplLang")
    if idx >0:
	return True
    return False

def guessImplLang(rootComp, contName):
    parser = Xml2Container()
    for root, dirs, files in os.walk(rootComp):
	for filename in files:
	    if filename.lower().endswith(".xml"):
            	#print "[guessImplLang] looking for container="+contName+" in CDB file="+root+"/"+filename
		lang = parser.Parse(root+"/"+filename, contName)
		if lang != None:
    		    if verbose:
			print "[guessImplLang] Found! in CDB file="+root+"/"+filename
		    return lang 

    sys.stderr.write("ERROR container="+contName+" not found in the components configuration of this CDB ="+rootComp+"\n")
    if verbose:
        print "[guessImplLang] ERROR container="+contName+" not found in the components configuration of this CDB"
    return None

def addImplLang(lang, rootContainers, filename):

    filestr = open(rootContainers+"/"+filename, "r").read()
    idx = filestr.find("<Container")
    if idx == -1:
    	idx = filestr.find("< Container")
	if idx == -1:
    	    if verbose:
	        print "[addImplLang] ERROR: '<Container' or '< Container' not found"
	    sys.stderr.write("ERROR: '<Container' or '< Container' not found in "+rootContainers+"/"+filename+"\n")
	    return

    lastidx = filestr.find(">",idx) 
    if lastidx == -1:
	if verbose:
            print "[addImplLang] ERROR: '>' not found"
        sys.stderr.write("ERROR: '>' not found in "+rootContainers+"/"+filename+"\n")
        return

    filestr = filestr[:lastidx] + "\n ImplLang=\""+lang+"\" " +filestr[lastidx:]

    #write filestr to file
    if verbose:
    	print "---------------------------------------------------"
	print "Modified file:"+rootContainers+"/"+filename
    	print "---------------------------------------------------"
    	print filestr
    	print "---------------------------------------------------"

    if backup:
	shutil.copyfile(os.path.join(rootContainers, filename),os.path.join(rootContainers, filename+".bkp"))    
	if verbose:
	    print "[addImplLang] Saving a backup copy of "+rootContainers+"/"+filename+".bkp"

    if execute:
        os.remove(os.path.join(rootContainers, filename))
	newfile = open(rootContainers+"/"+filename, "w")
    	newfile.write(filestr)
    	newfile.close()

def usage():
    print "Add ImplLang to Container and Component xml CDB files when is missing"
    print ""
    print "Usage cdbAddImplLang.py [OPTIONS]"
    print ""
    print "Options:"
    print "   -p PATH	the path to search for CDBs to change, default is the current directory"
    print "   -v	prints information"
    print "   -n	it doesn't execute the changes, it is used along with -v to just print"
    print "   -b	it creates backup for the files"
    print "   -h	it shows this message"
    print "   --containers	it modified only containers"
    print "   --components	it modified only components"
    print "   -a	it modified component and containers"

if __name__ == "__main__":

    try:
        opts, args = getopt.getopt(sys.argv[1:], "vnbp:ha", ["containers","components"])
    except getopt.GetoptError, err:
	# print help information and exit:
	print str(err) # will print something like "option -a not recognized"
	usage()
	sys.exit(2)
    path ="." 
    for o, a in opts:
        if o == "-v":
            verbose = True
        elif o == "-h":
            usage()
	    sys.exit()
        elif o == "-p":
            path = a
        elif o == "-n":
            execute = False
        elif o == "-a":
            containers = True
	    components = True
        elif o == "--containers":
            containers = True
        elif o == "--components":
	    components = True
        elif o == "-b":
            backup = True
        else:
            assert False, "unhandled option"

    maciContPath = "/MACI/Containers"
    maciCompPath = "/MACI/Components"

    for root, dirs, files in os.walk(path):
	if root.endswith(maciCompPath) and components:
	    isCDBModified = False
	    if verbose:
	    	print "###################################"
	    	print "[Main] a CDB/MACI/Components found!:",root
	    	print "###################################"
	    #we found a potential CDB Components xmls
    	    for rootComp, dirsComp, filesComp in os.walk(root):
	        for filename in filesComp:
		    
		    if filename.lower().endswith(".xml"):
	    		#print "[Main] a Components xml found!:",rootComp+"/"+filename
			#I assume xml files under MACI/Components components configuration
    			parser = Xml2Component()
			filestr = parser.Parse(rootComp+"/"+filename)
			if parser.isModified:
	    		    isCDBModified = True
			    if verbose:
			        print "---------------------------------------------------"
			        print "Modified file:"+rootComp+"/"+filename
			        print "---------------------------------------------------"
			        print filestr
			        print "---------------------------------------------------"
			    if backup:
			        shutil.copyfile(os.path.join(rootComp, filename),os.path.join(rootComp, filename+".bkp"))    
			        if verbose:
				    print "[addImplLang] Saving a backup copy in "+rootComp+"/"+filename+".bkp"

			    if execute:
			        os.remove(os.path.join(rootComp, filename))
			        newfile = open(rootComp+"/"+filename, "w")
			        newfile.write(filestr)
			        newfile.close()
	    if not isCDBModified and verbose:
	        print "[Main] Nothing to do here."	
	
	if root.endswith(maciContPath) and containers:
	    if verbose:
	    	print "###################################"
	    	print "[Main] a CDB/MACI/Containers found!:",root
	    	print "###################################"
	    #we found a potential CDB Containers xmls
	    isCDBModified = False
    	    for rootCont, dirsCont, filesCont in os.walk(root):
	        for filename in filesCont:
		    
		    if filename.lower().endswith(".xml"):
	    		#print "[Main] a Container xml found!:",rootCont+"/"+filename
			#I assume xml files under MACI/Containers are Container configuration
			if not haveImplLang(rootCont, filename):
	    		    isCDBModified = True
			    idx = rootCont.find(maciContPath)
			    contName = rootCont[idx+len(maciContPath)+1:]
			    rootComp = root[:len(root)-10]+"Components"
	    		    if verbose:
			    	print "[Main] Container doesn't have ImplLang, searching for container="+contName+" in "+rootComp
			    lang = guessImplLang(rootComp, contName)
			    if lang != None:
			    	addImplLang(lang, rootCont, filename)
	    		    	if verbose:
		            	    print "[Main] Added ImplLang="+lang+" to "+contName
			    else:
	    		    	if verbose:
				    print "[Main] ERROR: CDB container file wasn't updated="+rootCont+"/"+filename
	    if not isCDBModified and verbose:
	        print "[Main] Nothing to do here."	
