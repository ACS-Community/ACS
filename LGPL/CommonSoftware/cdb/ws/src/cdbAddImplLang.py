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

verbose = False
execute = True
backup = False

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

    filestr = filestr[idx:lastidx] + "\n ImplLang=\""+lang+"\" " +filestr[lastidx:]

    #write filestr to file
    if verbose:
    	print "---------------------------------------------------"
	print "Writing the file:"
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
    print "Add ImplLang to Container xml CDB files when is missing"
    print ""
    print "Usage cdbAddImplLang.py [OPTIONS]"
    print ""
    print "Options:"
    print "   -p PATH	the path to search for CDBs to change, default is the current directory"
    print "   -v	prints information"
    print "   -n	it doesn't execute the changes, it is used along with -v to just print"
    print "   -b	it creates backup for the files"
    print "   -h	it creates backup for the files"

if __name__ == "__main__":

    try:
        opts, args = getopt.getopt(sys.argv[1:], "vnbp:h", [])
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
        elif o == "-b":
            backup = True
        else:
            assert False, "unhandled option"

    maciPath = "/MACI/Containers"

    for root, dirs, files in os.walk(path):
	
	if root.endswith(maciPath):
	    if verbose:
	    	print "###################################"
	    	print "[Main] a CDB found!:",root
	    	print "###################################"
	    #we found a potential CDB Containers xmls
    	    for rootCont, dirsCont, filesCont in os.walk(root):
	        for filename in filesCont:
		    
		    if filename.lower().endswith(".xml"):
	    		#print "[Main] a Container xml found!:",rootCont+"/"+filename
			#I assume xml files under MACI/Containers are Container configuration
			if not haveImplLang(rootCont, filename):
			    idx = rootCont.find(maciPath)
			    contName = rootCont[idx+len(maciPath)+1:]
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
			#else:
			#    print "[Main] It have already ImplLang"
