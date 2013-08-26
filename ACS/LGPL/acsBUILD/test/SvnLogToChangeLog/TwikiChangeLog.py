'''
Created on Jun 18, 2013

@author: acaproni
'''
from datetime import datetime
import SvnLogEntry

class TwikiChangeLog:
    '''
    TwikiChangeLog writes the ChangeLog in Twiki format.
    
    We don't care of the changes for author (we do not want to blame anybody here) 
    but we want to have the page with the following format:
    ---+ Title
    %TOC% 
    ----------
    ---++ Stats
    ---++ ChangeLog by date
    ---++ Change by module
    ---++ Change by file
    
    This class allows also to create the changes by developer but this is not part of the ChangeLog.
    It is useful for ACS staff to write the ReleaseNotes.
    '''
    
    def __init__(self, entries):
        '''
        Constructor
        
        entries: the list of svn logs to manipulate (i.e a list of SvnLogEntry objects)
        '''
        self.entries=entries
        
    def printEntry(self,entry,outFile):
        '''
        Format and print the passed entry 
        
        entry: the entry (SvnLogEntry) to be formatted and printed
        outFile: the file to write the twiki page int
        '''
        outFile.write("*"+entry.date+"* _"+entry.author+"_\n")
        for path in entry.files:
            outFile.write("   * "+path.action+" ="+path.path+"= ("+path.kind+")\n")
        outFile.write("<BLOCKQUOTE><LITERAL>"+entry.msg.strip()+"</LITERAL></BLOCKQUOTE>\n")
        outFile.write("-------\n") 
        
    def printFileEntry(self, fileName, svnEntries,outFile):
        '''
        Format and print a file entry that is potentially composed of several
        entry (SvnLogEntry) if the files has been changed and committed more then once.
        
        fileName: the name (path) of the file
        svnEntries: the list of svn entries (SvnLogEntry)
        outFile: the file to write the twiki page int
        '''
        outFile.write("*"+fileName+"*\n")
        for entry in svnEntries:
            outFile.write("<DIV>\n")
            outFile.write("   * "+entry.date+" _"+entry.author+"_\n")
            outFile.write("<BLOCKQUOTE><LITERAL>"+entry.msg.strip()+"</LITERAL></BLOCKQUOTE>\n")
            outFile.write("</DIV>\n")
        outFile.write("-------\n")
            
    def printStats(self,outFile):
        '''
        print some stats on the stdout
        
        outFile: the file to write the twiki page int
        '''
        outFile.write("---++ Stats\n")
        outFile.write('*Total number of commits:'+str(len(self.entries))+'*\n\n')
    
        # Print the number of entries for author
        # It is the number of commit and not the number of files because each commit can 
        # include more then one source file
        changesByAuthor={}
        # The number of files modified by each author (this is not the number of log entries)
        modifiedFilesForAuthor={}
        for entry in self.entries:
            if entry.author in changesByAuthor:
                changesByAuthor[entry.author]=changesByAuthor[entry.author]+1
            else:
                changesByAuthor[entry.author]=1
                
            if entry.author in modifiedFilesForAuthor:
                modifiedFilesForAuthor[entry.author]=modifiedFilesForAuthor[entry.author]+len(entry.files)
            else:
                modifiedFilesForAuthor[entry.author]=len(entry.files)
        
        authors=changesByAuthor.keys()
        authors.sort()
        outFile.write("| *Author* | *# Commits* | *# Changed sources* |\n")
        for author in authors:
            outFile.write("| "+author+" | "+str(changesByAuthor[author])+" | "+str(modifiedFilesForAuthor[author])+" |\n")
        
    def changeByDate(self,outFile):
        '''
        Scans the entries to generate the entries by date.
        
        Note that this is the order of the entries returned by SVN
        so no check is done here but only formatting
        
        outFile: the file to write the twiki page int
        '''
        outFile.write("---++ ChangeLog by date\n")
        for entry in self.entries:
            self.printEntry(entry,outFile)
            
    def changeByDeveloper(self,outFile):
        '''
        Create the table with the changes done by each developer.
        This is not included in the ChangeLog but used internally
        by ACS to write the release notes.
        
        outFile: the file to write the twiki page int
        ''' 
        # Scans all the entries (that are ordered by time)
        # and create a dictionary for each user
        outFile.write("---+ Changes by developer\n")
        outFile.write("%TOC%\n")
        outFile.write("-------------\n")
        userChanges={}
        for entry in self.entries:
            if entry.author not in userChanges:
                userChanges[entry.author]=[]
            userChanges[entry.author].append(entry)
        
        # Scans the list and generate the twiki page.
        authors=userChanges.keys()
        authors.sort()
        for author in authors:
            outFile.write("---++ "+author+"\n")
            for entry in userChanges[author]:
                self.printEntry(entry,outFile)
    
    def createTwikiPage(self,outFile):
        '''
        Create the whole twiki page delegating to the other methods.
        
        outFile: the file to write the twiki page int
        '''
        now=datetime.now()
        outFile.write("---+ SVN %NOP%ChangeLog\n")
        outFile.write("Automatically generated on "+now.isoformat()+"\n")
        outFile.write("%TOC%\n")
        outFile.write("----------\n\n")
        self.printStats(outFile)
        self.changeByDate(outFile)
        self.changeByFile(outFile)
    
    def changeByFile(self,outFile):
        '''
        Generate the change log by file.
        
        Differently from CVS, each commit in SVN can involve more files at the same time
        
        outFile: the file to write the twiki page int
        '''
        outFile.write("---++ ChangeLog by file\n")
        # Scans all the entries and produce a new entry for each of files it contain.
        # At the end of this pass there is a bigger list of entries each of which contains 
        # only one file
        entriesWithOneFileOnly=[]
        for entry in self.entries:
            if len(entry.files)==1:
                entriesWithOneFileOnly.append(entry)
            else:
                # more files then split in more entries
                for filePath in entry.files:
                    fileList=[]
                    fileList.append(filePath)
                    newLogEntry=SvnLogEntry.SvnLogEntry(entry.revision,entry.author,entry.date,fileList,entry.msg)
                    entriesWithOneFileOnly.append(newLogEntry)
        # We create a dictionary with key the file name and value
        # the list of entries of such file name
        entriesByFileName={}
        for entry in entriesWithOneFileOnly:
            fileName=entry.files[0].path.strip()
            if fileName not in entriesByFileName:
                entriesByFileName[fileName]=[]
            entriesByFileName[fileName].append(entry)
        # Sorted list of file names
        sortedFileNames=entriesByFileName.keys()
        sortedFileNames.sort()
        print len(entriesByFileName),"entries without duplication"
        for f in sortedFileNames:
            self.printFileEntry(f,entriesByFileName[f],outFile)
            