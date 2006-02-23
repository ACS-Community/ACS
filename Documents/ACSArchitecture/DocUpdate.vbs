'************************************************
'
' DOC2PDF.VBS Microsoft Scripting Host Script (Requires Version 5.6 or newer)
' --------------------------------------------------------------------------------
'
' Author: Gianluca Chiozzi
' Created: 2003.09.19
'
' $Workfile: DocUpdate.vbs $
' $Author: gchiozzi $
' $Modtime: $
' $Archive: $
'
' $Revision: 1.3 $
'
' This script can update all links and fields in a Word document
' located in the directory where this program runs
' in batch
'


' Constants
Const WdPrintAllDocument = 0
Const WdDoNotSaveChanges = 0

' Global variables
Dim arguments
Set arguments = WScript.Arguments

' ***********************************************
' ECHOLOGO
'
' Outputs the logo information.
'
Function EchoLogo()
  If Not (arguments.Named.Exists("nologo") Or arguments.Named.Exists("n")) Then
    WScript.Echo "DocUpdate Version 1.0, Gianluca Chiozzi 2005"
    WScript.Echo "=============================================================================="
    WScript.Echo ""
  End If
End Function

' ***********************************************
' ECHOUSAGE
'
' Outputs the usage information.
'
Function EchoUsage()
  If arguments.Count=0 Or arguments.Named.Exists("help") or arguments.Named.Exists("h") Then
    WScript.Echo "Update the links and fields in a Word document."
    WScript.Echo ""
    WScript.Echo "Usage: DocUpdate.vbs <options> <doc-file>"
    WScript.Echo ""
    WScript.Echo "Available Options:"
    WScript.Echo ""
    WScript.Echo " /nologo - Specifies that the logo shouldn't be displayed"
    WScript.Echo " /help - Specifies that this usage/help information should be displayed."
    WScript.Echo " /debug - Specifies that debug output should be displayed."
    WScript.Echo ""
    WScript.Echo "Parameters:"
    WScript.Echo ""
  End If 
End Function

' ***********************************************
' CHECKARGS
'
' Makes some preliminary checks of the arguments.
' Quits the application is any problem is found.
'
Function CheckArgs()
  ' Check that <doc-file> is specified
  If arguments.Unnamed.Count <> 1 Then
    WScript.Echo "Error: Obligatory <doc-file> parameter missing!"
    WScript.Quit 1
  End If

  bShowDebug = arguments.Named.Exists("debug") Or arguments.Named.Exists("d")

End Function



' ***********************************************
' DOCUPDATE
'
' Update links and fields of a Word document
'
' Input:
' sDocFile - Full path to Word document to be opend, 
'            edited and saved
'
Function DOCUPDATE( sDocFile )

  Dim fso ' As FileSystemObject
  Dim wdo ' As Word.Application
  Dim wdoc ' As Word.Document
  Dim wdocs ' As Word.Documents

  Set fso = CreateObject("Scripting.FileSystemObject")
  ' Set curDir = CurDir()

  Dim CurrentFolder
  Set CurrentFolder = fso.GetFolder(".")

  ' ========================
  ' Debug outputs...
  WScript.Echo "Updating doc file = '" + sDocFile + "' in:" + CurrentFolder.Path
  If bShowDebug Then
    WScript.Echo "Doc file = '" + sDocFile + "'"
  End If

  sDocFullFile  = fso.GetAbsolutePathName(sDocFile)

  Set wdo = CreateObject("Word.Application")
  Set wdocs = wdo.Documents

  ' This is the very important call I was missing!!!
  ' This changes the current working directory
  ' of Word to the directory where the file resides.
  wdo.ChangeFileOpenDirectory CurrentFolder.Path



  ' Open the Word document
  Set wdoc = wdocs.Open(sDocFullFile)


  For Each aField In wdoc.Fields
    Set fieldCode = aField.Code
    If aField.Update = True Then
       WScript.Echo "Field:                " + fieldCode + "updated successfully"
    Else
       WScript.Echo "Error updating field: " + fieldCode
    End If        
  Next

  WScript.Echo "Saving file"
  wdoc.Save
  wdoc.Close WdSaveChanges
  wdo.Quit WdSaveChanges

  Set wdo = Nothing
  Set fso = Nothing

  WScript.Echo "Done."

End Function

' *** MAIN **************************************

Call EchoLogo()
Call EchoUsage()
Call CheckArgs()
Call DOCUPDATE( arguments.Unnamed.Item(0) )

Set arguments = Nothing



