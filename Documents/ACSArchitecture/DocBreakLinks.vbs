'************************************************
'
' DocBreakLinks.vbsS Microsoft Scripting Host Script (Requires Version 5.6 or newer)
' --------------------------------------------------------------------------------
'
' Author: Gianluca Chiozzi
' Created: 2005.06.03
'
' $Workfile: DocUpdate.vbs $
' $Author: gchiozzi $
' $Modtime: $
' $Archive: $
'
' $Revision: 1.2 $
'
' This script breaks all links and fields in a Word document
' located in the directory where this program runs
' in batch and save it with the name given as second parameter.
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
    WScript.Echo "DocBreakLinks, Gianluca Chiozzi 2005"
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
    WScript.Echo "Break the links in a Word document."
    WScript.Echo ""
    WScript.Echo "Usage: DocUpdate.vbs <options> <doc-file> <dest-file>"
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
  If arguments.Unnamed.Count <> 2 Then
    WScript.Echo "Error: Obligatory <doc-file> <dest-file> parameters missing!"
    WScript.Quit 1
  End If

  bShowDebug = arguments.Named.Exists("debug") Or arguments.Named.Exists("d")

End Function



' ***********************************************
' DOCBREAKLINKS
'
' Update links and fields of a Word document
'
' Input:
' sDocFile - Full path to Word document to be opend, 
'            edited and saved
'
Function DOCBREAKLINKS( sDocFile, dDocFile )

  Dim fso ' As FileSystemObject
  Dim wdo ' As Word.Application
  Dim wdoc ' As Word.Document
  Dim wdocs ' As Word.Documents
  Dim CurrentFolder

  Set fso           = CreateObject("Scripting.FileSystemObject")
  Set CurrentFolder = fso.GetFolder(".")

  ' ========================
  ' Debug outputs...
  WScript.Echo "Breaking links for doc file = '" + sDocFile + "' in:" + CurrentFolder.Path
  WScript.Echo "       and save as doc file = '" + dDocFile + "'"
  If bShowDebug Then
    WScript.Echo "Doc file = '" + sDocFile + "'"
  End If

  sDocFullFile  = fso.GetAbsolutePathName(sDocFile)
  dDocFullFile  = fso.GetAbsolutePathName(dDocFile)

  Set wdo   = CreateObject("Word.Application")
  Set wdocs = wdo.Documents

  ' This is the very important call I was missing!!!
  ' This changes the current working directory
  ' of Word to the directory where the file resides.
  wdo.ChangeFileOpenDirectory CurrentFolder.Path

  ' Open the Word document
  Set wdoc = wdocs.Open(sDocFullFile)

  ' Save the document with the new name, to
  ' avoid mesisng up the original
  wdoc.SaveAs(dDocFullFile)

  '
  ' First pass is supposed to break the INCLUDETEXT links
  '
  For Each aField In wdoc.Fields
    Set fieldCode = aField.Code
    If  InStr(fieldCode, "INCLUDETEXT") > 0 Then

       If aField.Update = True Then
         WScript.Echo "Field:                " + fieldCode + " broken successfully"
         aField.LinkFormat.BreakLink
       Else
         WScript.Echo "Error updating field: " + fieldCode
       End If        
    End If
  Next

  '
  ' Second pass is supposed to break the INCLUDEPUCTURE links
  ' that have been created by breaking the INCLUDETEXT links
  '
  For Each aField In wdoc.Fields
    Set fieldCode  = aField.Code
    If  InStr(fieldCode, "INCLUDEPICTURE") > 0 Then

       If aField.Update = True Then
         WScript.Echo "Field:                " + fieldCode + " broken successfully"
         aField.LinkFormat.BreakLink
       Else
         WScript.Echo "Error updating field: " + fieldCode
       End If        
    Else
       ' Just for documentation lists the unchanged fields.
       WScript.Echo "Field:                " + fieldCode + " left unchanged"
    End If
  Next

  WScript.Echo "Saving file " + dDocFullFile
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
Call DOCBREAKLINKS( arguments.Unnamed.Item(0), arguments.Unnamed.Item(1) )

Set arguments = Nothing



