'************************************************
'
' DOC2PDF.VBS Microsoft Scripting Host Script (Requires Version 5.6 or newer)
' --------------------------------------------------------------------------------
'
' Author: Michael Suodenjoki
' Created: 2003.09.19
'
' $Workfile: doc2pdf.vbs $
' $Author: gchiozzi $
' $Modtime: $
' $Archive: $
'
' $Revision: 1.1 $
'
' This script can create a PDF file from a Word document provided that you
' have Adobe Acrobat Distiller installed.
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
    WScript.Echo "doc2pdf Version 1.0, Michael Suodenjoki 2003"
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
    WScript.Echo "Generates a PDF file from a Word document using Adobe Distiller."
    WScript.Echo ""
    WScript.Echo "Usage: doc2pdf.vbs <options> <doc-file> [/o:<pdf-file>]"
    WScript.Echo ""
    WScript.Echo "Available Options:"
    WScript.Echo ""
    WScript.Echo " /nologo - Specifies that the logo shouldn't be displayed"
    WScript.Echo " /help - Specifies that this usage/help information should be displayed."
    WScript.Echo " /debug - Specifies that debug output should be displayed."
    WScript.Echo ""
    WScript.Echo "Parameters:"
    WScript.Echo ""
    WScript.Echo " /o:<pdf-file> Optionally specification of output file (PDF)."
    WScript.Echo " generate. E.g. dcdhelpconfig.xml"
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
' DOC2PDF
'
' Converts a Word document to PDF using Adobe
' Distiller.
'
' Input:
' sDocFile - Full path to Word document.
' sPDFFile - Optional full path to output file.
'
' If not specified the output PDF file
' will be the same as the sDocFile except
' file extension will be .pdf.
'
Function DOC2PDF( sDocFile, sPDFFile )

  Dim fso ' As FileSystemObject
  Dim wdo ' As Word.Application
  Dim wdoc ' As Word.Document
  Dim wdocs ' As Word.Documents
  Dim sPrevPrinter ' As String
  Dim oDistiller ' As PDFDistiller.PDFDistiller.1

  Set oDistiller = CreateObject("PDFDistiller.PDFDistiller.1")
  If oDistiller Is Nothing Then
    WScript.Echo "Error: Cannot create PDF document. Adobe Acrobat Distiller is not available! Quiting..."
    WScript.Quit 1
  End If

  Set fso = CreateObject("Scripting.FileSystemObject")
  Set wdo = CreateObject("Word.Application")
  Set wdocs = wdo.Documents

  sTempFile = fso.GetSpecialFolder(TemporaryFolder) + "\" + fso.GetTempName()

  sDocFile = fso.GetAbsolutePathName(sDocFile)

  ' Debug outputs...
  If bShowDebug Then
    WScript.Echo "Doc file = '" + sDocFile + "'"
    WScript.Echo "Temporary file = '" + sTempFile + "'"
    WScript.Echo "PDF file = '" + sPDFFile + "'"
  End If

  sFolder = fso.GetParentFolderName(sDocFile)

  If Len(sPDFFile)=0 Then
    sPDFFile = fso.GetBaseName(sDocFile) + ".pdf"
  End If

  If Len(fso.GetParentFolderName(sPDFFile))=0 Then
    sPDFFile = sFolder + "\" + sPDFFile
  End If

  ' Remember current active printer
  sPrevPrinter = wdo.ActivePrinter

  'wdo.ActivePrinter = "Acrobat PDFWriter"
  'wdo.ActivePrinter = "Acrobat Distiller"
  wdo.ActivePrinter = "Adobe PDF"

  ' Open the Word document
  Set wdoc = wdocs.Open(sDocFile)

  ' Print the Word document to the Acrobat Distiller - will generate a postscript (.ps) (temporary) file
  wdo.ActiveDocument.PrintOut False , , , sTempFile

  ' This outcommented part was used while trying to use "Acrobat PDFWriter"
  'Do While wdo.BackgroundPrintingStatus > 0
  ' 'Do nothing - just wait for printing to finish before closing Word
  'Loop

  wdoc.Close WdDoNotSaveChanges
  wdo.ActivePrinter = sPrevPrinter
  wdo.Quit WdDoNotSaveChanges
  Set wdo = Nothing

  ' Debug output...
  'If bShowDebug Then
  WScript.Echo " Distilling to '" + sPDFFile + "'"
  'End If

  ' Distill the postscript file to PDF
  oDistiller.FileToPDF sTempFile, sPDFFile, "Print"
  Set oDistiller = Nothing

  ' Delete the temporary postscript file...
  fso.DeleteFile( sTempFile )

  Set fso = Nothing

End Function

' *** MAIN **************************************

Call EchoLogo()
Call EchoUsage()
Call CheckArgs()
Call DOC2PDF( arguments.Unnamed.Item(0), arguments.Named.Item("o") )

Set arguments = Nothing