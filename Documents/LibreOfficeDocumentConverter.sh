#!/bin/bash

#
# This is a very crude script to drive the conversion
# of documents using Open Office.
# It should be made nicer, more protectec from errors, 
# less verbose and, probably, re-written in Python
# to avoid using the bash, that is not nicely portable.
#
echo "Converting docs in PDF using LibreOffice"
echo "Dest dir $1"

libreoffice4.2 --headless --nologo --convert-to pdf --outdir $*

echo "Done PDF conversion from LibreOffice"

#__oOo__

