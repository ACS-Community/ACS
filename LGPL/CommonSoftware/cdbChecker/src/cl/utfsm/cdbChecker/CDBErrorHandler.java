/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) UTFSM - Universidad Tecnica Federico Santa Maria, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
/**
 * @author Rodrigo Araya (raraya[at]inf.utfsm.cl) & Nicolas Barriga (nbarriga[at]inf.utfsm.cl) & Marco Salgado (msalgado[at]inf.utfsm.cl)
 * */

package cl.utfsm.cdbChecker;

import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;


class CDBErrorHandler extends DefaultHandler implements ErrorHandler {

	private CDBChecker _checker;

	public CDBErrorHandler(CDBChecker checker) {
		_checker = checker;
	}

	public void warning(SAXParseException e)
	throws SAXException{
		String filename=e.getSystemId();
		if(filename.startsWith("file:///"))
			filename=filename.substring(8);
		if(_checker.isVerbose())
			System.out.print(":"+e.getLineNumber()+":"+e.getColumnNumber()+" [Warning]\n\t "+e.getMessage()+"\n");
		else
			System.out.print(filename+":"+e.getLineNumber()+":"+e.getColumnNumber()+" [Warning]\n\t "+e.getMessage()+"\n");
		_checker.setErrorFlag(true);
		_checker.setGlobalErrorFlag(true);
	}

	public void error(SAXParseException e)
	throws SAXException{
		String filename=e.getSystemId();
		if(filename.startsWith("file:///"))
			filename=filename.substring(8);
		if(_checker.isVerbose())
			System.out.print(":"+e.getLineNumber()+":"+e.getColumnNumber()+" [Error]\n\t "+e.getMessage()+"\n");	
		else
			System.out.print(filename+":"+e.getLineNumber()+":"+e.getColumnNumber()+" [Error]\n\t "+e.getMessage()+"\n");	
			
		_checker.setErrorFlag(true);
		_checker.setGlobalErrorFlag(true);
	}
	
	public void fatalError(SAXParseException e)
	throws SAXException{
		String filename=e.getSystemId();
                if(filename != null){
                        if(filename.startsWith("file:///"))
                                filename=filename.substring(8);
                        if( _checker.isVerbose() )
                                System.out.print(":"+e.getLineNumber()+":"+e.getColumnNumber()+" [Fatal Error]\n\t "+e.getMessage()+"\n");
                        else
                                System.out.print(filename+":"+e.getLineNumber()+":"+e.getColumnNumber()+" [Fatal Error]\n\t "+e.getMessage()+"\n");
                }else{
                        System.out.print("\n[Fatal Error] There is something terribly wrong, we should never get here :(\n\t "+e.getMessage()+"\n");
                }
        		_checker.setErrorFlag(true);
        		_checker.setGlobalErrorFlag(true);
                System.out.print("\n************Unrecoverable error, please don't trust any messages after this one.***********\n\n");
	}
}
