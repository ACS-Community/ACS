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
		if(filename.startsWith("file://"))
			filename=filename.substring(7);
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
		if(filename.startsWith("file://"))
			filename=filename.substring(7);
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
                        if(filename.startsWith("file://"))
                                filename=filename.substring(7);
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
