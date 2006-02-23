/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs.cdb.dal;

import java.io.StringReader;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.InputSource;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DAOOperations;
import com.cosylab.CDB.XMLerror;
import com.cosylab.cdb.jdal.DAOImpl;
import com.cosylab.cdb.jdal.XMLHandler;

/**
 * Class representing local DAO object.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public final class DAOLocalProxy extends DAOProxy 
{
	
	/**
	 * Constructor.
	 * 
	 * @param curl		CDB CURL of the DAO
	 * @param dal		CORBA reference of the DAL
	 * @throws Exception
	 */
	public DAOLocalProxy(DAL dal, String curl) throws Exception
	{
		super(dal, curl, null);
		dao = createDAO(dal, curl);
	}

	/**
	 * Returns the locally cached DAO.
	 * @param curl		CDB CURL of the DAO
	 * @param dal		CORBA reference of the DAL
	 * @return DAOOperations	locally cached DAO
	 * @throws Exception
	 */
	private DAOOperations createDAO(DAL dal, String curl) throws Exception
	{

		String xml = dal.get_DAO(curl);

		SAXParserFactory factory = SAXParserFactory.newInstance();
		SAXParser saxParser = factory.newSAXParser();
		
		// use CDB XML handler which does not creates strings...
		XMLHandler xmlSolver = new XMLHandler(false);
		
		saxParser.parse(new InputSource(new StringReader(xml)), xmlSolver);
		
		if (xmlSolver.m_errorString != null)
			throw new XMLerror("XML parser error: " + xmlSolver.m_errorString);
		
		// create non-CORBA related, silent DAO
		return new DAOImpl(curl, xmlSolver.m_rootNode, null, true);
	}

}
