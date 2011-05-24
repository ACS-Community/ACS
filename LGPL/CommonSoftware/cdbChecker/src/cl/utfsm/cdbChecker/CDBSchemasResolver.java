/**
 * @author Rodrigo Araya (raraya[at]inf.utfsm.cl) & Nicolas Barriga (nbarriga[at]inf.utfsm.cl) & Marco Salgado (msalgado[at]inf.utfsm.cl)
 * */

package cl.utfsm.cdbChecker;

import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import java.io.File;

class CDBSchemasResolver implements EntityResolver
{
	private String schemaPaths[];
	private CDBChecker _checker;

    /**
     * Instantiates a CDBSchemasResolver and
     * initialises the array of directories where to
     * search for schema files.
     */
    public CDBSchemasResolver(CDBChecker checker, String XSDPath)
	{
    	_checker = checker;
	    schemaPaths=XSDPath.split(""+File.pathSeparatorChar);
	}

    /**
     * Resolves and entity.
     * Called whenever a schema file needs to be
     * mapped into a real file
     */
    public InputSource resolveEntity(String publicID,String systemID)
	{
	    if (systemID.startsWith("http://")) 
		{
		String[] arr = systemID.split("/");
		return new InputSource(findSchemaFile(arr[arr.length-1]));
		}
	    return null;
	}

    /**
     * Finds a schema file, is exists, in the directories
     * configured at construction time.
     * Returns null if the schema file cannot be found
     *
     * TODO:
     * - Code could be optimised keeping a map of the 
     *   found schema files, since I see that the method is
     *   called many times for the same file.
     * - We should add some diagnostic, but calling directly
     *   println would interleave with the output from other
     *   parts of the code.
     */
    public String findSchemaFile(String schemaName)
	{
	    for(int i=0;i<schemaPaths.length;i++)
		{
		String filePath = schemaPaths[i]+schemaName;
		if((new File(filePath)).exists())
		    { 
		    return filePath;
		    }
		}
	    _checker.setGlobalErrorFlag(true);

	    return null;
	}
}

