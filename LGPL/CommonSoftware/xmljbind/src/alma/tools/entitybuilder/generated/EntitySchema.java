/*
 * This class was automatically generated with 
 * <a href="http://castor.exolab.org">Castor 0.9.3.9+</a>, using an
 * XML Schema.
 * $Id: EntitySchema.java,v 1.7 2002/09/02 16:03:20 vltsccm Exp $
 */

package alma.tools.entitybuilder.generated;

  //---------------------------------/
 //- Imported classes and packages -/
//---------------------------------/

import java.io.Reader;
import java.io.Serializable;
import java.io.Writer;

/**
 * 
 * 
 * @version $Revision: 1.7 $ $Date: 2002/09/02 16:03:20 $
**/
public class EntitySchema implements java.io.Serializable {


      //--------------------------/
     //- Class/Member Variables -/
    //--------------------------/

    private java.lang.String _schemaName;

    private java.lang.String _relativePathSchemafile = "";

    private java.lang.String _xmlNamespace;


      //----------------/
     //- Constructors -/
    //----------------/

    public EntitySchema() {
        super();
        setRelativePathSchemafile("");
    } //-- alma.tools.entitybuilder.generated.EntitySchema()


      //-----------/
     //- Methods -/
    //-----------/

    /**
     * Returns the value of field 'relativePathSchemafile'.
     * 
     * @return the value of field 'relativePathSchemafile'.
    **/
    public java.lang.String getRelativePathSchemafile()
    {
        return this._relativePathSchemafile;
    } //-- java.lang.String getRelativePathSchemafile() 

    /**
     * Returns the value of field 'schemaName'.
     * 
     * @return the value of field 'schemaName'.
    **/
    public java.lang.String getSchemaName()
    {
        return this._schemaName;
    } //-- java.lang.String getSchemaName() 

    /**
     * Returns the value of field 'xmlNamespace'.
     * 
     * @return the value of field 'xmlNamespace'.
    **/
    public java.lang.String getXmlNamespace()
    {
        return this._xmlNamespace;
    } //-- java.lang.String getXmlNamespace() 

    /**
     * Sets the value of field 'relativePathSchemafile'.
     * 
     * @param relativePathSchemafile the value of field
     * 'relativePathSchemafile'.
    **/
    public void setRelativePathSchemafile(java.lang.String relativePathSchemafile)
    {
        this._relativePathSchemafile = relativePathSchemafile;
    } //-- void setRelativePathSchemafile(java.lang.String) 

    /**
     * Sets the value of field 'schemaName'.
     * 
     * @param schemaName the value of field 'schemaName'.
    **/
    public void setSchemaName(java.lang.String schemaName)
    {
        this._schemaName = schemaName;
    } //-- void setSchemaName(java.lang.String) 

    /**
     * Sets the value of field 'xmlNamespace'.
     * 
     * @param xmlNamespace the value of field 'xmlNamespace'.
    **/
    public void setXmlNamespace(java.lang.String xmlNamespace)
    {
        this._xmlNamespace = xmlNamespace;
    } //-- void setXmlNamespace(java.lang.String) 

}
