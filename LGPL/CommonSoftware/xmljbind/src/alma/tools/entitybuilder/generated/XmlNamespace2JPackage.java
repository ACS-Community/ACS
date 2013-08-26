/*
 * This class was automatically generated with 
 * <a href="http://castor.exolab.org">Castor 0.9.3.9+</a>, using an
 * XML Schema.
 * $Id: XmlNamespace2JPackage.java,v 1.7 2002/09/02 16:03:21 vltsccm Exp $
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
 * @version $Revision: 1.7 $ $Date: 2002/09/02 16:03:21 $
**/
public class XmlNamespace2JPackage implements java.io.Serializable {


      //--------------------------/
     //- Class/Member Variables -/
    //--------------------------/

    private java.lang.String _xmlNamespace;

    private java.lang.String _jPackage;


      //----------------/
     //- Constructors -/
    //----------------/

    public XmlNamespace2JPackage() {
        super();
    } //-- alma.tools.entitybuilder.generated.XmlNamespace2JPackage()


      //-----------/
     //- Methods -/
    //-----------/

    /**
     * Returns the value of field 'jPackage'.
     * 
     * @return the value of field 'jPackage'.
    **/
    public java.lang.String getJPackage()
    {
        return this._jPackage;
    } //-- java.lang.String getJPackage() 

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
     * Sets the value of field 'jPackage'.
     * 
     * @param jPackage the value of field 'jPackage'.
    **/
    public void setJPackage(java.lang.String jPackage)
    {
        this._jPackage = jPackage;
    } //-- void setJPackage(java.lang.String) 

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
