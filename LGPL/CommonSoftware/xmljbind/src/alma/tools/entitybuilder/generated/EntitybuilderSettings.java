/*
 * This class was automatically generated with 
 * <a href="http://castor.exolab.org">Castor 0.9.3.9+</a>, using an
 * XML Schema.
 * $Id: EntitybuilderSettings.java,v 1.7 2002/09/02 16:03:20 vltsccm Exp $
 */

package alma.tools.entitybuilder.generated;

  //---------------------------------/
 //- Imported classes and packages -/
//---------------------------------/

import java.util.ArrayList;

/**
 * 
 * 
 * @version $Revision: 1.7 $ $Date: 2002/09/02 16:03:20 $
**/
public class EntitybuilderSettings implements java.io.Serializable {


      //--------------------------/
     //- Class/Member Variables -/
    //--------------------------/

    private java.util.ArrayList _entitySchemaList;

    private java.util.ArrayList _xmlNamespace2JPackageList;


      //----------------/
     //- Constructors -/
    //----------------/

    public EntitybuilderSettings() {
        super();
        _entitySchemaList = new ArrayList();
        _xmlNamespace2JPackageList = new ArrayList();
    } //-- alma.tools.entitybuilder.generated.EntitybuilderSettings()


      //-----------/
     //- Methods -/
    //-----------/

    /**
     * 
     * 
     * @param vEntitySchema
    **/
    public void addEntitySchema(EntitySchema vEntitySchema)
        throws java.lang.IndexOutOfBoundsException
    {
        _entitySchemaList.add(vEntitySchema);
    } //-- void addEntitySchema(EntitySchema) 

    /**
     * 
     * 
     * @param index
     * @param vEntitySchema
    **/
    public void addEntitySchema(int index, EntitySchema vEntitySchema)
        throws java.lang.IndexOutOfBoundsException
    {
        _entitySchemaList.add(index, vEntitySchema);
    } //-- void addEntitySchema(int, EntitySchema) 

    /**
     * 
     * 
     * @param vXmlNamespace2JPackage
    **/
    public void addXmlNamespace2JPackage(XmlNamespace2JPackage vXmlNamespace2JPackage)
        throws java.lang.IndexOutOfBoundsException
    {
        _xmlNamespace2JPackageList.add(vXmlNamespace2JPackage);
    } //-- void addXmlNamespace2JPackage(XmlNamespace2JPackage) 

    /**
     * 
     * 
     * @param index
     * @param vXmlNamespace2JPackage
    **/
    public void addXmlNamespace2JPackage(int index, XmlNamespace2JPackage vXmlNamespace2JPackage)
        throws java.lang.IndexOutOfBoundsException
    {
        _xmlNamespace2JPackageList.add(index, vXmlNamespace2JPackage);
    } //-- void addXmlNamespace2JPackage(int, XmlNamespace2JPackage) 

    /**
    **/
    public void clearEntitySchema()
    {
        _entitySchemaList.clear();
    } //-- void clearEntitySchema() 

    /**
    **/
    public void clearXmlNamespace2JPackage()
    {
        _xmlNamespace2JPackageList.clear();
    } //-- void clearXmlNamespace2JPackage() 

    /**
    **/
    public java.util.Enumeration enumerateEntitySchema()
    {
        return new org.exolab.castor.util.IteratorEnumeration(_entitySchemaList.iterator());
    } //-- java.util.Enumeration enumerateEntitySchema() 

    /**
    **/
    public java.util.Enumeration enumerateXmlNamespace2JPackage()
    {
        return new org.exolab.castor.util.IteratorEnumeration(_xmlNamespace2JPackageList.iterator());
    } //-- java.util.Enumeration enumerateXmlNamespace2JPackage() 

    /**
     * 
     * 
     * @param index
    **/
    public EntitySchema getEntitySchema(int index)
        throws java.lang.IndexOutOfBoundsException
    {
        //-- check bounds for index
        if ((index < 0) || (index > _entitySchemaList.size())) {
            throw new IndexOutOfBoundsException();
        }
        
        return (EntitySchema) _entitySchemaList.get(index);
    } //-- EntitySchema getEntitySchema(int) 

    /**
    **/
    public EntitySchema[] getEntitySchema()
    {
        int size = _entitySchemaList.size();
        EntitySchema[] mArray = new EntitySchema[size];
        for (int index = 0; index < size; index++) {
            mArray[index] = (EntitySchema) _entitySchemaList.get(index);
        }
        return mArray;
    } //-- EntitySchema[] getEntitySchema() 

    /**
    **/
    public int getEntitySchemaCount()
    {
        return _entitySchemaList.size();
    } //-- int getEntitySchemaCount() 

    /**
     * 
     * 
     * @param index
    **/
    public XmlNamespace2JPackage getXmlNamespace2JPackage(int index)
        throws java.lang.IndexOutOfBoundsException
    {
        //-- check bounds for index
        if ((index < 0) || (index > _xmlNamespace2JPackageList.size())) {
            throw new IndexOutOfBoundsException();
        }
        
        return (XmlNamespace2JPackage) _xmlNamespace2JPackageList.get(index);
    } //-- XmlNamespace2JPackage getXmlNamespace2JPackage(int) 

    /**
    **/
    public XmlNamespace2JPackage[] getXmlNamespace2JPackage()
    {
        int size = _xmlNamespace2JPackageList.size();
        XmlNamespace2JPackage[] mArray = new XmlNamespace2JPackage[size];
        for (int index = 0; index < size; index++) {
            mArray[index] = (XmlNamespace2JPackage) _xmlNamespace2JPackageList.get(index);
        }
        return mArray;
    } //-- XmlNamespace2JPackage[] getXmlNamespace2JPackage() 

    /**
    **/
    public int getXmlNamespace2JPackageCount()
    {
        return _xmlNamespace2JPackageList.size();
    } //-- int getXmlNamespace2JPackageCount() 

    /**
     * 
     * 
     * @param vEntitySchema
    **/
    public boolean removeEntitySchema(EntitySchema vEntitySchema)
    {
        boolean removed = _entitySchemaList.remove(vEntitySchema);
        return removed;
    } //-- boolean removeEntitySchema(EntitySchema) 

    /**
     * 
     * 
     * @param vXmlNamespace2JPackage
    **/
    public boolean removeXmlNamespace2JPackage(XmlNamespace2JPackage vXmlNamespace2JPackage)
    {
        boolean removed = _xmlNamespace2JPackageList.remove(vXmlNamespace2JPackage);
        return removed;
    } //-- boolean removeXmlNamespace2JPackage(XmlNamespace2JPackage) 

    /**
     * 
     * 
     * @param index
     * @param vEntitySchema
    **/
    public void setEntitySchema(int index, EntitySchema vEntitySchema)
        throws java.lang.IndexOutOfBoundsException
    {
        //-- check bounds for index
        if ((index < 0) || (index > _entitySchemaList.size())) {
            throw new IndexOutOfBoundsException();
        }
        _entitySchemaList.set(index, vEntitySchema);
    } //-- void setEntitySchema(int, EntitySchema) 

    /**
     * 
     * 
     * @param entitySchemaArray
    **/
    public void setEntitySchema(EntitySchema[] entitySchemaArray)
    {
        //-- copy array
        _entitySchemaList.clear();
        for (int i = 0; i < entitySchemaArray.length; i++) {
            _entitySchemaList.add(entitySchemaArray[i]);
        }
    } //-- void setEntitySchema(EntitySchema) 

    /**
     * 
     * 
     * @param index
     * @param vXmlNamespace2JPackage
    **/
    public void setXmlNamespace2JPackage(int index, XmlNamespace2JPackage vXmlNamespace2JPackage)
        throws java.lang.IndexOutOfBoundsException
    {
        //-- check bounds for index
        if ((index < 0) || (index > _xmlNamespace2JPackageList.size())) {
            throw new IndexOutOfBoundsException();
        }
        _xmlNamespace2JPackageList.set(index, vXmlNamespace2JPackage);
    } //-- void setXmlNamespace2JPackage(int, XmlNamespace2JPackage) 

    /**
     * 
     * 
     * @param xmlNamespace2JPackageArray
    **/
    public void setXmlNamespace2JPackage(XmlNamespace2JPackage[] xmlNamespace2JPackageArray)
    {
        //-- copy array
        _xmlNamespace2JPackageList.clear();
        for (int i = 0; i < xmlNamespace2JPackageArray.length; i++) {
            _xmlNamespace2JPackageList.add(xmlNamespace2JPackageArray[i]);
        }
    } //-- void setXmlNamespace2JPackage(XmlNamespace2JPackage) 

}
