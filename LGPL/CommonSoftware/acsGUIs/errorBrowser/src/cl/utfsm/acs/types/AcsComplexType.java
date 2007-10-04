/**
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 * @author Jorge Avarias (javarias[at]alumnos.inf.utfsm.cl)
 */

package cl.utfsm.acs.types;

import java.util.ArrayList;

/**
 * This is a complex type, because contains an Arraylist with attributes. 
 * The main idea of this class is to setup a ComplexType for a complex object
 * with not only a value, but with attributes.
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 */
public class AcsComplexType extends AcsType {
        /** An Arraylist with the attributes. */
        public ArrayList<AcsAttribute> attrs;
        /** Fill the common AcsType values. Also initialize the ArrayList.
         * @param namespace Every type has a namespace
         * @param name The name of the type definition
         * @param documentation Some information of the type
         */
        public AcsComplexType(String namespace,String name,String documentation){
                super(namespace,name,documentation);
                attrs=new ArrayList<AcsAttribute>();
        }
        /** Add an Attribute to the Arraylist.
         * @param attr The attribute to add*/
        public void addAttr(AcsAttribute attr){
                attrs.add(attr);
        }
        /** Get an Arraylist of the attributes names.
         * @return An Arraylist of attributes names*/
        public ArrayList<String> getAttrNames(){
                ArrayList<String> names=new ArrayList<String>();
                for (AcsAttribute a : attrs){
                        names.add(a.name);
                }
                return(names);
        }
        /** Return an attribute type by name.
         * @param myName the name of the attribute
         * @return An attribute type, and null if myName was not found.
         */
        public AcsSimpleType getAttrType(String myName){
                for (AcsAttribute a : attrs){
                        if (a.name.compareTo(myName)==0)
                                return(a.type);
                }
                return(null);
        }
        /** Return the usage information of an attribute 
         * @param myName the name of the attribute
         * @return the string with the usage information,  and null if myName was not found.
         */
        public String getAttrUse(String myName){
                for (AcsAttribute a : attrs){
                        if (a.name.compareTo(myName)==0)
                                return(a.use);
                }
                return(null);
        }
}

