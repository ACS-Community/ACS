/**
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 * @author Jorge Avarias (javarias[at]alumnos.inf.utfsm.cl)
 */

package cl.utfsm.acs.types;

/**
 * An abstract class that defines an ACS XML object from a AcsType.
 * If an object is a typed object, then a value, a type, a documentation, 
 * and a namespace are associated to the object. Also, this class defines 
 * the toString() method, so any object will have an easy way to get the
 * value.
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 */
public abstract class TypedObject {
        /** The object type reference */
        protected AcsType type;
        /** The current value of the object */
        protected String value;
        /** Constructor with no operations (unuseful?) */
        public TypedObject(){
        }
        /** Constructor that setup the type. */
        public TypedObject(AcsType type){
                this.type=type;
        }
        /** Return the namespace of the type 
         * @return the type namespace*/
        public String getTypeNamespace(){
                return(type.namespace);
        }
        /** Return the documentation string.
         * @return the documentation string.
         */
        public String getTypeDocumentation(){
                return(type.documentation);
        }
        /** Return the typename of the type.
         * @return the typename
         */
        public String getTypeName(){
                return(type.name);
        }
        /** Return the current value of the object.
         * @return the value of the object
         */
        public String getValue(){
                return(value);
        }
        /** Setup the value of the Object
         * @param value the string value of the object
         */
        public void setValue(String value){
                this.value=value;
        }
        /** Return the current value of the object.
         * @return the value of the object
         */
        public String toString(){
                return(value);
        }
}


