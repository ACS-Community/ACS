/**
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 * @author Jorge Avarias (javarias[at]alumnos.inf.utfsm.cl)
 */

package cl.utfsm.acs.types;

/**
 * A standard ACS XML attribute type definition.
 * This class is used to define a new XML attribute, readed from a schema file, 
 * to setup the internal HashMaps of Errorbrowser and other applications.
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 */

public class AcsAttribute{
        /** The attribute name */
        public String name;
        /** The attribute type 
         * @see AcsSimpleType
         */
        public AcsSimpleType type;
        /** The usage information */
        public String use;
        /** Setup all the AcsAttribute information
         * @param name The attribute name
         * @param type The attribute type
         * @param use The usage information
         */
        public AcsAttribute(String name,AcsSimpleType type,String use){
                this.name=name;
                this.type=type;
                this.use=use;
        }
}

