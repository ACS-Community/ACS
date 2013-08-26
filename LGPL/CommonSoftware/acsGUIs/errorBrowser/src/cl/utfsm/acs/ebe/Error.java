/**
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 * @author Jorge Avarias (javarias[at]alumnos.inf.utfsm.cl)
 */

package cl.utfsm.acs.ebe;
import cl.utfsm.acs.types.AcsComplexType;
import cl.utfsm.acs.types.ComplexObject;
import java.util.TreeMap;


/** The internal representation of an Error.
 * This is a ComplexObject, but with a TreeMap with
 * members. This, includes some new methods.
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 */
public class Error extends ComplexObject{
        /** The class AcsType*/
        protected static AcsComplexType errorType;
        /** The members TreeMap*/
        protected TreeMap<String,Member> members;

        /** The class method to setup the type */
        public static void setClassType(AcsComplexType t){
                errorType=t;
        }
        /** The class method to get the class Type
         * @return The class type
         */
        public static AcsComplexType getClassType(){
                return(errorType);
        }
        /** Error Constructor. And initilize members
         */
        public Error(){
                super(errorType);
                members=new TreeMap<String,Member>();
        }
        /** Return the members TreeMap. 
         * @return the TreeMap with the members.
         */
        public TreeMap<String,Member> getMembers(){
                return(members);
        }
        /** Get a member by name.
         * @param name the member name
         * @return the member object
         */
        public Member getMember(String name){
                return(members.get(name));
        }
        /** Add a member to the TreeMap.
         * @param name the member name
         * @param member the member object
         */
        public void putMember(String name,Member member){
                members.put(name,member);
        }
        /** Add a member to the TreeMap. This includes the member
         * with the name of member value.
         * @param name the member name
         */
        public void putMember(Member member){
                members.put(member.getValue(),member);
        }
}

