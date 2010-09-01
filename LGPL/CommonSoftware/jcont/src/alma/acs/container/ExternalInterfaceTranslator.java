/**
 * 
 */
package alma.acs.container;

/**
 * Interface used to distinguish a user-provided interface translator for XML-binded components which uses
 * for certain operations the default interface translator. This interface allows the ContainerServices to
 * retrieve the default interface translator created by the AcsContainer, and thus inject the offshoot
 * implementation/CORBA-object mappings into the component's interface translator.
 * 
 * @author rtobar
 * @since ACS 9.0
 */
public interface ExternalInterfaceTranslator {

	public void setDefaultInterfaceTranslator(Object defaultInterfaceTranslator);

	public Object getDefaultInterfaceTranslator();

}