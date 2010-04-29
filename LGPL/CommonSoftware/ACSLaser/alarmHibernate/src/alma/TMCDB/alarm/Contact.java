/**
 * 
 */
package alma.TMCDB.alarm;

/**
 * @author msekoranja
 *
 */
public class Contact {

	private String name;
	private String email;
	private String gsm;
	
	public Contact(String name, String email, String gsm) {
		this.name = name;
		this.email = email;
		this.gsm = gsm;
	}

	/**
	 * @return the email
	 */
	public String getEmail() {
		return email;
	}
	/**
	 * @return the gsm
	 */
	public String getGsm() {
		return gsm;
	}
	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}
	
	
}
