package alma.acs.container.corba;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import org.omg.CORBA.portable.IDLEntity;

/**
 * This class finds illegal null values in corba parameters or return values, such as null strings inside structs.
 * It uses java reflection to navigate the possibly nested data.
 * <p>
 * Motivation: On the server side of a corba call, the implementation cannot catch and evaluate the resulting 
 * marshalling or NullPointer exceptions, which are often very unclear about the cause of the problem.
 * Thus to help with debugging, this class can be used to analyze return values or out parameters,
 * and report possible problems in an understandable way.
 * Alternatively, this class can also be used on the client side of a corba call, although it will be less useful there,
 * because marshalling errors for client data produces rather tangible exceptions. 
 * <p>
 * One CorbaNullFinder object must be constructed for every corba parameter / return value to be checked.
 * Then calling the methods {@link #hasErrors()} or {@link #getErrors()} will run the check 
 * and report the result.
 * 
 * @author hsommer
 * @since ACS 9.0 (see )
 */
public class CorbaNullFinder
{
	private final Object corbaData;
	private List<String> errors;
	
	/**
	 * Constructor from the top-level corba parameter or return value. 
	 * Must not be used for corba object references (e.g. offshoot references), which may be null,
	 * but cannot be distinguished from illegal null structs etc.
	 */
	public CorbaNullFinder(Object corbaData) {
		this.corbaData = corbaData;
	}
	
	/**
	 * The actual worker method, called by {@link #hasErrors()} or {@link #getErrors()}.
	 */
	synchronized private void checkForNulls() {
		if (errors == null) {
			errors = new ArrayList<String>();
			if (corbaData != null && !corbaData.getClass().getPackage().getName().startsWith("java")) {
				recursiveCheckForNulls(corbaData, corbaData.getClass().getSimpleName());
			}
			else {
				errors.add("Top-level object is null; cannot distinguish between a legal null object reference and an illegal null data item.");
			}
		}
	}
	
	/**
	 * @param _corbaData
	 * @param path
	 */
	private void recursiveCheckForNulls(Object _corbaData, String path) {
		
		if (_corbaData == null) {
			// happens when array members are null
			errors.add("Null object in field " + path);
			return;
		}
		
		Field[] fields = _corbaData.getClass().getFields();
		for (Field field : fields) {
			Class<?> clzz = field.getType();
			String qualifiedFieldName = path + "/" + field.getName();
			try {
				Object value = field.get(_corbaData);
				if (clzz.isPrimitive()) {
					// nothing to do 
				}
				else if (clzz == String.class) {
					if (value == null) {
						errors.add("Null string in field " + qualifiedFieldName);
					}
				}
				else if (isIDLEnumClass(clzz)) {
					if (value == null) {
						errors.add("Null enum in field " + qualifiedFieldName);
					}
				}
				else if (isIDLStructClass(clzz)) {
					if (value == null) {
						errors.add("Null struct in field " + qualifiedFieldName);
					}
					else {
						recursiveCheckForNulls(value, qualifiedFieldName);
					}
				}
				else if (clzz.isArray()) {
					if (value == null) {
						errors.add("Null array in field " + qualifiedFieldName);
					}
					else {
						// null element check 
						for (int i = 0; i < Array.getLength(value); i++) {
							recursiveCheckForNulls(Array.get(value, i), qualifiedFieldName + "[" + i + "]");
						}
					}
				}
				else if (!isIDLInterfaceClass(clzz)) {
					// @TODO: check test output, and eventually remove this println!
					System.out.println("DEBUG: Check if we need to update " + CorbaNullFinder.class.getName() + " to support " + clzz.getName() 
							+ " used in " + qualifiedFieldName);
				}
			} catch (Exception ex) {
				errors.add("Failed to read field of type " + clzz.getName());
			}
		}
	}
	
	static boolean isIDLEnumClass(Class<?> clzz) {
		if (IDLEntity.class.isAssignableFrom(clzz) && 
				!org.omg.CORBA.Object.class.isAssignableFrom(clzz)) {
			try {
				clzz.getMethod("from_int", int.class); // specified in IDL-to-Java mapping spec chapter 1.7
				return true; // an IDL enum!
			} catch (Exception ex) {
				return false; // an IDL struct 
			}
		} 
		else {
			return false; // maybe a String, or an IDL interface
		}
	}
	
	static boolean isIDLStructClass(Class<?> clzz) {
		if (IDLEntity.class.isAssignableFrom(clzz) && 
				!org.omg.CORBA.Object.class.isAssignableFrom(clzz)) {
			try {
				clzz.getMethod("from_int", int.class);
				return false; // an IDL enum
			} catch (Exception ex) {
				return true; // an IDL struct 
			}
		} 
		else {
			return false; // maybe a String, or an IDL interface
		}
	}
	
	public static boolean isIDLInterfaceClass(Class<?> clzz) {
		return ( IDLEntity.class.isAssignableFrom(clzz) && org.omg.CORBA.Object.class.isAssignableFrom(clzz) );
	}
	
	
	/**
	 * @return true if there were errors in the corba data passed to the constructor, false otherwise.
	 */
	public boolean hasErrors() {
		checkForNulls();
		return errors.size() > 0;
	}
	
	/**
	 * Gets the textual description of the errors found, including the pathname of the affected data item(s).
	 * @return List of errors found, or empty list if there are no errors.
	 */
	public List<String> getErrors() {
		checkForNulls();
		return new ArrayList<String>(errors);
	}
}
