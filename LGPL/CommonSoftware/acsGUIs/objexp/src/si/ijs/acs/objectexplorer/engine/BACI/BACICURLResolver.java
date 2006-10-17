package si.ijs.acs.objectexplorer.engine.BACI;

/**
 * Insert the type's description here.
 * Creation date: (6/26/2001 12:23:25 PM)
 * @author: 
 */
public final class BACICURLResolver {
	public static final String CURL_PREFIX = "curl://";
	public static final String ROOT_DOMAIN = "Root";
	/**
	 * Insert the method's description here.
	 * Creation date: (6/26/2001 12:24:10 PM)
	 * @param curl java.lang.String
	 * @return java.lang.String
	 */
	public final static java.lang.String resolveDomain(String curl) {
		if (curl==null) return null;
		
		// locate CURL_PREFIX
		int spos = curl.indexOf(CURL_PREFIX);
		
		// no prefix found, assume that contains only name (eg. "Mount1"), so there is no domain
		if (spos==-1)
			return ROOT_DOMAIN;
		spos = CURL_PREFIX.length();
		
		// find first separator '/'
		int lpos = curl.indexOf('/', spos);
		
		// no prefix found, so there is no domain specified, so we have only the name (eg. "Mount1")
		if (lpos==-1)
			return ROOT_DOMAIN;
		
		if (spos==lpos)
			return ROOT_DOMAIN;
		else
			return curl.substring(spos, lpos);
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (6/26/2001 12:24:10 PM)
	 * @param curl java.lang.String
	 * @return java.lang.String
	 */
	public final static java.lang.String resolveName(String curl) {
		if (curl==null) return null;
		
		// locate CURL_PREFIX
		int spos = curl.indexOf(CURL_PREFIX);
		
		// no prefix found, assume that contains only name (eg. "Mount1"), so there is no domain
		if (spos==-1)
			return curl;
		spos = CURL_PREFIX.length();
		
		// find first separator '/'
		int lpos = curl.indexOf('/', spos);
		
		// no prefix found, so there is no domain specified, so we have only the name (eg. "Mount1")
		if (lpos==-1)
			return curl.substring(spos);
		if (lpos==curl.length())
			return null;
		lpos++;
		
		if (spos==lpos)
			return curl.substring(spos);
		else
			return curl.substring(lpos);
	}
	
	/**
	 * 
	 * @param curl
	 * @return
	 */
	public final static String getFirstLevelCurl(String curl) {
		if (curl==null) return null;
		
		// locate CURL_PREFIX
		int spos = curl.indexOf(CURL_PREFIX);
		
		// no prefix found, assume that contains only name (eg. "Mount1"), so there is no domain
		if (spos==-1)
			return curl.split("/",2)[0];
		spos = CURL_PREFIX.length();
		
		// find first separator '/'
		int lpos = curl.indexOf('/', spos);
		
		// no prefix found, so there is no domain specified, so we have only the name (eg. "Mount1")
		if (lpos==-1)
			return curl.substring(spos).split("/",2)[0];;
			if (lpos==curl.length())
				return null;
			lpos++;
			
			if (spos==lpos)
				return curl.substring(0,spos-1) + curl.substring(spos).split("/",2)[0];
			else
				return curl.substring(0,lpos-1) + "/" + curl.substring(lpos).split("/",2)[0];
	}
	
}
