/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.util;

import java.util.Hashtable;
import java.util.Vector;

/**
 * Code provided by msekoran, mschilli added method parse(String). 
 */
public class IorParser {

	private IorParser() {}

	//
	// ==================== Sample usage ========================
	//
	/**
	 * Prints info on the specified IOR. If no IOR is given, a demo IOR will be used.
	 */
	public static void main (String[] args) {
		// String defaultIOR =
		// "IOR:012020202100000049444c3a434f5242416e65742f526f6f6d496e666f726d6174696f6e3a312e3000202020020000000153495670000000010101200500000073756e6700202020d7000000010000004e00000001504d43000000002100000049444c3a434f5242416e65742f526f6f6d496e666f726d6174696f6e3a312e30002020201a000000564953494f5242202d20494f4e414f52422054455354494e4700202000000000000000006a000000010100200c0000003139322e3136382e312e3300600420204e00000001504d43000000002100000049444c3a434f5242416e65742f526f6f6d496e666f726d6174696f6e3a312e30002020201a000000564953494f5242202d20494f4e414f52422054455354494e4700";
		String maciManagerIOR = "IOR:000000000000001C49444C3A696A732E73692F6D6163692F4D616E616765723A312E300000000002000000000000006C000102000000000E3133342E3137312E34302E3139000BB8000000164F52422F4D616E61676572504F412F4D616E616765720000000000020000000000000008000000004A414300000000010000001C00000000000100010000000105010001000101090000000105010001000000010000002C0000000000000001000000010000001C00000000000100010000000105010001000101090000000105010001";

		if (args.length == 0) {
			System.err.println("Usage: (this) IOR:0123456789...");
			System.exit(2);
		}
		String hexIOR = args[0];
		new IOR(hexIOR).print();

		String[] result = parse(hexIOR);
		System.out.println("\n host / port: " + result[0] + " / " + result[1]);
	}

	//
	// ========================= API ==============================
	//

	/**
	 * Parses the specified hex-IOR and returns info about it. If parsing fails for
	 * any reason (be it even an algorithmic bug), an IllegalArgumentException will be thrown.
	 * 
	 * @return a string array (host, port)
	 * @throws IllegalArgumentException If parsing fails
	 */
	public static String[] parse (String hexIOR) throws IllegalArgumentException {

		String host = null;
		String port = null;

		IOR ior = null;

		try {
			ior = new IOR(hexIOR);

		} catch (Exception exc) {
				throw new IllegalArgumentException("parsing failed: " + exc);
		}


		if (ior == null || ior._profiles == null) {
			throw new IllegalArgumentException("parsing failed for unknown reason");
		}

		try {		
			
			// ior._profiles vector = (tag, hashtable, tag, hashtable, ...)
			
			for (int i = 0; i < ior._numProfiles; i++) {
				int tag = ((Long) ior._profiles.elementAt(i * 2)).intValue();
				Hashtable profile_data = (Hashtable) ior._profiles.elementAt(i * 2 + 1);

				if (tag == IOR.TAG_INTERNET_IOP) {
					host = String.valueOf(profile_data.get("ProfileBody.host"));
					port = String.valueOf(profile_data.get("ProfileBody.port"));
					break;
				}
			}
			
		} catch (Exception exc) {
			throw new IllegalArgumentException("parsing failed: " + exc);
		}

		if (host == null || port == null) {
			throw new IllegalArgumentException("parsing failed, host or port not found in IOR");
		}
		
		return new String[]{host, port};
	}

	//
	// ============================================================
	//

	private static class CDR {

		protected boolean _byteOrder;
		protected int _byteLen, _bytePos = 1;
		protected char[] _CDRArray;

		public char getChar () {
			return (char) _CDRArray[_bytePos++];
		}

		public long getULong () {
			skip(4);

			return getBytes(4);
		}

		public int getUShort () {
			skip(2);

			return (int) getBytes(2);
		}

		public char[] getSequence () {
			char[] sequence;
			double seqLen;

			seqLen = getULong();
			sequence = new char[(int) seqLen];
			for (int i = 0; i < seqLen; i++)
				sequence[i] = _CDRArray[_bytePos++];
			return sequence;
		}

		public String getString () {
			// In IDL an unsigned long is a 32-bit unsigned integer. In Java a long
			// is a 64-bit signed two's-complement integer.

			double stringLen;

			stringLen = getULong();
			_bytePos += (int) stringLen;
			return new String(_CDRArray, _bytePos - (int) stringLen, (int) stringLen - 1);
		}

		public void parseByteArray (char[] sequence) {
			_CDRArray = sequence;
			init();
		}

		public void parseHexString (String hexCDRString) {
			char cdrByte;

			_byteLen = hexCDRString.length() / 2;
			_CDRArray = new char[_byteLen];
			for (int i = 0; i < _byteLen * 2; i += 2) {
				cdrByte = hex2dual(hexCDRString.charAt(i));
				cdrByte = (char) (cdrByte << 4);
				cdrByte |= hex2dual(hexCDRString.charAt(i + 1));
				_CDRArray[i / 2] = cdrByte;
			}
			init();
		}

		public String toHexString () {
			return new String();
		}

		// Protected methods.

		protected long getBytes (int count) {
			// Copy the octets depending on the server's byte order. Java Virtual
			// Machine uses ordering in big-endian (IOR flag is 0 = false).

			long buff = 0;

			if (_byteOrder) {
				// Server's ordering is little-endian (IOR flag is 1 = true).

				for (int i = count - 1; i >= 0; i--) {
					buff += _CDRArray[_bytePos + i];
					if (i != 0)
						buff = (buff << 8);
				}
				_bytePos += count;
				/*
				 * for ( int i = 0; i < count; i++) ((long) Math.pow(256, i));
				 */
			} else {
				// Server's ordering is big-endian (IOR flag is 0 = false).

				for (int i = 0; i < count; i++) {
					buff += _CDRArray[_bytePos++];
					if (i != count - 1)
						buff = (buff << 8);
				}
				/*
				 * for ( int i = 0; i < count; i++) ((long) Math.pow(256, count-i-1));
				 */
			}
			return buff;
		}

		protected char hex2dual (char hc) {
			char dual = 0;

			if (Character.isDigit(hc))
				dual = (char) (new Integer("" + hc).intValue());
			else {
				hc = Character.toLowerCase(hc);

				if (hc == 'a')
					dual = 10;
				if (hc == 'b')
					dual = 11;
				if (hc == 'c')
					dual = 12;
				if (hc == 'd')
					dual = 13;
				if (hc == 'e')
					dual = 14;
				if (hc == 'f')
					dual = 15;
			}
			return dual;
		}

		protected void init () {
			if (_CDRArray[0] == 0)
				_byteOrder = false;
			else
				_byteOrder = true;
		}

		protected void skip (int alignment) {
			double remainder;

			remainder = _bytePos % alignment;
			if (remainder > 0)
				_bytePos += (alignment - (int) remainder);
		}
	}

	private static class IOR {

		// Module IOP.

		public static final long TAG_INTERNET_IOP = 0;
		public static final long TAG_MULTIPLE_COMPONENTS = 1;

		protected long _numProfiles;

		protected String _iorString, _prefix, // IOP::IOR.
				_type_id; // IOP::IOR.

		// IOP::IOR. This vector contains the tags (ProfileId) and the profile_data
		// (octet sequences).

		protected Vector _profiles = null;

		public IOR(String iorString) {
			_iorString = new String(iorString);
			parse();
		}

		public long getNumProfiles () {
			return _numProfiles;
		}

		public String getProfileData (long profileNo, String profileItem) {
			String profile_item;
			Hashtable profile_data;

			profile_data = (Hashtable) _profiles.elementAt((int) profileNo * 2 + 1);
			profile_item = profile_data.get(profileItem).toString();

			return profile_item;
		}

		public int getProfileTag (long profileNo) {
			int tag;
			Long tagObj;

			tagObj = (Long) _profiles.elementAt((int) profileNo * 2);
			tag = tagObj.intValue();

			return tag;
		}

		public String getTypeId () {
			return _type_id;
		}

		public void print () {
			if (_profiles != null) {
				int tag;
				Hashtable profile_data;

				System.out.println("Type ID           : " + _type_id);
				System.out.println("Number of profiles: " + _numProfiles);
				for (int i = 0; i < _numProfiles; i++) {
					System.out.println("Profile No. " + (i + 1));
					tag = ((Long) _profiles.elementAt(i * 2)).intValue();
					profile_data = (Hashtable) _profiles.elementAt(i * 2 + 1);
					switch (tag) {
						case (int) TAG_INTERNET_IOP :
							System.out.println("  Tag               : TAG_INTERNET_IOP");
							System.out.println("  Version.major     : " + profile_data.get("Version.major"));
							System.out.println("  Version.minor     : " + profile_data.get("Version.minor"));
							System.out.println("  Host              : " + profile_data.get("ProfileBody.host"));
							System.out.println("  Port              : " + profile_data.get("ProfileBody.port"));
							System.out.println("  Object key        : " + profile_data.get("ProfileBody.object_key"));
							break;
						case (int) TAG_MULTIPLE_COMPONENTS :
							System.out.println("  Tag               : TAG_MULTIPLE_COMPONENTS");
							System.out.println("  Profile data      : " + profile_data.get("ProfileData"));
							break;
						default :
							System.out.println("  New tag           : " + tag);
							System.out.println("  Profile data      : " + profile_data.get("ProfileData"));
							break;
					}
				}
			}
		}

		protected void parse () {
			long tag;

			// IIOP::Version and IIOP::ProfileBody. This hashtable contains the
			// version (char major, char minor), the host (string), the port
			// (unsigned short) and the object_key (octet sequences).

			Hashtable profile_data;
			CDR iorCdr;
			CDR profileCdr;

			_prefix = _iorString.substring(0, 4);
			iorCdr = new CDR();
			iorCdr.parseHexString(_iorString.substring(4));
			_type_id = iorCdr.getString();
			_numProfiles = iorCdr.getULong();
			// Store the tags and the profile_data sequences from IOP::TaggedProfile
			// in the Vector _profiles.
			_profiles = new Vector((int) _numProfiles * 2);
			for (int i = 0; i < _numProfiles; i++) {
				tag = iorCdr.getULong();
				_profiles.addElement(new Long(tag));
				profile_data = new Hashtable(5);
				if (tag == TAG_INTERNET_IOP) {
					profileCdr = new CDR();
					// In future use ByteArrays instead of Strings.

					profileCdr.parseByteArray(iorCdr.getSequence());

					profile_data.put("Version.major", new Integer(profileCdr.getChar()));
					profile_data.put("Version.minor", new Integer(profileCdr.getChar()));
					profile_data.put("ProfileBody.host", profileCdr.getString());
					profile_data.put("ProfileBody.port", new Integer(profileCdr.getUShort()));
					profile_data.put("ProfileBody.object_key", new String(profileCdr.getSequence()));
				} else
					// If a MultipleComponentProfile or a profile with a new tag was
					// discovered, skip the following sequence.
					profile_data.put("ProfileData", new String(iorCdr.getSequence()));
				_profiles.addElement(profile_data);
			}
		}
	}

}

// //////////////////////////////////////////////////////
// / ------------------- API ------------------------ ///
// //////////////////////////////////////////////////////

// //////////////////////////////////////////////////////
// / ----------------- Internal --------------------- ///
// //////////////////////////////////////////////////////

//
//
//
//
//
//
//
//
//
//
//
//