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
package alma.acs.eventbrowser.views;

import java.util.ArrayList;

import org.eclipse.core.runtime.IProgressMonitor;
import org.omg.CORBA.Any;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.DynamicAny.DynAny;
import org.omg.DynamicAny.DynAnyFactory;
import org.omg.DynamicAny.DynEnum;
import org.omg.DynamicAny.DynSequence;
import org.omg.DynamicAny.DynStruct;
import org.omg.DynamicAny.NameValuePair;
import org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;

import alma.acs.eventbrowser.model.EventModel;
import alma.acs.util.UTCUtility;

public class DynAnyParser {

	private static final boolean DEBUG = false;
	private final DynAnyFactory daFactory;
	private DynAny dynAny = null;
	private ArrayList<ParsedAnyData> pdlist = new ArrayList<ParsedAnyData>(100);
	private String eventName;
	private IProgressMonitor monitor;
	private boolean cancelFlag = false;

	public DynAnyParser(Any anyToParse, String eventName) {
		daFactory = EventModel.getDynAnyFactory();
		try {
			dynAny = daFactory.create_dyn_any(anyToParse);
			this.eventName = eventName;
		} catch (InconsistentTypeCode e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ClassCastException e) {
			e.printStackTrace();
		}
	}

	public ParsedAnyData[] getParsedResults(IProgressMonitor monitor) {
		this.monitor = monitor;
		if (cancelFlag  == true) return null;
		parseEventAny(dynAny, "");
		return pdlist.toArray(new ParsedAnyData[0]);
	}

	private void parseEventAny(DynAny dynAny2, String path) {
		if (monitor != null && monitor.isCanceled()) {
			cancelFlag = true;
			return; // Check for cancellation each time through
		}
		DynAny da = dynAny2;
		int tcKind = da.type().kind().value();
		ParsedAnyData entry = new ParsedAnyData(path, "", "");

		try {
			switch (tcKind) {

			case TCKind._tk_short:
				entry.setType("short");
				entry.setValue(String.valueOf(da.get_short()));
				pdlist.add(entry);
				break;
			case TCKind._tk_long:
				entry.setType("long");
				entry.setValue(String.valueOf(da.get_long()));
				pdlist.add(entry);
				break;
			case TCKind._tk_longlong:
				entry.setType("longlong");
				if (path.contains("timestamp")) {
					entry.setValue(String.valueOf(UTCUtility.getUTCDate(UTCUtility.utcOmgToJava(da.get_longlong()))));
				}
				else
					entry.setValue(String.valueOf(da.get_longlong()));
				pdlist.add(entry);
				break;
			case TCKind._tk_ulonglong:
				entry.setType("ulonglong");
				entry.setValue(String.valueOf(da.get_ulonglong()));
				pdlist.add(entry);
				break;
			case TCKind._tk_string:
				entry.setType("string");
				entry.setValue(da.get_string());
				pdlist.add(entry);
				break;
			case TCKind._tk_boolean:
				entry.setType("boolean");
				entry.setValue("" + da.get_boolean());
				pdlist.add(entry);
				break;
			case TCKind._tk_float:
				entry.setType("float");
				entry.setValue("" + da.get_float());
				pdlist.add(entry);
				break;
			case TCKind._tk_double:
				entry.setType("double");
				entry.setValue("" + da.get_double());
				pdlist.add(entry);
				break;
			case TCKind._tk_enum:
				entry.setType("enum");
				entry.setValue(((DynEnum) da).get_as_string());
				pdlist.add(entry);
				break;
			case TCKind._tk_array:
				entry.setType("array");
				entry.setValue("size: " + da.component_count());
				pdlist.add(entry);
				int numDisplayElements = Math.min(da.component_count(), 5);
				int elementType = da.type().content_type().kind().value();
				switch (elementType) {
				case TCKind._tk_double:
					for (int j = 0; j < numDisplayElements; j++) {
						String dname = path + "[" + j + "]";
						double value = da.current_component().get_double();
						pdlist.add(new ParsedAnyData(dname, "double",
								("" + value)));
						da.next();
					}
					break;
				default:
					pdlist.add(new ParsedAnyData(path,
							"Unimplemented type for array: " + elementType,
							"Unknown"));
				}
				break;
			case TCKind._tk_struct:
			case TCKind._tk_except:
				DynStruct ds = (DynStruct) da;
				String structName = ds.type().name();
				if (DEBUG)
					System.out.println("Struct name: " + structName);
				entry.setType("struct");
				if (path.equals("")) {
					entry.setName(structName);
				} else
					entry.setName(path + " / " + structName); // TODO: still not
																// working --
																// this doesn't
																// display
																// Actuator
																// Space at all!

				StringBuilder members = new StringBuilder("Members: ");
				for (int i = 0; i < ds.component_count(); i++) {
					members.append(ds.current_member_name()
							+ ((i == ds.component_count() - 1) ? " " : ", "));
					ds.next();
				}
				entry.setValue(members.toString());
				pdlist.add(entry);
				ds.rewind();

				for (int i = 0; i < ds.component_count(); i++) {
					String dname = ds.current_member_name();
					if (DEBUG)
						System.out.println("\tMember name: " + dname
								+ " type: "
								+ ds.current_component().type().kind().value());
					parseEventAny(ds.current_component(), dname);
					ds.next();
				}

				if (DEBUG) {
					NameValuePair[] nvp = ds.get_members();

					for (int i = 0; i < nvp.length; i++) {
						System.out.println(nvp[i].id + " " + nvp[i].value);
					}
				}
				break;

			case TCKind._tk_sequence:
				DynSequence dsq = (DynSequence) da;
				String seqName = path + dsq.type().name();
				entry.setType("sequence" + seqName); // TODO: Straighten this
														// out
				entry.setValue("IDL sequences not implemented yet");

			default:
				entry.setType(da.type().kind().toString());
				entry.setValue("Unimplemented type");
				pdlist.add(entry);
				break;
			}
		} catch (TypeMismatch e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvalidValue e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (BadKind e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return;
	}

}
