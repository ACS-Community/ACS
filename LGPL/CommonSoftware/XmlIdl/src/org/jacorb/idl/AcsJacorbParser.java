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
package org.jacorb.idl;

import java.io.IOException;

import org.jacorb.idl.ConstDecl;
import org.jacorb.idl.GlobalInputStream;
import org.jacorb.idl.NameTable;
import org.jacorb.idl.TypeMap;
import org.jacorb.idl.lexer;
import org.jacorb.idl.parser;

import alma.tools.idlgen.JacorbVisitor;

/**
 * Wrapper of {@link org.jacorb.idl.parser}.
 * 
 * @author hsommer
 */
public class AcsJacorbParser extends parser
{
	public String idlFile;

	/**
	 * We do not reuse the option parsing from {@link org.jacorb.idl.parser#compile(String[])}
	 * because that comes bundled together with IDL parsing, which in turn calls cleanup() in the end 
	 * and thus renders our parser useless for later code generation. 
	 * This dependency of code generation on the parser comes from the jacorb IDL tree node classes 
	 * that reference some static parser fields.
	 * <p>
	 * Note that if we would run all code generation from inside class JacorbVisitor then 
	 * parser cleanup would be no problem (everything would run before the cleanup call), 
	 * but it seems nicer to separate off the code generation from the parse tree analysis.
	 * @param args
	 * @param jacorbVisitor
	 * @throws IOException
	 */
	public void init(String[] args, JacorbVisitor jacorbVisitor) throws IOException {
		
		initLogging();
		
		for (int i = 0; i < args.length; i++) {
			if (args[i].equals("-d")) {
				if (i+1 < args.length && args[i+1].charAt(0) != '-') {
					parser.out_dir = args[++i];
				}
			}
			else if (args[i].equals("-W")) {
				if (i + 1 == args.length || args[i + 1].charAt(0) == '-') {
					usage(args, "-W");
				}
				int level = Integer.parseInt(args[++i]);
				getLogger().setPriority(Environment.intToPriority(level));
			}
			else if (args[i].startsWith("-I")) {
				GlobalInputStream.setIncludePath(args[i].substring(2));
			}
			else if (args[i].endsWith(".idl")) {
				if (idlFile == null && i == args.length - 1) {
					idlFile = args[i];
				}
				else {
					throw new IllegalArgumentException("A single IDL file is expected as the last argument. Found '" + args[i] + "' instead.");
				}
			}
			else {
				throw new IllegalArgumentException("Unexpected argument " + args[i]);
			}
		}
		
		// Hook up our plugin. This corresponds to the "-backend" option.
		parser.setGenerator(jacorbVisitor);

		// The following option gets added to JacORB by an ACS patch
		// and must be set for the xml code generation 
		// to use the IDL's pragma prefix for Java package names.
		parser.auto_prefix = true;
		
		// While debugging this relieves us from clearing the src_generated directory between runs.
		// In operational use we currently do not foresee multiple invocations, so that this options should be neutral.
		parser.forceOverwrite = true;
		
		// Other stuff, see org.jacorb.idl.JacIDL#execute() or org.jacorb.idl.parser.prepareAndParse(String)
		// Perhaps some is not needed since we call main() only for one IDL file.
		
		GlobalInputStream.init();
		GlobalInputStream.setInput(idlFile);
		lexer.reset();
		NameTable.init();
		ConstDecl.init();
		TypeMap.init();
		openScope();
		
		// Standard compiler defines
		lexer.define("JACORB", "1");
		lexer.define("_PRE_3_0_COMPILER_", "1");
		lexer.define("GIOP_1_1", "1");
		lexer.define("GIOP_1_2", "1");
	}
}
