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

import java.io.PrintWriter;
import java.util.Enumeration;
import java.util.Set;

import alma.tools.idlgen.AcsXmlNamingExpert;

/**
 * This class encapsulates printing of an {@link Interface} in the ACS-xml way.
 * It assumes that the interface is already renamed to end in "J". 
 * <p>
 * We (re-)use the jacorb package <code>org.jacorb.idl</code> to access protected methods, 
 * which is an attempt to not duplicate even more jacorb code.
 * It is a pity that JacORB does not use a level of indirection when instantiating 
 * the Interface objects in CUP$actions, which would allow us to use a custom Interface type.
 * 
 * @author hsommer
 * @author jschwarz
 */
public class AcsInterfacePrinter
{
	private final Interface interfce;
	
	private final Set<AliasTypeSpec> entityTypes;
	private final Set<StructType> xmlAwareStructs;
	private final Set<Interface> xmlAwareIFs;
	private final AcsXmlNamingExpert namingExpert;
	
	public AcsInterfacePrinter(Interface interfce, final Set<AliasTypeSpec> entityTypes, 
			final Set<StructType> xmlAwareStructs, final Set<Interface> xmlAwareIFs, final AcsXmlNamingExpert namingExpert) {
		this.interfce = interfce;
		this.entityTypes = entityTypes;
		this.xmlAwareStructs = xmlAwareStructs;
		this.xmlAwareIFs = xmlAwareIFs;
		this.namingExpert = namingExpert;
	}
	
	/**
	 * Todo:
	 * - Substitute types in operations parameters and returns
	 * 
	 * The code was copied from {@link Interface#printOperations()}
	 * and then modified to not append "Operations" 
	 * (plus "interfce." scope resolution to fix compile errors) 
	 */
	public void printAcsJInterface() {
        PrintWriter ps = interfce.openOutput(interfce.name);
        if (ps == null)
        {
            return;
        }

        interfce.printPackage(ps);
        interfce.printSuperclassImports(ps);
        interfce.printImport(ps);
        
        // TODO: Print ACS comment
        interfce.printClassComment(/*"interface",*/ interfce.name, ps);

        ps.println("public interface " + interfce.name);

        if (interfce.inheritanceSpec.v.size() > 0)
        {
            ps.print("\textends ");
            Enumeration e = interfce.inheritanceSpec.v.elements();

            do
            {
                ScopedName sne = (ScopedName) e.nextElement();

                // See description of abstractInterfaces for logic here.
                if (interfce.abstractInterfaces != null &&
                		interfce.abstractInterfaces.contains(sne.toString()))
                {
                    ps.print(sne);
                }
                else
                {
                    if (sne.resolvedTypeSpec() instanceof ReplyHandlerTypeSpec && parser.generate_ami_callback)
                    {
                        ps.print(sne + "Operations");
                    }
                    else
                    {
                        ConstrTypeSpec ts = unwindTypedefs(sne);
                        ps.print(ts + "Operations");
                    }
                }

                if (e.hasMoreElements())
                {
                    ps.print(" , ");
                }
            }
            while (e.hasMoreElements());

            // TODO-jacorb33
            ps.print(AcsAdapterForOldJacorb.getEnvironmentNL()); //Environment.NL);
        }

        ps.println("{");

        if (interfce.body != null)
        {
            // forward declaration
        	interfce.body.printConstants(ps);
        	
        	// ACS hack
//        	interfce.body.printOperationSignatures(ps);
        	printOperationSignatures(interfce.body, ps);
        	// end ACS hack
        }

        ps.println("}");
        ps.close();

	}
	
    /**
     * Copied from {@link Interface#unwindTypedefs}, without ACS changes except "interfce." resolution.
     * @param scopedName
     * @return
     */
    private ConstrTypeSpec unwindTypedefs(ScopedName scopedName)
    {
        TypeSpec resolvedTSpec = scopedName.resolvedTypeSpec();
        //unwind any typedefs
        while (resolvedTSpec instanceof AliasTypeSpec )
        {
            resolvedTSpec =
                ((AliasTypeSpec)resolvedTSpec).originalType();
        }

        if (! (resolvedTSpec instanceof ConstrTypeSpec))
        {
            if (interfce.logger.isDebugEnabled())
            {
            	interfce.logger.debug("Illegal inheritance spec in Interface.unwindTypeDefs, not a constr. type but " +
                             resolvedTSpec.getClass() + ", name " + scopedName );
            }
            parser.fatal_error("Illegal inheritance spec in Interface.unwindTypeDefs (not a constr. type): " +
            		interfce.inheritanceSpec, interfce.token);
        }

        return (ConstrTypeSpec) resolvedTSpec;
    }

    
    /**
     * Copied from {@link InterfaceBody#printOperationSignatures(PrintWriter)}.
     * We probably don't need this if we can modify the OpDecl objects before generating interface code. 
     * @param ps
     */
    void printOperationSignatures(InterfaceBody ifb, PrintWriter ps )
    {
        if( ifb.v.size() > 0  )
        {
            ps.println( "\t/* operations  */" );
        }

        for( Enumeration<Definition> e = ifb.v.elements(); e.hasMoreElements(); )
        {
            Definition d = e.nextElement();
            if( d.get_declaration() instanceof OpDecl )
            {
//                ( (OpDecl)d.get_declaration() ).printSignature( ps );
            	OpDecl opdecl = (OpDecl)d.get_declaration();
                printSignature( ps, opdecl );
            }
            else if( d.get_declaration() instanceof AttrDecl )
            {
                for( Enumeration m = ( (AttrDecl)d.get_declaration() ).getOperations();
                     m.hasMoreElements(); )
                {
                    ( (Operation)m.nextElement() ).printSignature( ps );
                }
            }
        }
    }
    
    public void printSignature( PrintWriter ps, OpDecl opdecl )
    {
        printSignature( ps, false, opdecl );
    }

    /**
     * @param printModifiers whether "public abstract" should be added
     */
    public void printSignature( PrintWriter ps, boolean printModifiers, OpDecl opdecl )
    {
        ps.print( "\t" );
        if( printModifiers ) 
        	ps.print( "public abstract " );
        else
        	ps.print("public ");

        TypeSpec ts = opdecl.opTypeSpec;
        sanitizeOpNames(ps, opdecl, ts);
		for( Enumeration e = opdecl.paramDecls.elements(); e.hasMoreElements(); )
        {
            printParam( ps, (ParamDecl)e.nextElement() );
            if( e.hasMoreElements() ) ps.print( ", " );
        }

        ps.print( ")" );
        opdecl.raisesExpr.print( ps );
        ps.println( ";" );
    }

	private void sanitizeOpNames(PrintWriter ps, OpDecl opdecl, TypeSpec ts) {
		if (ts.typeSpec() instanceof ConstrTypeSpec) {
			ConstrTypeSpec cts = (ConstrTypeSpec)ts;
			if (cts.c_type_spec instanceof Interface) { // probably don't need this if clause
				ps.print(ts.toString() + " " + opdecl.name + "(");
			} else if (cts.c_type_spec instanceof StructType) {
				StructType stype = (StructType)cts.c_type_spec;
				if (xmlAwareStructs.contains(stype))
					ps.print(namingExpert.getJavaTypeForXmlStruct(stype) + " " + opdecl.name + "(");
				else
					ps.print(ts.toString() + " " + opdecl.name + "(");
			} else
				ps.print(ts.toString() + " " + opdecl.name + "(");
		} else if (ts instanceof AliasTypeSpec && entityTypes.contains(ts)) {
			AliasTypeSpec alias = (AliasTypeSpec)ts;
			ps.print(namingExpert.getJavaTypeForXmlTypedef(alias) + " " + opdecl.name + "(");
		}
		else
			ps.print(ts.toString() + " " + opdecl.name + "(");
	}

		
	    private void printParam( PrintWriter ps, ParamDecl pdecl )
	    {
	    	TypeSpec ts = pdecl.paramTypeSpec;
	        switch( pdecl.paramAttribute )
	        {
	            case ParamDecl.MODE_IN:
	            	sanitizeParamNames(ps, pdecl, ts);
	                //ps.print( pdecl.paramTypeSpec.toString() );
	                break;
	            case ParamDecl.MODE_OUT:
	            case ParamDecl.MODE_INOUT:
	            	if (pdecl.paramTypeSpec instanceof AliasTypeSpec) {
	            		AliasTypeSpec alias = (AliasTypeSpec)pdecl.paramTypeSpec;
	            		String theHolderName = namingExpert.getHolderClassNameForXmlTypedef(alias);
	            		ps.print(theHolderName);
	            	} else
	            		ps.print( pdecl.paramTypeSpec.holderName() ); // TODO replace & merge w/rest of if stmt
	                break;
	        }
	        ps.print(" ");
	        ps.print(pdecl.simple_declarator);
	    }
	    
	    private void sanitizeParamNames(PrintWriter ps, ParamDecl pdecl, TypeSpec ts) {
	    	if (!(entityTypes.contains(ts))) { 
	    		ps.print(ts.toString() + " " + pdecl.name);
	    	} else if (ts.typeSpec() instanceof ConstrTypeSpec && ((ConstrTypeSpec)ts).c_type_spec instanceof Interface) { // TODO: Still broken when type is Interface
	    		ps.print(ts.name() + " " + pdecl.name);
	    	} else {
	    		if (ts instanceof AliasTypeSpec) {
	    			AliasTypeSpec alias = (AliasTypeSpec)ts;
	    			ps.print(namingExpert.getJavaTypeForXmlTypedef(alias) + " " + pdecl.name);
	    		}
	    		else
	    			ps.print(ts.pack_name+"."+ts.name() + " " + pdecl.name);
	    	}
	    }


}
