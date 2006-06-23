/*
 * @@COPYRIGHT@@
 */
 
/*
 * Copyright (C) The Community OpenORB Project. All rights reserved.
 *
 * This software is published under the terms of The OpenORB Community Software
 * License version 1.0, a copy of which has been included with this distribution
 * in the LICENSE.txt file.
 */

package abeans.models.acs.baci.generator;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Hashtable;

import org.openorb.ReleaseInfo;
import org.openorb.compiler.Configurator;
import org.openorb.compiler.IdlCompiler;
import org.openorb.compiler.object.IdlObject;
import org.openorb.compiler.parser.IdlParser;

/**
 * This class is the IDL to Abeans R3 BACI model compiler implementation.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */

public class IDLtoAbeansCompiler extends IdlCompiler
{

	/**
	 * VERSION.
	 */
	public static final String VERSION = "1.1";

	/**
	 * VERSION.
	 */
	public static final String PRODUCT_NAME = "IDL to Abeans R3 BACI model compiler";

    /**
     * Display a help message
     */
    public static void displayHelpAndExit()
    {
        System.out.println();
        System.out.println( PRODUCT_NAME + " v" + VERSION );
		System.out.println( "based on" );
        System.out.println( "OpenORB IDL To Java Compiler (c) 2002 The Community OpenORB" );
		System.out.println();
        System.out.println( "Options" );
        System.out.println( "\t-all" );
        System.out.println( "\t\tGenerate mapping for included files." );

        System.out.println( "\t-d directory_name" );
        System.out.println( "\t\tProvide a way to specify the ouput dir. This option" );
        System.out.println( "\t\twill not use the 'generated' directory." );
        System.out.println( "\t\tFor example :" );
        System.out.println( "\t\t  abeans.models.acs.baci.generator.IDLtoAbeansCompiler demo.idl -d /home/matej" );

        System.out.println( "\t-D" );
        System.out.println( "\t\tDefine a symbol. It is equivalent to #define." );

        System.out.println( "\t-importLink link" );

		/*
		System.out.println( "\t-dynamic" );
		System.out.println( "\t\tGenerate stub with DII and skeleton with DSI" );
		System.out.println( "\t\t( portable way before CORBA 2.3 )." );

		System.out.println( "\t-minTableSize <size>" );
		System.out.println( "\t\tThe minimum size of method table." );
		*/
		
        System.out.println( "\t-I" );
        System.out.println( "\t\tAllow specification of include directory." );
        System.out.println( "\t\tExample:" );
        System.out.println( "\t\t  abeans.models.acs.baci.generator.IDLtoAbeansCompiler demo.idl -I /home/matej/idl -I ../other" );

        System.out.println( "\t-release" );
        System.out.println( "\t\tShow version number." );

        System.out.println( "\t-silent" );
        System.out.println( "\t\tSuppress any output." );

        System.out.println( "\t-verbose" );
        System.out.println( "\t\tShow debug output." );
        
        System.out.println();
        System.out.println();
        System.exit(1);
    }

	/**
     * Scan command line arguments.
	 * @param args	arguments to be scanned.
     */
    public static void scanArguments(String[] args)
    {
		// set defaults
		map_stub = false;
		map_skeleton = false;
		map_tie = false;
		portableHelper = true;
		jdk1_4 = true;
		
		useSwitch = true;
		useClasses = false;
		
		// unsupported anyway
		useReflection = false;
		
		minTableSize = 3;
		
		// this prefix is very simple but not nice solution
		// it produces: abeans.com.cosylab.ACS.EnumType
		// packageName = "abeans";
		// use_package = true;


        for ( int i = 0; i < args.length; i++ )
        {
            if ( args[ i ].charAt( 0 ) != '-' )
                idl_file_name_list.addElement( args[ i ] );
            else
                if ( args[ i ].equals( "-release" ) )
                {
                    System.out.println( ReleaseInfo.RELEASE );
                    System.exit( 1 );
                }
                else if ( args[ i ].equals( "-h" ) || args[ i ].equals( "-help" ) )
                    displayHelpAndExit();
                else if ( args[ i ].equals( "-silent" ) )
                    silentMode = true;
                else if ( args[ i ].equals( "-verbose" ) )
                    verbose = true;
                /*else if ( args[ i ].equals( "-dynamic" ) )
                    dynamic = true;*/
                else if ( args[ i ].equals( "-all" ) )
                    map_all = true;
/*                else if ( "-invokeMethod".equals( args[i] ) )
                {
                    if ( ( i + 1 ) == args.length )
                    {
                        System.out.println( "Argument expected after '-invokeMethod'" );
                        System.exit( 2 );
                    }
                    i++;
                    useReflection = false;
                    useSwitch = false;
                    useClasses = false;
                    
                    if ( "Classes".equals( args[i] ) )
                    {
                        useClasses = true;
                    }
                    else if ( "Reflection".equals( args[i] ) )
                    {
                        useReflection = true;
                    }
                    else if ( "Switch".equals( args[i] ) )
                    {
                        useSwitch = true;
                    }
                    else
                    {
                        System.out.println( "'-invokeMethod' support arguments: " + 
                                "'Classes', 'Switch' and 'Reflection'" );
                        System.exit( 2 );
                    }
                }
                else if ( "-minTableSize".equals( args[i] ) )
                {
                    if ( i + 1 == args.length )
                    {
                        System.out.println( "Argument expected after '-minTableSize'" );
                        System.exit( 2 );
                    }

                    minTableSize = Integer.parseInt(args[ ++i ]);
                }*/
                else if ( args[ i ].equals( "-importlink" ) )
                {
                    if ( i + 1 == args.length )
                    {
                        System.out.println( "Argument expected after '-importlink'" );
                        System.exit( 2 );
                    }

                    importLink.addElement( args[ ++i ] );
                }
                else if ( args[ i ].equals( "-d" ) )
                {
                    if ( i + 1 == args.length )
                    {
                        System.out.println( "Argument expected after '-d'" );
                        System.exit( 2 );
                    }

                    outdir = args[ ++i ];

                    if ( packageName == null )
                    {
                        packageName = "";
                        use_package = false;
                    }
                }
                else if ( args[ i ].equals( "-I" ) )
                {
                    if ( ++i == args.length )
                    {
                        System.out.println( "Argument expected after '-I'" );
                        System.exit( 2 );
                    }

                    URL url = null;

                    try
                    {
                        url = new URL( args[ i ] );
                    }
                    catch ( MalformedURLException ex )
                    {
                        try
                        {
                            url = new java.io.File( args[ i ] ).toURL();
                        }
                        catch ( MalformedURLException ex1 )
                        {}

                    }

                    if ( url != null )
                        includeList.addElement( url );
                }
                else if ( args[ i ].startsWith( "-I" ) )
                {
                    String path = args[ i ].substring( 2 );

                    URL url = null;

                    try
                    {
                        url = new URL( path );
                    }
                    catch ( MalformedURLException ex )
                    {
                        try
                        {
                            url = new java.io.File( path ).toURL();
                        }
                        catch ( MalformedURLException ex1 )
                        {}

                    }

                    if ( url != null )
                        includeList.addElement( url );
                }
                else if ( args[ i ].startsWith( "-D" ) )
                {
                    try
                    {
                        int idx = args[ i ].indexOf( '=' );

                        if ( idx < 0 )
                            definedMacros.put( args[ i ].substring( 2, args[ i ].length() ) , "" );
                        else
                            definedMacros.put( args[ i ].substring( 2, idx ) , args[ i ].substring( idx + 1 ) );
                    }
                    catch ( StringIndexOutOfBoundsException ex )
                    { }

                }
                else if ( displayBadFlag )
                {
                    System.out.println( "Bad parameter : " + args[ i ] );
                    displayHelpAndExit();
                }
        }

        if ( outdir == null || outdir.equals( "" ) )
        {
            outdir = new String( "generated" );

            if ( packageName == null )
            {
                packageName = "";
                use_package = false;
            }
        }

        idl_file_name = new String[ idl_file_name_list.size() ];

        for ( int i = 0; i < idl_file_name_list.size(); i++ )
            idl_file_name[ i ] = ( String ) idl_file_name_list.elementAt( i );
    }

    /**
     * Get arguments from configuration file.
     */
    public static void configFile()
    {
        Configurator config = null;

        try
        {
            java.lang.Object obj = Thread.currentThread().getContextClassLoader().loadClass( "org.openorb.compiler.orb.DefaultConfigurator" ).newInstance();
            config = (org.openorb.compiler.Configurator)obj;
        }
        catch ( java.lang.Exception ex )
        {
        	// do not complain
            return;
        }
        
		// update
        config.updateInfo( includeList, importLink );
    }

    /**
     * Prepare the compilation process.
     */
    public static void prepare()
    {
        // define some standard macros
        definedMacros.put( "__IDL_TO_JAVA__", "" );
        definedMacros.put( "__OPENORB__", ReleaseInfo.RELEASE );
        definedMacros.put( "__OPENORB_MAJOR__", String.valueOf(ReleaseInfo.VERSION_MAJOR) );
        definedMacros.put( "__OPENORB_MINOR__", String.valueOf(ReleaseInfo.VERSION_MINOR) );
        definedMacros.put( "__OPENORB_MINOR_CHANGE__", String.valueOf(ReleaseInfo.VERSION_MINOR_CHANGE) );
        definedMacros.put( "__CORBA_IDL__", ReleaseInfo.SPEC_VERSION_MAJOR + "." + ReleaseInfo.SPEC_VERSION_MINOR );
        definedMacros.put( "__CORBA_MAJOR__", String.valueOf(ReleaseInfo.SPEC_VERSION_MAJOR) );
        definedMacros.put( "__CORBA_MINOR__", String.valueOf(ReleaseInfo.SPEC_VERSION_MINOR) );
        definedMacros.put( "__CORBA_MINOR_CHANGE__", String.valueOf(ReleaseInfo.SPEC_VERSION) );
    }

    /**
     * This operation is used to compile an IDL file.
	 * @param fileName	file to be compiled.
	 * @param parser	parser to be used to parse a file.
	 * @throws org.openorb.compiler.parser.CompilationException
	 */
    public static void compile( String fileName, IdlParser parser )
	    throws org.openorb.compiler.parser.CompilationException
    {
		if ( !silentMode )
			System.out.println( "compile : " + fileName);

		if ( verbose )
			System.out.println( "parsing...");

		// start compilation (parsing)
        IdlObject compilationGraph = parser.compile_idl( fileName );

		// check for errors
        if ( IdlParser.totalError != 0 )
			throw new org.openorb.compiler.parser.CompilationException();

		if ( verbose )
			System.out.println( " done.");

        // -- Start to generate Java code --

        if ( verbose )
            System.out.println( "translating to Abeans R3 model..." );

		// create translator
        IDLtoAbeans translator = new IDLtoAbeans();

		// and translate...
		translator.translateData( compilationGraph, packageName );
		translator.translateBean( compilationGraph, packageName );

		if ( verbose )
			System.out.println( " done.");

    }

	/**
	 * Creates a new IDL parser.
	 * @param args	arguments.
	 * @return parser instance.
	 */
    public static IdlParser createIDLParser(String[] args)
    {
    	// prepare for compilation
        prepare();

		// scan and setup configuration
        scanArguments(args);

		// also check for configuration file
        configFile();

		// after configuration is set
		// return parser instance
        return new IdlParser();
    }

    /**
     * The IDL to Abeans R3 BACI model application entry point.
     * @param args  command line arguments list
     */
    public static void main(String[] args)
    {

		// check arguments
        if (args.length==0)
            displayHelpAndExit();

		// create OpenORB IDL parser
        IdlParser parser = createIDLParser( args );

		// no IDL file(s) ?!
        if (idl_file_name.length==0)
            displayHelpAndExit();

        // welcome message
        if ( !silentMode )
            System.out.println( PRODUCT_NAME + " based on OpenORB IDL To Java Compiler" );

		//
        // compile
		//
        try
        {
        	// make a copy of macros, so that every new file gets clean set
            Hashtable definedCopy = (Hashtable)definedMacros.clone();

			// compile only one
            for ( int i = 0; i < 1 /*idl_file_name.length*/; i++ )
            {
				// setup macros
                definedMacros = definedCopy;
                
				// start compilation
                compile( idl_file_name[i], parser );
				
            }
        }
        catch (org.openorb.compiler.parser.CompilationException ex)
        {
            System.out.println( "Error(s) detected, compilation process stopped..." );
            System.exit(2);
        }

		if ( !silentMode )
			System.out.println( "done." );
    }
}
