package alma.tools.entitybuilder;

import java.io.File;
import java.util.ArrayList;

import junit.framework.TestCase;

public class CastorBuilderTest extends TestCase {

	private CastorBuilder builder;
	
	protected void setUp() throws Exception {
//        assertEquals("test", System.getProperty("user.dir").subs
		builder = new CastorBuilder();
	}

	protected void tearDown() throws Exception {
	}

	public void _testBuildFromSchema() throws Exception {
        File schemaDir = new File("../idl/");
        File primaryConfigFile = new File(schemaDir, "TestBindingConfig.xml");
        File javaOutputDir = new File("generator_out");
        builder.run(schemaDir, primaryConfigFile, new ArrayList<String>(), new ArrayList<File>(), javaOutputDir);
    }
    
    public void testBuildFromSchemaInOtherDir() throws Exception {
        File schemaDir = new File("../idl/");
        File primaryConfigFile = new File("../idl/TestBindingConfig.xml");
        File javaOutputDir = new File("generator_out");
        builder.run(schemaDir, primaryConfigFile, new ArrayList<String>(), new ArrayList<File>(), javaOutputDir);
    }
    
}
