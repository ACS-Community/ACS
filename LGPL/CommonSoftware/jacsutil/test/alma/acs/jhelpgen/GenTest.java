/*
 * Created on Oct 4, 2006 by mschilli
 */
package alma.acs.jhelpgen;

import java.io.File;
import java.util.List;
import java.util.Vector;

import junit.framework.TestCase;
import alma.acs.jhelpgen.Gen.AnchorNode;




public class GenTest extends TestCase {

	final boolean dbg = false;
	
	
	@Override
	public void setUp() {
		System.out.println("================="+getName()+"===================");
	}
	
	// uppercase/lowercase tags
	String H1_a = "<H1> <A name=\"h1_a\"> The h1_a </A> </H1>";
	String H1_b = "<h1> <a name=\"h1_b\"> The h1_b </a> </h1>";
	String H1_c = "<h1> <A name=\"h1_c\"> The h1_c </A> </h1>";

	// single quotes, double quotes, no quotes
	String H2_a = "<h2> <a name=\"h2_a\"> The h2_a </a></h2>";
	String H2_b = "<h2> <a name='h2_b'  > The h2_b </a></h2>";
	String H2_c = "<h2> <a name=h2_c    > The h2_c </a></h2>";
	
	// with/without embedded anchor
	String H3_a = "<h3><a id=\"h3_a\">   The h3_a </a></h3>";
	String H3_b = "<h3>                  The h3_b     </h3>";
	String H3_c = "<h3><a name=\"h3_c\"> The h3_c </a></h3>";
	
	// and some other variations
	String H4_a = "<h4 class=bla><a name=h4_a> The h4_a </a></h4>";
	String Hxy =  "<h1><a name=hxy>The hxy</a></H2>";

	
	String htmlContent = "Some content " +
		H1_a + ", containing " +
		   H2_a + ", containing " +
		      H3_a +
		      H3_b +
		H1_b + " holding " +
		   H2_b + ", having " +
	         H3_c + 
		   H2_c + " having " +
		         H4_a + 
		H1_c + " and stuff ";
 
	
	String[] expectedDom = new String[]{
			"The h1_a",
			   "The h2_a",
			"The h1_b",
			   "The h2_b",
			      "The h3_c",
			   "The h2_c",
			         "The h4_a",
			"The h1_c"}; 

	
	String expectedToc = 
		"<tocitem text=\"The h1_a\" target=\"h1_a\">" +
		"<tocitem text=\"The h2_a\" target=\"h2_a\"/>" +
		"</tocitem>" +
		"<tocitem text=\"The h1_b\" target=\"h1_b\">" +
		"<tocitem text=\"The h2_b\" target=\"h2_b\">" +
		"<tocitem text=\"The h3_c\" target=\"h3_c\"/>" +
		"</tocitem>" +
		"<tocitem text=\"The h2_c\" target=\"h2_c\">" +
		"<tocitem text=\"The h4_a\" target=\"h4_a\"/>" +
		"</tocitem>" +
		"</tocitem>" +
		"<tocitem text=\"The h1_c\" target=\"h1_c\"/>";
	
	
	String expectedMap = 
		"<mapID target=\"h1_a\" url=\"h1_a\"/>" +
		"<mapID target=\"h2_a\" url=\"h2_a\"/>" + 
		"<mapID target=\"h1_b\" url=\"h1_b\"/>" + 
		"<mapID target=\"h2_b\" url=\"h2_b\"/>" + 
		"<mapID target=\"h3_c\" url=\"h3_c\"/>" + 
		"<mapID target=\"h2_c\" url=\"h2_c\"/>" + 
		"<mapID target=\"h4_a\" url=\"h4_a\"/>" + 
		"<mapID target=\"h1_c\" url=\"h1_c\"/>";
	
	
	public void testHtmlToDom() throws Exception {
		Gen gen = new Gen();
		String contents = htmlContent;
		
		Gen.AnchorNode domRoot = gen.htmlToDom(contents);
		
		if (dbg)
			Gui.showTree(domRoot);

		List<AnchorNode> result = domRoot.depthFirst(new Vector<AnchorNode>());
		//	skip first node as it is the root node itself
		result.remove(0);
		
		assertEquals(expectedDom.length, result.size());
		for (int i=0; i<result.size(); i++)
			assertEquals("node #"+i, expectedDom[i], result.get(i).prettyName);
	}	
	
	public void testDomToToc () throws Exception {
		Gen gen = new Gen();
		String contents = htmlContent;

		Gen.AnchorNode domRoot = gen.htmlToDom(contents);
		
		StringBuilder result = new StringBuilder();
		gen.domToToc(domRoot.children, result);
		
		if (dbg) {
			System.out.println(result);
			Thread.sleep(2000);
		}
		
		String result2 = result.toString().replaceAll("\\s*<", "<");
		assertEquals("generated toc", expectedToc, result2);
	}
	
	public void testDomToMap () throws Exception {
		Gen gen = new Gen();
		String contents = htmlContent;

		Gen.AnchorNode domRoot = gen.htmlToDom(contents);
		
		StringBuilder result = new StringBuilder();
		gen.domToMap(domRoot.children, result);

		if (dbg) {
			System.out.println(result);
			Thread.sleep(2000);
		}
		
		String result2 = result.toString().replaceAll("\\s*<", "<");
		assertEquals("generated map", expectedMap, result2);
	}
	
	
}


