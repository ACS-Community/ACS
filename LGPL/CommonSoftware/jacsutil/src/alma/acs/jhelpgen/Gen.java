/*
 * Created on Oct 5, 2006 by mschilli
 */
package alma.acs.jhelpgen;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;





public class Gen {


	public static void main (String[] args) {
		try {
			if (args.length < 2)
				throw new Exception("too few arguments");
			
			String appName = args[0];
			File tocFile = new File(appName + "TOC.xml");
			File mapFile = new File(appName + "Map.jhm");
			
			List<File> fArgs = new LinkedList<File>();
			for (int i=1; i<args.length; i++)
				fArgs.add (new File(args[i]));

			new Gen().go(tocFile, mapFile, fArgs);
			
		} catch (Exception e) {
			System.err.println("Error: "+e);
			System.err.println("Usage: (this) appName {dir|file}+");
		}
	}
	
	protected void go (File fToc, File fMap, List<File> fArgs) throws IOException {
		
		StringBuilder sbToc = new StringBuilder();
		StringBuilder sbMap = new StringBuilder();
		
		List<File> files = new Vector<File>();
		for (File f : fArgs)
			if (f.isDirectory())
				Util.findFiles (files, f, ".html", ".htm", ".HTML", ".HTM", ".input");
			else
				files.add(f);
		
		for (File f : files) {
			String fileContents = Util.readFile(f);
			String fileId = Util.filenameToIdentifier(f);
			
			AnchorNode root = htmlToDom(fileContents);
			
			// tweak anchors a little
			List<AnchorNode> nn = Util.depthFirst (root, new Vector<AnchorNode>());
			for (AnchorNode n : nn) {
				 n.anchorName = fileId +"#"+ n.anchorName;
			}
			
			domToToc (root.children, sbToc);
			domToMap (root.children, sbMap);
		}
		
		String toc = finishToc(sbToc);
		String map = finishMap(sbMap);

		Util.writeFile(toc, fToc);
		Util.writeFile(map, fMap);
	}
	
	
	protected AnchorNode htmlToDom (String contents) {
		
		AnchorNode root = new AnchorNode(0, "root", "", "");
		AnchorNode latest = root;
		
		Pattern headingPattern = Pattern.compile("<[Hh]([1-6]).*?</[Hh]([1-6])>");
		Pattern anchorPattern = Pattern.compile("<[Aa].* name=['\"]?(\\w*)['\"]?.*>(.*)</[Aa]>");
		
		Matcher hm = headingPattern.matcher(contents);
		while (hm.find()) {
			String heading = hm.group().trim();
			int level = Integer.parseInt(hm.group(1));
	
			Matcher am = anchorPattern.matcher(heading);
			if (am.find()) {
				String anchor = am.group(1).trim();
				String pretty = am.group(2).trim();
				AnchorNode n = new AnchorNode(level, heading, anchor, pretty);

				// sort into tree
				AnchorNode cand = latest;
				while (cand.level >= n.level)
					cand = cand.parent;
				cand.add(n);
				latest = n;
			}
		}
		return root;
	}

	protected void domToMap (List<AnchorNode> nn, StringBuilder sb) {
		for (AnchorNode m : nn)
			domToMap(m, sb);
	}
	
	protected void domToMap (AnchorNode n, StringBuilder sb) {
		sb.append("\n\t");
		sb.append("<mapID target=\"").append(n.anchorName).append("\"");
		sb.append(" url=\"").append(n.anchorName).append("\"");
		domToMap (n.children, sb);
	}

	protected void domToToc (List<AnchorNode> nn, StringBuilder sb) {
		for (AnchorNode m : nn)
			domToToc(m, sb);
	}
	
	protected void domToToc (AnchorNode n, StringBuilder sb) {
		sb.append("\n").append("\t\t\t\t\t\t".substring(0, n.level));
		sb.append("<tocitem text=\"").append(n.prettyName).append("\"");
		sb.append(" target=\"").append(n.anchorName).append("\"");
		
		if (n.children.size() == 0)
			sb.append("/>");
		else {
			sb.append(">");
			domToToc (n.children, sb);
			sb.append("\n").append("\t\t\t\t\t\t".substring(0, n.level));
			sb.append("</tocitem>");
		}
	}

	protected String finishToc (StringBuilder sb) {
		
		String begin = 
			"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>" +
			"\n<!DOCTYPE toc PUBLIC \"-//Sun Microsystems Inc.//DTD JavaHelp TOC Version 1.0//EN\"" +
			"\n         \"http://java.sun.com/products/javahelp/toc_1_0.dtd\">" +
			"\n<toc version=\"1.0\" categoryclosedimage=\"chapter\" topicimage=\"topic\">" +
			"\n\t<tocitem text=\"Help Topics\" image=\"toplevelfolder\">";

		String end = "\n\t</tocitem>\n</toc>";
		
		return begin + sb + end;
	}

	protected String finishMap (StringBuilder sb) {
		
		String begin = 
			"<?xml version='1.0' encoding='ISO-8859-1' ?>" +
			"\n<!DOCTYPE map PUBLIC \"-//Sun Microsystems Inc.//DTD JavaHelp Map Version 1.0//EN\" " +
			"\n         \"http://java.sun.com/products/javahelp/map_1_0.dtd\"> " +
			"\n<map version=\"1.0\"> " +
			"\n\t<mapID target=\"toplevelfolder\" url=\"images/toplevel.gif\" /> " +
			"\n\t<mapID target=\"chapter\" url=\"images/chapTopic.gif\" /> " +
			"\n\t<mapID target=\"topic\" url=\"images/topic.gif\" />" +
			"\n";

		String end = "\n</map>";
		
		return begin + sb + end;
	}

	protected String finishHelpset () {
		String b = "<?xml version='1.0' encoding='ISO-8859-1' ?> <!DOCTYPE helpset PUBLIC \"-//Sun Microsystems Inc.//DTD JavaHelp HelpSet Version 2.0//EN\" "+
         "\"../dtd/helpset_2_0.dtd\"> <?MyFavoriteApplication this is data for my favorite application ?>" +

"<helpset version=\"1.0\">  <!-- title -->  <title>Acs Command Center Online Help</title>" +
  "<!-- maps -->  <maps>     <homeID>intro</homeID>     <mapref location=\"Map.jhm\"/>" +
  "</maps>  <!-- views --> <view>    <name>TOC</name>    <label>Acs Command Center</label> " +
  "  <type>javax.help.TOCView</type>    <data>AcsCommandCenterTOC.xml</data>  </view> </helpset>";
		return b;
	}
	
	
	
	protected class AnchorNode {
		public int level;
		public String heading;
		public String anchorName;
		public String prettyName;
		public AnchorNode parent;
		public List<AnchorNode> children = new Vector<AnchorNode>();

		AnchorNode(int level, String heading, String anchorName, String prettyName) {
			this.level = level;
			this.heading = heading;
			this.anchorName = anchorName;
			this.prettyName = prettyName;
		}

		void add (AnchorNode n) {
			n.parent = this;
			children.add(n);
		}
		
		@Override
		public String toString() {
			return "AnchorNode[level="+level+", anchor="+anchorName+", caption="+prettyName+", text=\""+heading+"\"]";
		}
	}	
}






