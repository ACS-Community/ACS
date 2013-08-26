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
			if (args.length < 1)
				throw new IllegalArgumentException("too few arguments");
			
			File helpDir = new File(args[0]);

			if (!helpDir.isDirectory())
				throw new IllegalArgumentException("not a directory: "+helpDir);
			
			List<File> fArgs = new LinkedList<File>();
			if (args.length == 1)
				fArgs.add (helpDir);
			else
				for (int i=1; i<args.length; i++) {
					File f = new File(args[i]);
					if (! f.getPath().startsWith(helpDir.getName()))
						throw new IllegalArgumentException("toc-dir|toc-file must live under help-dir "+helpDir.getAbsolutePath()+": "+f.getAbsolutePath());
					fArgs.add (f);
				}
			
			new Gen().go(helpDir, fArgs);
			
		} catch (IllegalArgumentException e) {
			System.err.println("Error: "+e.getMessage());
			System.err.println("Usage: (this) help-dir {toc-dir|toc-file}+");
		} catch (Exception e) {
			System.err.println("Error: "+e);
		}
	}
	
	protected void go (File helpDir, List<File> fArgs) throws IOException {
		StringBuilder worklog = new StringBuilder();
		
		StringBuilder sbToc = new StringBuilder();
		StringBuilder sbMap = new StringBuilder();
		
		List<File> files = new Vector<File>();
		for (File f : fArgs)
			if (f.isDirectory())
				Util.findFiles (files, f, ".html", ".htm", ".HTML", ".HTM", ".input");
			else
				files.add(f);
		
		String firstFileId = null;
		
		for (File f : files) {
			String fileContents = Util.readFile(f);
			String fileId = f.getPath().substring(helpDir.getName().length()+1).replace("\\", "/");

			if (firstFileId == null)
				firstFileId = fileId;
			
			AnchorNode root = htmlToDom(fileContents);
			
			// tweak anchors a little
			for (AnchorNode n : root.depthFirst(new Vector<AnchorNode>()))
				 n.anchorName = fileId +"#"+ n.anchorName;

			domToToc (root.children, sbToc);
			domToMap (root.children, sbMap);
		}
		
		
		File fToc = new File(helpDir, Const.TOC_FILENAME);
		File fMap = new File(helpDir, Const.MAP_FILENAME);
		File fSet = new File(helpDir, Const.SET_FILENAME);
		
		String toc = finishToc(sbToc);
		String map = finishMap(sbMap, firstFileId);
		String set = finishHelpset(fToc.getName(), fMap.getName());

		Util.writeFile(toc, fToc);
		Util.writeFile(map, fMap);
		Util.writeFile(set, fSet);

		worklog.append("Created "+fToc.getPath()+"\n");
		worklog.append("Created "+fMap.getPath()+"\n");
		worklog.append("Created "+fSet.getPath()+"\n");

		// copy images from our jar to helpdir
		String[] imageNames = new String[]{"chapTopic.gif", "topic.gif", "toplevel.gif"};
		File imageDir = new File(helpDir, "images");
		imageDir.mkdir();
		for (String imageName : imageNames) {
			String imageContent = Util.readResource("/alma/acs/jhelpgen/content/" + imageName);
			
			File fImage = new File(imageDir, imageName);
			Util.writeFile(imageContent, fImage);
			
			worklog.append("Created " + fImage.getPath()+"\n");
		}

		
		System.out.println("\n" + worklog);
	}

	
	
	protected AnchorNode htmlToDom (String contents) {
		
		AnchorNode root = new AnchorNode(0, "root", "");
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
				AnchorNode n = new AnchorNode(level, anchor, pretty);

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
		sb.append(" url=\"").append(n.anchorName).append("\"/>");
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
		String begin = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>"
				+ "\n<!DOCTYPE toc PUBLIC \"-//Sun Microsystems Inc.//DTD JavaHelp TOC Version 1.0//EN\""
				+ "\n         \"http://java.sun.com/products/javahelp/toc_1_0.dtd\">"
				+ "\n<toc version=\"1.0\" categoryclosedimage=\"_chapter\" topicimage=\"_topic\">";

		String end = "\n</toc>";
		
		return begin + sb + end;
	}

	protected String finishMap (StringBuilder sb, String introFileId) {
		String begin = "<?xml version='1.0' encoding='ISO-8859-1' ?>"
				+ "\n<!DOCTYPE map PUBLIC \"-//Sun Microsystems Inc.//DTD JavaHelp Map Version 1.0//EN\" "
				+ "\n         \"http://java.sun.com/products/javahelp/map_1_0.dtd\"> " + "\n<map version=\"1.0\"> "
				+ "\n\t<mapID target=\"_toplevelfolder\" url=\"images/toplevel.gif\" /> "
				+ "\n\t<mapID target=\"_chapter\" url=\"images/chapTopic.gif\" /> "
				+ "\n\t<mapID target=\"_topic\" url=\"images/topic.gif\" />" + "\n"
				+ "\n\t<mapID target=\"_intro\" url=\""+introFileId+"\" />";

		String end = "\n</map>";
		
		return begin + sb + end;
	}

	protected String finishHelpset (String fTocName, String fMapName) {
		String b = "<?xml version='1.0' encoding='ISO-8859-1' ?> <!DOCTYPE helpset PUBLIC \"-//Sun Microsystems Inc.//DTD JavaHelp HelpSet Version 2.0//EN\" "
				+ "\"../dtd/helpset_2_0.dtd\"> <?MyFavoriteApplication this is data for my favorite application ?>"
				+ "<helpset version=\"1.0\"> <title>Online Help</title>"
				+ "\n<maps>"
				+ "\n\t<homeID>_intro</homeID>"
				+ "\n\t<mapref location=\""+fMapName+"\"/>"
				+ "\n</maps>"
				+ "\n<view>"
				+ "\n\t<name>TOC</name>"
				+ "\n\t<label>Online Help</label>"
				+ " \n\t<type>javax.help.TOCView</type>"
				+ "\n\t<data>"+fTocName+"</data>"
				+ "\n</view>"
				+ "\n</helpset>";
		return b;
	}
	

	
	protected class AnchorNode {
		public int level;
		public String anchorName;
		public String prettyName;
		public AnchorNode parent;
		public List<AnchorNode> children = new Vector<AnchorNode>();

		AnchorNode(int level, String anchorName, String prettyName) {
			this.level = level;
			this.anchorName = anchorName;
			this.prettyName = prettyName;
		}

		void add (AnchorNode n) {
			n.parent = this;
			children.add(n);
		}
		
		List<AnchorNode> depthFirst (List<AnchorNode> ret) {
			ret.add(this);
			for (AnchorNode m : children)
				m.depthFirst(ret);
			return ret;
		}
		
		@Override
		public String toString() {
			return "AnchorNode[level="+level+", anchor="+anchorName+", caption="+prettyName+"]";
		}
	}	
}






