/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.algorithms;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import junit.framework.TestCase;

/**
 * Tests for {@link TopologicalSort}.
 * 
 * @author hsommer
 * created Sep 30, 2003 2:06:13 PM
 */
public class TopologicalSortTest extends TestCase
{
	/**
	 * Sorts professor Bumstead's clothing as in Cormen, "Introduction to Algorithms" 
	 */
	public void testSortDirectedAcyclicGraph()
	{
		List unsortedGarments = createGarmentVertices(false);
		
		// sort the graph
		TopologicalSort topSort = new TopologicalSort(unsortedGarments);
		List sortedGarments = topSort.sort();
		
		assertFalse("no cycles", topSort.hasCycles());
		assertEquals("conservation of vertices", unsortedGarments.size(), sortedGarments.size());
		
		System.out.println("here's one possible sequence for Prof. Bumstead to put on your clothing:");
		for (Iterator iter = sortedGarments.iterator(); iter.hasNext();)
		{
			Vertex garment = (Vertex) iter.next();
			System.out.print((String)garment.getUserObject());
			if (iter.hasNext())
			{
				System.out.print(" -> ");
			}
		}
		System.out.println();
	}


	public void testFailOnCyclicGraph()
	{
		List unsortedGarments = createGarmentVertices(true);

		TopologicalSort topSort = new TopologicalSort(unsortedGarments);
		topSort.sort();
		
		assertTrue("cycle", topSort.hasCycles());
		
		Map cyclicVerticesMap = topSort.getCyclicVertices();
		
		for (Iterator iter = cyclicVerticesMap.keySet().iterator(); iter.hasNext();)
		{
			Vertex badVertex = (Vertex) iter.next();
			
			System.out.print("found a cycle at garment " + badVertex.getUserObject());
			System.out.print(" connecting with ");

			List badLinks = (List) cyclicVerticesMap.get(badVertex);
			for (Iterator iterator = badLinks.iterator(); iterator.hasNext();)
			{
				Vertex badLink = (Vertex) iterator.next();
				System.out.print(badLink.getUserObject() + ", ");
			}
			System.out.println();
		}
	}
	
	
	private List createGarmentVertices(boolean withCycle)
	{
		List unsortedGarments = new ArrayList();
		
		Vertex jacket = new Vertex("jacket");
		unsortedGarments.add(jacket);
		Vertex pants = new Vertex("pants");
		unsortedGarments.add(pants);
		Vertex shoes = new Vertex("shoes");
		unsortedGarments.add(shoes);
		Vertex watch = new Vertex("watch");
		unsortedGarments.add(watch);
		Vertex undershorts = new Vertex("undershorts");
		unsortedGarments.add(undershorts);
		Vertex shirt = new Vertex("shirt");
		unsortedGarments.add(shirt);
		Vertex socks = new Vertex("socks");
		unsortedGarments.add(socks);
		Vertex belt = new Vertex("belt");
		unsortedGarments.add(belt);
		Vertex tie = new Vertex("tie");
		unsortedGarments.add(tie);
		 
		// set up dependencies
		shirt.addAdjacentVertex(tie);
		shirt.addAdjacentVertex(belt);
		socks.addAdjacentVertex(shoes);
		undershorts.addAdjacentVertex(shoes);
		undershorts.addAdjacentVertex(pants);
		pants.addAdjacentVertex(shoes);
		pants.addAdjacentVertex(belt);
		belt.addAdjacentVertex(jacket);
		tie.addAdjacentVertex(jacket);
		
		if (withCycle)
		{
			jacket.addAdjacentVertex(undershorts);
		}
		
		return unsortedGarments;
	}
}

