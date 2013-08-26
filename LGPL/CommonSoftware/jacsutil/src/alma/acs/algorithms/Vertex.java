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
package alma.acs.algorithms;

import java.util.*;


/**
 * A vertex of the graph that the {@link TopologicalSort} can work on.
 */
public class Vertex
{
	// "color" constants
	static final int WHITE = 1;	// undiscovered
	static final int GRAY = 2;	// discovered, adjacencyList under investigation
	static final int BLACK = 3;	// discovered, adjacencyList investigated

	// members for the sorting algorithm
	private int    m_color;
	private List<Vertex>   m_adjacencyList;

	// user object (the real entity on whose behalf the Vertex gets sorted)
	private Object m_userObject;


	/**
	 * Constructor that wraps a node in the graph that has to be sorted.
	 * <p>
	 * The <code>userObject</code> must implement reasonable <code>equals()</code>
	 * and <code>hashCode()</code> methods.
	 * 
	 * @param userObject  the application's graph node, to be retrieved later
	 * 						through {@link #getUserObject}
	 */
	public Vertex(Object userObject)
	{
		m_userObject = userObject;
		m_adjacencyList = new ArrayList<Vertex>();
	}


	/**
	 * Adds a dependent vertex to this vertex.
	 * <p>
	 * The direction of the graph is from "must occur first" to "may occur later".
	 * If node B depends on node A, then call <code>vertexA.addAdjacentVertex(vertexB)</code>.
	 * <p>
	 * To use the example from Cormen, if you want to figure out in which order
	 * to put on your clothes, the vertex for socks must point to the vertex for shoes,
	 * stating that socks must be put on before you can put on your shoes, or in other 
	 * words, that shoes depend on socks. 
	 * So add the shoe vertex to the sock's adjacent vertex list.
	 * 
	 * @param vertex  another vertex that depends on this vertex.
	 */
	public void addAdjacentVertex(Vertex vertex)
	{
		if (!m_adjacencyList.contains(vertex))
		{
			m_adjacencyList.add(vertex);
		}
	}

	/**
	 * Returns the application's graph node that was wrapped by this vertex.
	 */
	public Object getUserObject()
	{
		return m_userObject;
	}


	int getColor()
	{
		return m_color;
	}

	List<Vertex> getAdjacencyList()
	{
		return m_adjacencyList;
	}

	void setColor(int color)
	{
		if (color == WHITE || color == GRAY || color == BLACK)
		{
			m_color = color;
		}
		else
		{
			// todo: error
		}
	}

	public boolean equals(Object obj)
	{
		if (this == obj)
			return true;
		if (obj == null || !(obj instanceof Vertex))
			return false;
		Vertex other = (Vertex) obj;
		return ( m_userObject.equals(other.m_userObject) );
	}

	public int hashCode()
	{
		return ( m_userObject.hashCode() );
	}

}

