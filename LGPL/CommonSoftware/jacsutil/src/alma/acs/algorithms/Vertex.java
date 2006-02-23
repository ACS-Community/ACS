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
	private List   m_adjacencyList;

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
		m_adjacencyList = new ArrayList();
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

	List getAdjacencyList()
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

