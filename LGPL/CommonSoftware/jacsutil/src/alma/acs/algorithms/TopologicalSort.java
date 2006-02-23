package alma.acs.algorithms;

import java.util.*;


/**
 * Topological sort algorithm, following Cormen et al, "Introduction to Algorithms".
 * <p>
 * To be used to sort a list of interdependent nodes, or to find out that this
 * is impossible because the dependencies are cyclic. 
 * Applications need to wrap each of their nodes with a {@link Vertex} object,
 * set up the directed dependencies between any two such vertex objects
 * (see {@link Vertex#addAdjacentVertex(Vertex vertex) addAdjacentVertex}), run the sort algorithm, 
 * and then extract the original nodes from the sorted vertices. 
 * <p>
 * Expected use is to detangle component dependencies etc. 
 */
public class TopologicalSort
{
	// the graph as a List of Vertex objects
	private Collection m_vertices;

	// the sorted graph as a List of Vertex objects
	private LinkedList m_sortedVertices;

	// Map [key = Vertex, Value = List of Vertex objects that form a back edge in the graph]
	private Map m_cyclicVertices;


	/**
	 * Constructor that takes the nodes, which we hope form a directed acyclic graph
	 * @param vertices
	 */
	public TopologicalSort(Collection vertices)
	{
		m_vertices = vertices;
		m_sortedVertices = new LinkedList();
	}

	/** 
	 * Tries to sort the vertices and to return them in a list.
	 * The list will be incomplete if the graph could not be sorted, 
	 * see {@link #hasCycles()}.
	 */
	public List sort()
	{
		// init
		for (Iterator iter = m_vertices.iterator(); iter.hasNext(); )
		{
			Vertex vertex = (Vertex) iter.next();
			vertex.setColor(Vertex.WHITE);
		}

		// depth-first-search
		for (Iterator iter = m_vertices.iterator(); iter.hasNext(); )
		{
			Vertex vertex = (Vertex) iter.next();
			if (vertex.getColor() == Vertex.WHITE)
			{
				dfsVisit(vertex);
			}
		}

		return m_sortedVertices;
	}


	private void dfsVisit(Vertex vertex)
	{
		vertex.setColor(Vertex.GRAY);
		for (Iterator iter = vertex.getAdjacencyList().iterator(); iter.hasNext(); )
		{
			Vertex descendant = (Vertex) iter.next();
			if (descendant.getColor() == Vertex.WHITE)
			{
				///descendant.setPredecessor(vertex);
				dfsVisit(descendant);
			}
			else if (descendant.getColor() == Vertex.GRAY)
			{
				// we found a "back edge", which means a cycle in the graph
				if (m_cyclicVertices == null)
				{
					m_cyclicVertices = new HashMap();
				}
				if (m_cyclicVertices.get(vertex) == null)
				{
					m_cyclicVertices.put(vertex, new ArrayList());
				}
				((List) m_cyclicVertices.get(vertex)).add(descendant);
			}
		}

		// vertex is finished
		vertex.setColor(Vertex.BLACK);
		m_sortedVertices.addFirst(vertex);
	}


	/**
	 * States whether the graph contains cyclic dependencies, 
	 * which implies that it could not be sorted.
	 * To be called after {@link #sort()} was called.
	 * @return  true if cycles were found
	 */
	public boolean hasCycles()
	{
		return ( m_cyclicVertices != null );
	}

	/** 
	 * Returns the vertices (graph nodes) that form a back edge in the graph.
	 * 
	 * @return Map [key = Vertex, Value = List of adjacent Vertex objects that form the cycle] 
	 */
	public Map getCyclicVertices()
	{
		return m_cyclicVertices;
	}
}

