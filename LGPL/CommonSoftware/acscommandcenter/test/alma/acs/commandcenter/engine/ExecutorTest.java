/*
 * Created on Dec 4, 2008 by mschilli
 */
package alma.acs.commandcenter.engine;

import alma.acs.commandcenter.engine.Executor.SearchBuffer;
import junit.framework.TestCase;




public class ExecutorTest extends TestCase {

	@Override
	protected void setUp () throws Exception {
		super.setUp();
	}

	public void testSearchBuffer() {
		SearchBuffer sb;
		byte[] stream;
		
		sb = new SearchBuffer("bla");
		stream = "zzzzzzzzzblazzzzzzzzzz".getBytes();
		assertTrue ("should find expression in stream", sb.add(stream, 0, stream.length));
		
		sb = new SearchBuffer("bla");
		stream = "bla".getBytes();
		assertTrue ("should find expression in stream", sb.add(stream, 0, stream.length));

		sb = new SearchBuffer("bla");
		stream = "albbla".getBytes();
		assertTrue ("should find expression in stream", sb.add(stream, 0, stream.length));


		sb = new SearchBuffer("bla");
		stream = "ba".getBytes();
		assertFalse ("should not find expression in stream", sb.add(stream, 0, stream.length));

		sb = new SearchBuffer("bla");
		stream = "blubb".getBytes();
		assertFalse ("should not find expression in stream", sb.add(stream, 0, stream.length));

	}
	
}


