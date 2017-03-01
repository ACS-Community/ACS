/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2016 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.acs.classloading;

/** 
 * This test creates a {@link JarOrderOptimizer} object that, in turn, reads AcsTopsJars.txt
 * (@see {@link JarOrderOptimizer#}
 * <P>
 * At this stage of ACS buld, the jars are not present so the real functioning of this class
 * is tested in <code>jcont</code>.
 * What is tested here is only the correctness of the list of jars in AcsTopsJars.txt from 
 * i.e. the syntax. 
 * 
 * @author  acaproni
 * @since   2016.6
 */
class TestJarOrderOptmizer {
	public TestJarOrderOptmizer() {
		JarOrderOptimizer jarOrderOptimizer = new JarOrderOptimizer(true);
	}
	
	public static void main(String[] args) {
		TestJarOrderOptmizer testOptimizer = new TestJarOrderOptmizer();
	}
}


