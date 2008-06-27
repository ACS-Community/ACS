/*
ALMA - Atacama Large Millimiter Array
* (c) UNSPECIFIED - FILL IN, 2008 
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
/** 
 * @author  cparedes
 * @version $Id: JVMSettingsTest.java,v 1.1 2008/06/27 06:25:59 cparedes Exp $
 * @since    
 */

/**
 * Insert a Class/Interface comment.
 * 
 */
package alma.acs.JVMSettingsTest;

class JVMSettingsTest{
    public void produceOutOfMemoryException() throws OutOfMemoryError
    {
        long [][] bigArray = new long[5000][5000];      
    }
 public static void main(String args[]){
 
    JVMSettingsTest t = new JVMSettingsTest();
    t.produceOutOfMemoryException();
 
 }
} 

 
