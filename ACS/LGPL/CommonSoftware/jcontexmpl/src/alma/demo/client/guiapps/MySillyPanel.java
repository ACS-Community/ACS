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
package alma.demo.client.guiapps;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServices;
import alma.demo.XmlComponent;
import alma.demo.XmlComponentJ;
import alma.demo.XmlComponentOperations;
import alma.xmljbind.test.schedblock.SchedBlock;

/**
 * @author hsommer Mar 27, 2003 3:47:51 PM
 */
public class MySillyPanel extends JPanel
{

	private XmlComponentJ m_xmlCompJ;

	private JButton m_butt;

	/**
	 * 
	 */
	public MySillyPanel(ContainerServices containerServices) throws AcsJContainerServicesEx
	{
		org.omg.CORBA.Object compObj = containerServices.getComponent("XMLCOMP1");
		XmlComponent xmlComp = alma.demo.XmlComponentHelper.narrow(compObj);
		
		m_xmlCompJ =
			(XmlComponentJ) containerServices.getTransparentXmlWrapper(XmlComponentJ.class,
				xmlComp,
				XmlComponentOperations.class);
		
		m_butt = new JButton("Press me for the best SchedBlock...");
		add(m_butt);
		
		final JTextArea textArea = new JTextArea();
		JScrollPane areaScrollPane = new JScrollPane(textArea);
		areaScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
		areaScrollPane.setPreferredSize(new Dimension(500, 250));
		add(areaScrollPane);
		
		m_butt.addActionListener(new ActionListener()
		   {
			  public void actionPerformed(ActionEvent e)
			  {
				SchedBlock sb = m_xmlCompJ.getBestSchedBlock();
				textArea.append("the best SchedBlock has the unique ID " + 
					sb.getSchedBlockEntity().getEntityId());
				textArea.append("\n");
			  }
		   }
		);
		
	}

}
