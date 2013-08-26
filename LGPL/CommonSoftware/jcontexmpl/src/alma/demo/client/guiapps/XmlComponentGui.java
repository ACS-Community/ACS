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
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.logging.Logger;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

import alma.acs.component.client.ComponentClient;
import alma.acs.container.ContainerServices;


/**
 * @author hsommer Apr 8, 2003 5:17:12 PM
 */
public class XmlComponentGui
{
	private ComponentClient m_componentClient;

	private Logger m_logger;


	public XmlComponentGui() throws Exception // ok, dirty
	{
		m_componentClient = connect();
		
		ContainerServices containerServices = m_componentClient.getContainerServices();
		
		JPanel jpanel = new MySillyPanel(containerServices);
		
		JFrame jframe = wrapWithFrame(jpanel);

		jframe.addWindowListener(
			new WindowAdapter()
			{
				public void windowClosing(WindowEvent e)
				{
					try
					{
						m_componentClient.tearDown();
					}
					catch (Exception ex)
					{
						// TODO Auto-generated catch block
						ex.printStackTrace(System.err);
					}
				}
			}
		);
		jframe.setVisible(true);
	}


	private ComponentClient connect() throws Exception // ok, dirty
	{
		String managerLoc = System.getProperty("ACS.manager");
		if (managerLoc == null)
		{
			throw new Exception("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
		}
		String clientName = getClass().getName() + Long.toString(System.currentTimeMillis());
		m_logger = Logger.getLogger(clientName);
		
		ComponentClient componentClient = new ComponentClient(m_logger, managerLoc, clientName);
		
		return componentClient;
	}

	
	private JFrame wrapWithFrame(JPanel jpanel)
	{
		JFrame jframe;
		jframe = new JFrame("XmlComponent's GUI");
		jframe.getContentPane().add(jpanel);
		
		// termination behav.
		jframe.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
		
		// position the frame in the middle of the screen
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		int width = (int) screenSize.getWidth();
		int height = (int) screenSize.getHeight();
		width = (width >= 600 ? 600 : width);
		height = (height >= 400 ? 400 : height);
		jframe.setSize(width, height);
		jframe.setLocation((int)(screenSize.getWidth()-width)/2, (int)(screenSize.getHeight()-height)/2);
		
		return jframe;
	}
	
	
	public static void main(String[] args)
	{
		try
		{
			new XmlComponentGui();
		}
		catch (Exception e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace(System.err);
		}
	}
}
