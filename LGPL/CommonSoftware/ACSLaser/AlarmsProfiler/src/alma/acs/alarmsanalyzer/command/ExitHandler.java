/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.alarmsanalyzer.command;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.commands.IHandlerListener;
import org.eclipse.ui.handlers.HandlerUtil;

import alma.acs.alarmsanalyzer.document.AnnunciatedContainer;
import alma.acs.alarmsanalyzer.document.ChatteringAlarmsContainer;
import alma.acs.alarmsanalyzer.document.MostFrequantAlarmsContainer;
import alma.acs.alarmsanalyzer.document.StaleAlarmsContainer;
import alma.acs.alarmsanalyzer.document.SuppressedContainer;
import alma.acs.alarmsanalyzer.document.TenMinutesContainer;
import alma.acs.alarmsanalyzer.document.flood.FloodContainer;
import alma.acs.alarmsanalyzer.engine.AcsSourceClient;

public class ExitHandler implements IHandler {

	@Override
	public void addHandlerListener(IHandlerListener handlerListener) {
		// TODO Auto-generated method stub

	}

	@Override
	public void dispose() {
		// TODO Auto-generated method stub

	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		AcsSourceClient.shutdown();
		MostFrequantAlarmsContainer.getInstance().shutdownContainer();
		StaleAlarmsContainer.getInstance().shutdownContainer();
		ChatteringAlarmsContainer.getInstance().shutdownContainer();
		TenMinutesContainer.getInstance().shutdownContainer();
		SuppressedContainer.getInstance().shutdownContainer();
		AnnunciatedContainer.getInstance().shutdownContainer();
		FloodContainer.getInstance().shutdownContainer();
		HandlerUtil.getActiveWorkbenchWindow(event).close();
		return null;
	}

	@Override
	public boolean isEnabled() {
		return true;
	}

	@Override
	public boolean isHandled() {
		return true;
	}

	@Override
	public void removeHandlerListener(IHandlerListener handlerListener) {
		// TODO Auto-generated method stub

	}

}
