/*
 *    ALMA - Atacama Large Millimeter Array
 *    (c) Universidad Tecnica Federico Santa Maria, 2009
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

/**
 * Groups all the classes that deal with the logic of ACG. There are three different kind of classes:
 * <ul>
 *   <li>ACS-related classes: These classes deal with the connection of the application with ACS
 *       and the DAL ({@link cl.utfsm.acs.acg.core.AcsInformation}), and the retrieval of the different DAOs that are needed by
 *       ACG ({@link cl.utfsm.acs.acg.core.DAOManager}).</li>
 *   <li>Manager classes: The second kind of classes are those that manage the current contents of
 *       ACG. There is one for each kind of element: {@link cl.utfsm.acs.acg.core.AlarmSystemManager},
 *       {@link cl.utfsm.acs.acg.core.AlarmManager}, {@link cl.utfsm.acs.acg.core.CategoryManager},
 *       {@link cl.utfsm.acs.acg.core.ReductionManager} and {@link cl.utfsm.acs.acg.core.SourceManager}.</li>
 *   <li>Others: There are two other classes that cannot be grouped into the previous classifications.
 *       The first is {@link cl.utfsm.acs.acg.core.ReductionRule}, an internal representation of a Reducion Rule, while the
 *       second one is {@link cl.utfsm.acs.acg.core.UserAuthenticator}, which handles the authentication of the users into
 *       the system.</li>
 * </ul>
 */
package cl.utfsm.acs.acg.core;