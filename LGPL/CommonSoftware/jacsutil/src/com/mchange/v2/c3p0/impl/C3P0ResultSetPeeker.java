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
package com.mchange.v2.c3p0.impl;

import java.sql.ResultSet;

/**
 * This is a sneaky way to get at the inner ResultSet of NewProxyResultSet. It marks the variable as protected,
 * so here I just make a class in the same package and get the value out. 
 * 
 * See http://stackoverflow.com/questions/1971617/c3p0-resultset-unwrap-throws-an-abstractmethoderror to see the
 * original thread.
 * 
 * @author "HappyEngineer" Stackoverflow.com user (adapted by rtobar for ACS)
 */
public class C3P0ResultSetPeeker
{
	public static ResultSet getInnerFrom(Object rs) {
		try {
			Class<?> clazz;
			clazz = Class.forName("com.mchange.v2.c3p0.impl.NewProxyResultSet");
			return (ResultSet)clazz.getDeclaredField("inner").get(rs);
		} catch (Exception e) {
			e.printStackTrace();
		}

		return null;
	}
}
