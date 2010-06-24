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
