/**
 * 
 */
package com.cosylab.cdb.jdal.hibernate;

import org.hibernate.dialect.Oracle9Dialect;
import org.hibernate.id.SequenceIdentityGenerator;

/**
 * @author msekoranja
 *
 */
public class Oracle9DialectWithSequenceIdentity extends Oracle9Dialect {

	/* (non-Javadoc)
	 * @see org.hibernate.dialect.Dialect#getNativeIdentifierGeneratorClass()
	 */
	@Override
	public Class getNativeIdentifierGeneratorClass() {
		return SequenceIdentityGenerator.class;
	}


}
