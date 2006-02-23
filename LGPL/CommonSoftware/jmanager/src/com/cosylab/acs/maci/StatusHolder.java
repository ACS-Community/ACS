/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci;

/**
 * Helper class to hold COBStatus value.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public final class StatusHolder
{

    /**
     * The <code>ComponentStatus</code> value held by this <code>StatusHolder</code>
     * object in its <code>status</code> field.
     */
    private ComponentStatus status = null;

    /**
     * Constructs a new <code>StatusHolder</code> object with its
     * <code>status</code> field initialized to <code>null</code>.
     */
	public StatusHolder()
	{
	}

    /**
     * Constructs a new <code>StatusHolder</code> object with its
     * <code>status</code> field initialized to the given
     * <code>ComponentStatus</code>.
     * @param initial the <code>ComponentStatus</code> with which to initialize
     *                the <code>status</code> field of the newly-created
     *                <code>StatusHolder</code> object
     */
    public StatusHolder(ComponentStatus status)
    {
		setStatus(status);
    }

	/**
	 * Returns the status.
	 * @return ComponentStatus
	 */
	public ComponentStatus getStatus()
	{
		return status;
	}

	/**
	 * Sets the status.
	 * @param status The status to set
	 */
	public void setStatus(ComponentStatus status)
	{
		this.status = status;
	}

}
