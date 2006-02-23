/*
 *                 Sun Public License Notice
 * 
 * The contents of this file are subject to the Sun Public License
 * Version 1.0 (the "License"). You may not use this file except in
 * compliance with the License. A copy of the License is available at
 * http://www.sun.com/
 * 
 * The Original Code is NetBeans. The Initial Developer of the Original
 * Code is Sun Microsystems, Inc. Portions Copyright 1997-2000 Sun
 * Microsystems, Inc. All Rights Reserved.
 */

package cern.gp.actions;

/** 
 * Move an item down in a list.
 * 
 * @see org.openide.nodes.Index
 *
 * @author  Vito Baggiolini
 */
public class MoveUpAction extends MoveUpActionCopiedFromNetbeans {
    /**
     * this is needed to avoid that  
     * {@link cern.gp.action.support.ActionUtils#createJButton(SystemAction)}
     * creates a JButton with twice the text returned by {@link #getName()}.
     * This happens because, if no icon is returned, the icon is replaced by 
     * the action text.
     */
    protected String iconResource() {
        return "org/netbeans/core/resources/arrowtop.gif";
    }
}
