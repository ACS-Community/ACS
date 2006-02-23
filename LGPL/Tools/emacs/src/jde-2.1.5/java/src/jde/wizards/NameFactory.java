/*
 * Copyright (c) Eric D. Friedman 1998. All Rights Reserved.
 * Copyright (c) Paul Kinnucan 1998. All Rights Reserved.
 *
 * $Revision: 1.26 $ 
 * $Date: 2002/02/27 10:32:22 $ 
 *
 * InterfaceFactory is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 *
 * InterfaceFactory is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * To obtain a copy of the GNU General Public License write to the
 * Free Software Foundation, Inc.,  59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.  
 */

package jde.wizards;

/**
 * Interface for class that provides name for method
 * parameters.
 *
 * @author Eric D. Friedman
 * @version $Revision: 1.26 $
 */
public interface NameFactory
{
  /** Returns a unique (descriptive?) parameter name for the specified
   * type.
   *
   * @param type - the parameter type
   * @param param_num - the parameter's number in its method Signature
   * @param total_params - the total number of parameters in the Signature.
   */
  public String getParameterName( Class type, int param_num, int total_params );
}
