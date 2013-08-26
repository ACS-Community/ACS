/*
 * Copyright (c) Eric D. Friedman 1998. All Rights Reserved.
 * Copyright (c) Paul Kinnucan 1998. All Rights Reserved.
 *
 * $Revision: 1.26 $ 
 * $Date: 2002/02/27 10:32:21 $ 
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
 * Defines a default parameter name factory for the InterfaceFactory.
 * Generates argument names of the form param1, param2, etc.
 *
 * @author Eric D. Friedman
 * @version $Revision: 1.26 $
 */

public class DefaultNameFactory
  implements NameFactory
{
  /** @return "param" + param_num */
  public String getParameterName( Class type, int param_num, int total_params )
  {
    return "param" + param_num;
  }
}

// End of DefaultNameFactory.java
