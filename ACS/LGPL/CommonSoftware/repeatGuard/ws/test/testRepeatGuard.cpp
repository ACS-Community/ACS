/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) Associated Universities Inc., 2007 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: testRepeatGuard.cpp,v 1.3 2007/03/23 09:50:06 nbarriga Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* nbarriga  2007-02-20  created
*/

// Uncomment this if you are using the VLT environment
// #include "vltPort.h"


#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static char *rcsId="@(#) $Id: testRepeatGuard.cpp,v 1.3 2007/03/23 09:50:06 nbarriga Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <unistd.h>
#include "RepeatGuard.h"

int main(int argc, char *argv[])
{
	RepeatGuard rg(10000000,10);

	for(int i=0;i<50;i++){
		rg.increment();
		if(rg.check())
			printf("Log attempt #%d\n",rg.count());
	}
	printf("\n\nUsing 10000000,10,true ------------------------------------------\n\n");
	rg.reset();
        for(int i=0;i<25;i++){
		usleep(200000);
                if(rg.checkAndIncrement())
                        printf("Log attempt #%d\n",rg.count());
        }
        
        printf("\n\nUsing 10000000,10,false -----------------------------------------\n\n");
        rg.reset(10000000,10,false);
        for(int i=0;i<25;i++){
                usleep(200000);
                if(rg.checkAndIncrement())
                        printf("Log attempt #%d\n",rg.count());
        }
        printf("\n\nUsing 10000000,0 -----------------------------------------\n\n");
        rg.reset(10000000,0,false);
        for(int i=0;i<25;i++){
                usleep(200000);
                if(rg.checkAndIncrement())
                        printf("Log attempt #%d\n",rg.count());
        }
        printf("\n\nUsing 0,10 -----------------------------------------\n\n");
        rg.reset(0,10);
        for(int i=0;i<25;i++){
                usleep(200000);
                if(rg.checkAndIncrement())
                        printf("Log attempt #%d\n",rg.count());
        }


	return 0;

}








