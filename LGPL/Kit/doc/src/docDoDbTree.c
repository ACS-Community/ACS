/*************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: docDoDbTree.c,v 1.32 2008/06/09 06:52:09 bjeram Exp $" 
*
* who       when      what
* --------  --------  ----------------------------------------------
* pforstma  17/08/94  created
*                     CREDIT: "display_dir" core algorithm is based on
*                     the "onedir" function listed in the chapter 4 
*                     (page 78) of the POSIX Programmer's Guide - 
*                     Donald Lewine - O'Reilly & Associates, Inc- 1991.
*/

/************************************************************************
*   NAME
*   docDoDbTree - displays (DB) directory structure
* 
*   SYNOPSIS
*   docDoDbTree <directory> [-n]
* 
*   DESCRIPTION
*   docDoDbTree displays the directory structure starting from
*   the <directory> parameter, each directory is on line, linked
*   to his to his parents with dashes:
*
*   <PARENT>
*     |--<CHILD>
*     |--<CHILD>
*
*   No file is displayed, only directories are.
*   
*   <directory> is the mandatory parameter specifying the root of
*   the directory hierarchy to be displayed.
*
*   [-n] is a optional switch: if present the directory structure
*   is printed without indentation.
*
*   This utility serves first to display the RTAP DB structure but
*   can also be used to get a quick directory structure.
*
*   FILES
*
*   ENVIRONMENT
*
*   RETURN VALUES
*
*   CAUTIONS 
*   
*   Inaccessible directories (links pointing to nothing or directories
*   protected) are not displayed and trigger an error message on
*   the standard error output: the program does not stop.
*
*   EXAMPLES
*
*   SEE ALSO
*
*   BUGS   
* 
*------------------------------------------------------------------------
*/

#define _POSIX_SOURCE 1
#include "vltPort.h"


static char *rcsId="@(#) $Id: docDoDbTree.c,v 1.32 2008/06/09 06:52:09 bjeram Exp $";
/* static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId); */


#include <stdio.h>
#include <dirent.h>
#include <unistd.h>
#include <sys/stat.h>
#include <errno.h>

#define MAX_PATH 255

#define INDENTATION_STEP 2

/*
** Function panic: 
**
** displays error messages and stops the program
** if the error is not "No such file or directory" or "Permission Denied".
**
** Input: name of the system call
**        parameter of the system call
*/

void panic(char *function_name, char *parm)

{
    if (parm == NULL)
        fprintf(stderr, "Error in %s system call \n", function_name);
    else
        fprintf(stderr, "Error in %s system call (%s) \n", function_name,
                parm);
    perror(NULL);

    /* exit only if the error is not related to a file access right
    ** or a dangling link
    */
    if (errno != ENOENT && errno != EACCES)
	exit(-1);

}

/*
** Function display_dir:
**
** displays the directory structure from <dir> using the
** <indent> numbers of characters to display directories 
** with indentation.
**
** Input: name of the root directory
**        number of indentation characters
**        output mode
*/


void display_dir(char *dir, int indent, int indented_output)

{

 char cwd[MAX_PATH+1], tcwd[MAX_PATH+1];

 DIR *dirp;
 struct dirent *dir_entry;
 struct stat status;

 int i,j, vertices_nb;

 dirp = opendir(dir);
 if (indent != 0 && indented_output == 1)
 {
     vertices_nb = indent / INDENTATION_STEP;

     /* second line */

     for (i=0; i < vertices_nb; i++)
	 {
            for (j=0; j < INDENTATION_STEP; j++) printf(" ");
            printf("|");
    	 }

     printf("--%s \n", dir);

 }


 /* Remember the current working directory and connect to
 ** new directory. We will then be able to stat() the files
 ** without building a prefix.
 */

 if (getcwd(cwd, MAX_PATH+1) == NULL)
    panic("getcwd", NULL);

 if (chdir(dir) != 0)
    panic("chdir", dir);

  else if (indented_output == 0) 
  {
      if (getcwd(tcwd, MAX_PATH+1) == NULL)
         panic("getcwd", NULL);
      else
         printf("%s \n", tcwd);
  }

 while ((dir_entry = readdir(dirp)) != NULL) 
 {
    /* ignore "." and ".." not to be confused */
    if (strcmp(dir_entry->d_name, ".") != 0 &&
        strcmp(dir_entry->d_name, "..") != 0)
	{
          if (stat(dir_entry->d_name, &status) != 0)
	  {
              panic("stat", dir_entry->d_name);
          } 
          /* ignore anything that is not a directory */
          if (S_ISDIR(status.st_mode))
	  {
             /* if this is a nested directory, process it */
             display_dir(dir_entry->d_name, indent + INDENTATION_STEP, 
                         indented_output);
          } 
        }
 }
 closedir(dirp);

 if (chdir(cwd) != 0)
    panic("chdir", cwd);

 return;

}

int main(int argc, char **argv)

{
   int indented_output = 1;

   if (argc < 2)
   {
      fprintf(stderr, "Directory argument missing \n");
      exit(-1);
   }
   else if (argc == 3)
   {
      if (strcmp(argv[2],"-n") != 0)
	  {
      fprintf(stderr, "Invalid switch: only -n is allowed \n");
      exit(-1); 
      }
      else indented_output = 0;
   }    

   display_dir(argv[1],0, indented_output);

   exit(0);
}















