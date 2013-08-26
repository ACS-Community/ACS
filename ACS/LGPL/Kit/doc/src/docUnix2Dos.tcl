#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: docUnix2Dos.tcl,v 1.31 2002/06/08 17:20:48 vltsccm Exp $" 
#
# who       when      what
# --------  --------  ----------------------------------------------
# pforstma  29/07/94  created
# gfilippi  24/08/98  riformatted as a tcl script
#

#************************************************************************
#   NAME
#   docUnix2Dos - file transfer betweem Unix and Dos via ENVIZEX floppy disk.
# 
#   SYNOPSIS
#   docUnix2Dos
# 
#   DESCRIPTION
#   create one "DOS" directory, ready to be copied on DOS.
#
#   The source directory is always the current directory.
#	
#   A directory, named from the basename (path last name) of the chosen 
#   directory with the "dos" suffix, is created as a brother directory of the
#   current  one. Every file from the chosen directory is copied into this 
#   directory  with the following name conversions:
#
#   - UNIX file name can only contain alphanumeric characters, the dot ('.')
#   character and the underscore ('_') character, but UNIX filenames which 
#   begin with the dot character are rejected (every dot character, except
#   the last one is converted to '_').
#   - UNIX file names which are the same when uppercase are rejected
#   (a DOS file name is always uppercase).
#
#   - otherwise, the DOS file name is built the following way:
#     - the 7 first characters are taken from the UNIX name;
#       if there is a name duplication problem, one character (underscore or
#       digit from 0 to 9) is added otherwise an underscore character is added
#     - if there is no UNIX suffix, there is not DOS suffix.
#     - if there is a UNIX suffix, it is truncated to the first 3 characters.
#  
#
#   Ex.1.: comment_list0 -> COMMENT_
#          comment_list9 -> COMMENT1
#          conclusions_of_discussion_on_open_points_after_INS_review ->
#          CONCLUS_
#
#   Ex.2.: comment_list.doc10 -> COMMENT_.DOC
#          comment_list.doc3  -> COMMENT1.DOC
#          com.doc3           -> COM.DOC
#          test.fm            -> TEST.FM
#
#   
#   To copy a UNIX directory to a DOS directory from an ENVIZEX terminal,
#   do the following  steps:
#
#   1. Go to the directory to be transfered:
#      cd <source_dir>
#   2. Execute docUnix2Dos
#      docUnix2Dos
#   3. Create the target directory on the floppy disk:
#      mkdir $HOME/floppy/<target_dir>
#   4. Copy the files from the UNIX directory to the DOS directory:
#      cp ../<source_dir>.dos/* $HOME/floppy/<target_dir>/.
#
#   Note that cp -r does not work.
#   If you cannot find a UNIX file in the DOS directory, use
#   grep <file> dos2unix in $HOME/floppy/<target_dir> to find
#   the DOS file name of a UNIX file name.
# 
#   FILES
#   ./*           read     to be copied
#   $HOME/floppy/<dir>     DOS counterpart of the UNIX directory
#
#   ENVIRONMENT
#
#   RETURN VALUES
#
#   CAUTIONS
#
#   What ever is the terminal you are logged on, it is the $HOME/floppy
#   directory which represents the terminal floppy disk you can access.
#
#   This script does not work if the current directory contains a subdirectory
#
#   If the UNIX directory contains the following files:
#
#   section1
#   section2
#   section3
#
#   the DOS directory contains these files:
#
#   section_
#   section0
#   section1
#
#   If the current directory is empty, the message "No files match glob
#   pattern *" is displayed.
#
#   EXAMPLES
#
#   SEE ALSO
#
#   docDos2Unix
#
#   BUGS     
#------------------------------------------------------------------------
#

# Checking of the contents of the current directory:
# builds a file name list which cannot be transfered.

set nok_file_name_list {}

# First, test if there are files whose name start with "."

set list_of_dot_file_names [glob .*] 
set i [expr [llength $list_of_dot_file_names] -1]
while {$i >=0} {
   set dot_file_name [lindex $list_of_dot_file_names $i]
   if { ([string compare $dot_file_name "."]  != 0) &&
        ([string compare $dot_file_name ".."] != 0) } {
      lappend nok_file_name_list $dot_file_name
   }
   incr i -1
}

# Then, test the file name with respect to other characters
# (files whose names begin with '.' are not taken into account
#  by [glob *]

set tmp_list_of_file_names [glob *] 
set list_of_file_names [lsort $tmp_list_of_file_names]
set i [expr [llength $list_of_file_names] -1]
while {$i >=0} {
   set string_is_valid "true"
   set file_name [lindex $list_of_file_names $i]

#  Range of characters ?

   set ok [regexp {[0-9A-Za-z\._]*} $file_name]
   if {$ok == 0} {
      set string_is_valid "false"
   }
 
# Number of dots ?

#   set file_name_length [string length $file_name]
#   set j 0
#   set number_of_dots 0
#   while {$j < $file_name_length} {
#       set current_char [string index $file_name $j]
#       if {$current_char == "."} {
#         incr number_of_dots 1
#       }
#       incr j 1
#   }
#   if {$number_of_dots > 1} {
#       set string_is_valid "false"
#   }

# Uppercase collision ?

  set uppercase_file_name [string toupper $file_name]
  set j [expr [llength $list_of_file_names] -1]
  while {$j >=0} {
      set file_name_bis [lindex $list_of_file_names $j]
      set uppercase_file_name_bis [string toupper $file_name_bis]
      if {[string compare $uppercase_file_name $uppercase_file_name_bis] == 0             &&  [string compare $file_name $file_name_bis] == 1} {
         set string_is_valid "false"
      }
      incr j -1
  }

   if {[string compare $string_is_valid "false"] == 0} {
      lappend nok_file_name_list $file_name
   }
   
   incr i -1
}


# if at least one file cannot be transfered, a message
# is displayed and script aborts.

set nok_file_name_number [llength $nok_file_name_list]
if {$nok_file_name_number > 0} {

   set k 0
   set kmax [expr $nok_file_name_number -1]
   while {$k <= $kmax} {
       set nok_file_name [lindex $nok_file_name_list $k]
       set message "File $nok_file_name cannot be transfered to DOS"
       puts $message
       incr k 1
  }
  return -1
}

# Here, every file in $file_name_list is valid.

set first_dos_file_name_list {}
set sorted_list_of_file_names [lsort $list_of_file_names]



# Turn the UNIX file name into DOS file name:
# truncate file name 

set i [expr [llength $sorted_list_of_file_names] -1]
while {$i >=0} {
   set file_name [lindex $sorted_list_of_file_names $i]
   set file_name_length [string length $file_name]
   set dos_file_name ""   
   set dot_index [string last "." $file_name]


# $range_high_bound is the index (starting from 0) of the last character
# part of the first part of the dos_file_name.

   if {$dot_index == -1} {
#     the UNIX file name has no suffix
      set range_high_bound 6
      if {$range_high_bound > $file_name_length} {
          set range_high_bound $file_name_length
      }
      set dos_file_name [string range $file_name 0 [expr $range_high_bound]]  
      append dos_file_name "_"
   } else {
#     the UNIX file name has a suffix
      if {$dot_index > 7} {
          set range_high_bound 7
      } else {
          set range_high_bound $dot_index
      }
      set dos_file_name [string range $file_name 0 [expr $range_high_bound -1]]
      set suffix [string range $file_name [expr $dot_index + 1] end]
      set suffix_length [string length $suffix] 
      if {$suffix_length > 3} {
          set suffix_length 3
      }
      set suffix [string range $file_name [expr $dot_index + 1] [expr $dot_index + $suffix_length]]
#
#     replace every dot character by a underscore
#
      set ic 0
      set new_dos_file_name ""
      while {$ic < 7} {
          set c [string index $dos_file_name $ic]
          if {$c == "."} {
              set c "_"
          }
          set new_dos_file_name $new_dos_file_name$c
          incr ic +1
      }
      set dos_file_name $new_dos_file_name

      append dos_file_name "_"
      append dos_file_name "."
      append dos_file_name $suffix

   }     

   lappend first_dos_file_name_list $dos_file_name
   incr i -1
}

# Turn the UNIX file name into DOS file name:
# rename duplicated names.

set dos_file_name_list {}
set i [expr [llength $first_dos_file_name_list] -1]
set prev_file_name ""
set current_duplicate ""
set duplicate_character "0"

while {$i >=0} {
   set file_name [lindex $first_dos_file_name_list $i]
   set file_name_length [string length $file_name]
   set dos_file_name ""   

   if {[string compare $prev_file_name $file_name] == 0} {

#  we have duplicate

      if {[string compare $file_name $current_duplicate] == 0} {

#  we already have duplicates for this name: increment duplicate character

      incr duplicate_character +1 
      } else {       
         set duplicate_character 0
      }
      set current_duplicate $file_name
      set dot_index [string first "." $file_name]

      if {$dot_index == -1} {
          set dos_file_name [string range $file_name 0 [expr $file_name_length -2]]
          append dos_file_name $duplicate_character
      } else {

          set dos_file_name [string range $file_name 0 [expr $dot_index -2]]
          append dos_file_name $duplicate_character
          append dos_file_name [string range $file_name [expr $dot_index] end]
      }

   } else {
#  dos file name is the same as unix file name
      set dos_file_name $file_name
   }

   lappend dos_file_name_list $dos_file_name
   incr i -1
   set prev_file_name $file_name

}


# create "DOS" directory 

# basename 'pwd'
set wd [pwd]
set basename_index [string last "/" $wd]
set basename [string range $wd [expr $basename_index +1] end]

exec mkdir ../$basename.dos

#
# copy files on floppy
# 

set i [expr [llength $dos_file_name_list] -1]
while {$i >=0} {
   set dos_file_name  [lindex $dos_file_name_list $i]
   set unix_file_name [lindex $list_of_file_names $i]
   exec cp $unix_file_name ../$basename.dos/$dos_file_name
   incr i -1
}
 

#
# create log file (UNIX <-> DOS)
# this log file is called by docDos2Unix

set process_id [pid]
set log_name /tmp/dos2unix.$process_id
set log [open $log_name w]

puts $log "#!/bin/sh"
puts $log "# DO NOT DELETE OR UPDATE THIS FILE, IT IS NEEDED TO COPY BACK YOUR FILES FROM DOS TO UNIX"
puts $log "if \[ \$# -eq 0 \]"
puts $log "then target=$wd"
puts $log "else target=\$1"
puts $log "fi"

set i [expr [llength $dos_file_name_list] -1]
while {$i >=0} {
   set dos_file_name  [lindex $dos_file_name_list $i]
   set unix_file_name [lindex $list_of_file_names $i]
   puts $log "cp $dos_file_name \$target/$unix_file_name"
   incr i -1
}
 
close $log

exec cp $log_name ../$basename.dos/dos2unix











