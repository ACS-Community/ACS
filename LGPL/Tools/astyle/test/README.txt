There are 2 tests:
1. astyle is applyed to astyle_main.cpp
2. astyle is applyed to LoggingClient.java

Both astyle_main.cpp and  LoggingClient.java were a modified version of the
originals to check if astyle works. Basically they are the same versions of
the originals but some portions were cutted and they appear almost
unreadable.

The test is done by using the first syntax of astyle: astyle <originalfile
 
astyle reads the file from the standard input and write its beautified version
in stdout. The output file is then compared with the verion in ref.

Some logic has to be added if the second syntax of astyle is to be checked
too.
