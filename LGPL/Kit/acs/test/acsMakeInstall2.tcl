# -*- tcl -*-
file mkdir ../include/subdir1 ../include/subdir1/subdir1.1 ../include/subdir2 ../include/CVS
exec touch ../include/duplicateFile \
           ../include/subdir1/duplicateFile \
           ../include/subdir1/subdir1.1/duplicateFile \
           ../include/subdir2/duplicateFile \
           ../include/CVS/duplicateFile