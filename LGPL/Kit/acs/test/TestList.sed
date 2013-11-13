# Useful/necessary for the acsMakeInstall2 and acsMakeInstall3 tests only!

# Replace each of the used subdectories for this duplicateFile
# by a generic name (<subdir>), as we are not sure in which order
# they will be listed.
s/\.\/duplicateFile/<subdir>duplicateFile/
s/\.\/subdir1\/duplicateFile/<subdir>duplicateFile/
s/\.\/subdir1\/subdir1.1\/duplicateFile/<subdir>duplicateFile/
s/\.\/subdir2\/duplicateFile/<subdir>duplicateFile/

s/\/.*\/lib\/python\/site-packages\/AcsPyTestPkg/<installpath>\/lib\/python\/site-packages\/AcsPyTestPkg/
s/\/.*\/acs\/test/<modpath>\/test/
