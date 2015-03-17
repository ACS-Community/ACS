# Useful/necessary for the acsMakeInstall* tests only!

# Replace each of the used subdectories for this duplicateFile
# by a generic name (<subdir>), as we are not sure in which order
# they will be listed.
s/\.\/duplicateFile/<subdir>duplicateFile/
s/\.\/subdir1\/duplicateFile/<subdir>duplicateFile/
s/\.\/subdir1\/subdir1.1\/duplicateFile/<subdir>duplicateFile/
s/\.\/subdir2\/duplicateFile/<subdir>duplicateFile/

s/\/.*\/lib\/python\/site-packages\/AcsPyTestPkg/<installpath>\/lib\/python\/site-packages\/AcsPyTestPkg/
s/\/.*\/acs\/test/<modpath>\/test/

# Note that these acsMakeInstall* tests can be executed directly via "tat", and also via 
# "make test"; in the latter case it means "make" will execute a test that executes a "make",
# i.e. there will be an additional level of make and that produces some differences in output.
# The "Entering directory" and "Leaving directory" messages are stripped off by TestList.grep,
# so what is left is to fake output from a single make level.
s/make\[1\]:/make:/