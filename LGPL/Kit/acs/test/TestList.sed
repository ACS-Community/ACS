# Useful/necessary for the acsMakeInstall2 test only!

# Replace each of the used subdectories for this duplicateFile
# by a generic name (<subdir>), as we are not sure in which order
# they will be listed.
s/\.\/duplicateFile/<subdir>duplicateFile/
s/\.\/subdir1\/duplicateFile/<subdir>duplicateFile/
s/\.\/subdir1\/subdir1.1\/duplicateFile/<subdir>duplicateFile/
s/\.\/subdir2\/duplicateFile/<subdir>duplicateFile/
