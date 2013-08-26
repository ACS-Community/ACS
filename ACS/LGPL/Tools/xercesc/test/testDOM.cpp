#include <ParameterSet.h>
#include <iostream>

void printUsage();

int main(int argc, char *argv[])
{
	if(argc != 3) {
		printUsage();
		return -1;
	}

	for(int i=1; i<argc; i++)
	{
		if (strcmp(argv[i], "-file") == 0)
	    	{
			if(NULL != argv[i+1]) {
				ParameterSet pset(argv[i+1]);
			}
		}
	}
}

void printUsage() 
{
	std::cout << "To run, please type: testDOM -file <xmlFileToUseForInput> \n";
}
