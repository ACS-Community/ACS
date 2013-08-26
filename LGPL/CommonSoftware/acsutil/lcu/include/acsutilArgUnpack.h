//
// Define entry for server for VxWorks
// The server should be called as "server_entry (int argc, char * argv[])"
// in the source file
#define server_entry \
server_i (int, ASYS_TCHAR *[]); \
int \
server (char * inputString) \
{ \
  int argc; \
  char * argv[100]; \
  argv[0] = "server"; \
  argc = argUnpack(inputString, argv); \
  ACE_MAIN_OBJECT_MANAGER return server_i (argc, argv); \
} \
int \
server_i 

//
// Define entry for client for VxWorks
// The client should be called as "client_entry (int argc, char * argv[])"
// in the source file
#define client_entry \
client_i (int, ASYS_TCHAR *[]); \
int \
client (char * inputString) \
{ \
  int argc; \
  char * argv[100]; \
  argv[0] = "client"; \
  argc = argUnpack(inputString, argv); \
  ACE_MAIN_OBJECT_MANAGER return client_i (argc, argv); \
} \
int \
client_i 

//
// Define entry for driver for VxWorks
// The driver should be called as "driver_entry (int argc, char * argv[])"
// in the source file
#define driver_entry \
driver_i (int, ASYS_TCHAR *[]); \
int \
driver (char * inputString) \
{ \
  int argc; \
  char * argv[100]; \
  argv[0] = "driver"; \
  argc = argUnpack(inputString, argv); \
  ACE_MAIN_OBJECT_MANAGER return driver_i (argc, argv); \
} \
int \
driver_i

//
// Routine for unpacking entry string into argc, argv structure
int argUnpack (char * inputString, char * argv [])
{
  char * s ;
  char * ppLast;
  int i = 1;

  if (inputString != 0)
    {
    s = strtok_r (inputString, " ", &ppLast);
    if (s != NULL)
      {
      argv[i] = s;
      i++;
      }
    while (s != NULL)
      {
      s = strtok_r (0, " ", &ppLast);
      if (s != NULL)
	{
	argv[i] = s;
	i++;
	}
      }
    }
  return i;
}
