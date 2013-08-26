#!/bin/ksh

echo "=== Starting the Manager"
maciManager &
sleep 15

# Start the container
echo "=== Starting the Containers"
  maciContainer Container01 -ORBEndpoint iiop://$HOST:3060 &  
  sleep 5
  maciContainer Container02 -ORBEndpoint iiop://$HOST:3061 &
  sleep 5
  maciContainer Container03 -ORBEndpoint iiop://$HOST:3062 &
