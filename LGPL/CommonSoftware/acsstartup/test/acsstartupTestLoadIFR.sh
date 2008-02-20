#! /bin/bash

echo "Starting ACS..."
acsStart -noloadifr

sleep 5

echo "Checking for Interface..."
acsstartupTestLoadIFR

echo "Loading Interface Repository..."
acsstartupLoadIFR 

sleep 5

echo "Checking for Interface..."
acsstartupTestLoadIFR

echo "Shutting down ACS..."
acsStop
