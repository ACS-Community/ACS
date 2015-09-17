#!/bin/bash
acsNotifyService -k -s -b 0 -w -n $1 -x corbaloc::$HOST:3001/NameService
