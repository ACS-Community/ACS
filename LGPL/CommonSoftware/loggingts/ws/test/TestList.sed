s/TimeStamp=\"[^\"]*\"/TimeStamp=\"xxx\"/g
s/Host=\"[^\"]*\"/Host=\"xxx\"/g
s/Process=\"[0-9]*\"/Process=\"xxx\"/g
s/TimeStamp:\ .*/TimeStamp:\ xxx/g
s/Host:\ .*/Host:\ xxx/g
s/Process:\ [0-9]*/Process:\ xxx/g
s/Process:\ .*/Process:\ xxx/g
s/Log ID:\ [0-9]*/Log ID:\ xxx/g
s/LogId=\"[0-9]*\"/LogId=\"xxx\"/g
s/: initialized by \w*@[a-zA-Z0-9_\-]*/: initialized by user@host/g
