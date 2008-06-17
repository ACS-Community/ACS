#! /usr/bin/gawk -f

BEGIN {
width = 562;
pngRegexp = "[A-Za-z_-]+\.png"
hRegexp="[Hh][Ee][Ii][Gg][Hh][Tt]=\"?[0-9]+\"?";
wRegexp="[Ww][Ii][Dd][Tt][Hh]=\"?[0-9]+\"?";

while (getline < "imageSizeList.txt" == 1) {
	fname = substr($1,1,length($1)-1);
	height=$7*512.0/$5;
	h[fname] = height;
	}
close("imageSizeList.txt");
}
/^.*.png/{
if (match($0,pngRegexp) !=0) {
	sub(hRegexp,"");
	sub(wRegexp,"");
	match($0,pngRegexp);
	fname = substr($0, RSTART, RLENGTH);
	nmRegexp = fname"\"";
	sub(nmRegexp,("&" sprintf(" WIDTH=\"%i\" HEIGHT=\"%i\" ",width, h[fname])));
	print $0 >> FILENAME".tmp"
	next;
   }
}
{print $0 >> FILENAME".tmp"}
