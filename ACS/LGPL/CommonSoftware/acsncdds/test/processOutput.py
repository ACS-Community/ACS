#!/usr/bin/env python

import sys
import re


def increment_cell(c):
	if len(c)==1:
		if c=="Z":
			return "AA"
		else:
			return chr(ord(c)+1)
	else:
		s=""
		l = len(c)
		i = 0
		while i < l-1:
			s = s + c[i]
			i+=1
		if c[len(c)-1] == "Z":
			return increment_cell(s)+"A"
		else:
			return s+chr(ord(c[l-1])+1)


dir = sys.argv[1]
qty= int(dir)

csv=open(dir+"/output.csv","w")

table = []
dds = []
nc = []

p = re.compile("(\d+),(\d+)")

for i in range(1,qty+1):
	nc_t=[str(i)+"_NC"]
	dds_t=[str(i)+"_DDSNC"]
	nc_f=open(dir+"/"+str(i)+"_NC","r")
	dds_f=open(dir+"/"+str(i)+"_DDSNC","r")
	for line in nc_f:
		try:
			l = int(p.match(line).group(2))
			nc_t.append(l)
		except:
			pass
	for line in dds_f:
		l=int(p.match(line).group(2))
		dds_t.append(l)
	dds.append(dds_t)
	nc.append(nc_t)
	nc_f.close()
	dds_f.close()
	
for c in dds:
	table.append(c)

for c in nc:
	table.append(c)


ex=0
i=0
while ex<(qty*2):
	line=""
	ex=0
	for c in table:
		try:
			line=line+str(c[i])+","
		except:
			line=line+","
			ex=ex+1
	csv.write(line+"\n")
	i=i+1

k=0
cells=[]
csv.write("\n\n\n")

line=""
cell="A"
for j in range (1, qty*2+1):
	cells.append(cell)
	cell = increment_cell(cell)

avg=""
std=""
count=""
i=0
for cell in cells:
	if i < len(dds):
		avg+="=AVERAGE("+cell+"2:"+cell+str(len(dds[i]))+"),"
		std+="=STDEV("+cell+"2:"+cell+str(len(dds[i]))+"),"
		count+="=COUNT("+cell+"2:"+cell+str(len(dds[i]))+"),"
	else:
		avg+="=AVERAGE("+cell+"2:"+cell+str(len(nc[i-len(dds)]))+"),"
		std+="=STDEV("+cell+"2:"+cell+str(len(nc[i-len(dds)]))+"),"
		count+="=COUNT("+cell+"2:"+cell+str(len(nc[i-len(dds)]))+"),"
	i+=1

csv.write(avg+"\n")
csv.write(std+"\n")
csv.write(count+"\n")
csv.write("\n\nCONSIDERING ALL THE SAMPLES\n")
cell="A"
base="A"
for j in range(qty-2):
	cell=increment_cell(cell)
base=cell
base=increment_cell(base)
basenc=increment_cell(base)
for j in range(qty+1):
	cell=increment_cell(cell)

csv.write("=AVERAGE(A"+"2:"+base+"1001),=AVERAGE("+basenc+"2:"+cell+"1001\n");
csv.write("=STDEV(A"+"2:"+base+"1001),=STDEV("+basenc+"2:"+cell+"1001)\n");
csv.write("=COUNT(A"+"2:"+base+"1001),=COUNT("+basenc+"2:"+cell+"1001\n");

csv.close()
