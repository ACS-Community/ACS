#!/bin/bash

for number in 1 10 20 30 50 100; do
	echo "$number"

	mkdir -p $number
	
	for I in $(seq $number); do
		FILE=$(echo $I"_NC")
		ncConsumer > $number/$FILE &
	done
	sleep 25
	./testNCBenchmarkClient.py
	sleep 25
	
	for I in $(seq $number); do
		FILE=$(echo $I"_DDSNC")
		ddsSubscriber > $number/$FILE &
	done
	sleep 25
	./testDDSNCBenchmarkClient.py
	sleep 25

done

