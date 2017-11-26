#!/bin/bash

while :
do
	vol=$(awk -F"[][]" '/dB/ { print $2 }' <(amixer sget Master))
	bat=$(awk -F", " '/Battery/ {print $2}' <(acpi -b))
echo	'Charge:' $bat'  Vol:' $vol'  ' $(date '+%a %d %b %T')
	sleep 1
done
