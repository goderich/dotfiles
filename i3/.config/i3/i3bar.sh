#!/bin/bash

while :
do
	vol=$(awk -F"[][]" '/dB/ { print $2 }' <(amixer sget Master))
echo	'Vol:' $vol'  ' $(date '+%a %d %b %T')
	sleep 1
done
