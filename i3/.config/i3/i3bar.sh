#!/bin/bash

function volume {
	local amixer=$(awk '/dB/' <(amixer sget Master))
	if echo $amixer | grep off > /dev/null; then
		echo "Muted"
	else
		echo $amixer | awk -F"[][]" '{print $2}'
	fi
}

while :
do
	vol=$(volume)
	bat=$(awk -F", " '/Battery/ {print $2}' <(acpi -b))
echo	'Charge:' $bat'  Vol:' $vol'  ' $(date '+%a %d %b %T')
	sleep 1
done

