#!/bin/bash

#rm -f enterprise-numbers.txt
#wget http://www.iana.org/assignments/enterprise-numbers.txt
./enterprise-numbers.pl enterprise-numbers.txt > ipmi-iana-enterprise-numbers-spec.c

# Determine max index, last 30 lines of the file is sufficient
output=`tail -n 30 enterprise-numbers.txt`
max=0
for i in $output
do
    if [[ $i =~ ^[0-9]+$ ]] ; then
	if [ $i -gt $max ] ; then
	    max=$i
	fi
    fi
done

sed -i -e "s/IPMI_IANA_ENTERPRISE_ID_MAX \(.*\)/IPMI_IANA_ENTERPRISE_ID_MAX ${max}/" ../include/freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h
