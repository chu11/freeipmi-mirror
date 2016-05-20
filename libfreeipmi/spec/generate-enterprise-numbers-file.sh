#!/bin/sh

rm enterprise-numbers
wget http://www.iana.org/assignments/enterprise-numbers
./enterprise-numbers.pl enterprise-numbers > ipmi-iana-enterprise-numbers-spec.c
