#!/bin/bash

declare -a arr=("GSE49644" "GSE58252" "GSE59922" "GSE43495" "GSE36081" "GSE26391")

for i in "${arr[@]}"
do
	Rscript EMT_GEO.R $i
done