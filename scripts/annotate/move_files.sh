#!/bin/bash

set -ue

D=$(readlink -f $(dirname ${BASH_SOURCE[0]}))

source $D/../get_global_vars.sh

for fullfile in `ls -d ../../intermediate-data/*/*/models/*model`; do
    path=$(dirname -- "$fullfile")
    filename=$(basename -- "$fullfile")
    extension="${filename##*.}"
    filename="${filename%.*}"

    filename="${filename%.*}"

    new_filename=$path"/"$filename".$extension.gender"
    echo $fullfile $new_filename
    #mv $fullfile $new_filename
    
done