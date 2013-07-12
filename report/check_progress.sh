#!/usr/bin/env bash

#
# Script reads plan.txt and looks for occurences of each line
# in files in subdirectories.
# plan.txt is written in the form of questions to be answered
# somewhere in these files. We expect the files to cite these
# questions while answering them.
#

show="$1"

test -z $show && show=all;


if [[ $show == 'answered' || $show == 'all' ]]; then
    show_answered=true;
else
    show_answered=false;
fi

if [[ $show == 'unanswered' || $show == 'all' ]]; then
    show_unanswered=true;
else
    show_unanswered=false;
fi


cat plan.txt | while read i; do
    # Remove comments
    echo "$i" | grep '^\s*#' -q && continue;
    # Remove empty lines
    echo "$i" | grep '^\s*$' -q && continue;

    grep -l -q -d recurse "$i" */* && answered=true || answered=false

    if ( $answered ); then
        test $show_answered == true && echo "$i" ANSWERED
    else
        test $show_unanswered == true && echo "$i" NOT ANSWERED
    fi
done
