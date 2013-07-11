#!/bin/bash
for test in tests/*.pl
do
    swipl -s $test -t run_tests -g true
done

