#!/bin/bash
# Author: Joseph Pecoraro
# Date: Friday, September 18, 2009
# Description: Bash Script to Test the Haskell Solution

# Interpreter
GHCI='ghci -v0'

# Run Tests
{
  echo ":load seq";
  echo 'let tst = (\a -> if a then "PASS" else ">> FAIL <<")'; # Cleaner Output
  echo "tst $ take' 5 ones'                        == [1,1,1,1,1]        ";
  echo "tst $ take' 5 (map' (1+) ones')            == [2,2,2,2,2]        ";
  echo "tst $ take' 5 (zipWith' (+) ones' ones')   == [2,2,2,2,2]        ";
  echo "tst $ take' 5 ints'                        == [1,2,3,4,5]        ";
  echo "tst $ take' 5 (iterate' (1+) 1)            == [1,2,3,4,5]        ";
  echo "tst $ take' 5 evens'                       == [2,4,6,8,10]       ";
  echo "tst $ take' 5 odds'                        == [1,3,5,7,9]        ";
  echo "tst $ take' 5 squares'                     == [1,4,9,16,25]      ";
  echo "tst $ take' 5 (powers' 2)                  == [2,4,8,16,32]      ";
  echo "tst $ take' 5 (square' 2)                  == [2,4,16,256,65536] ";
  echo "tst $ take' 5 (seq' 0 ints')               == [0,1,2,3,4]        ";
  echo "tst $ take' 5 (mapl' (+) (seq' 0 ints'))   == [1,3,5,7,9]        ";
  echo "tst $ take' 5 (fibs' 1 2)                  == [1,2,3,5,8]        ";
  echo "tst $ take' 5 (total' (+) 0 ints')         == [1,3,6,10,15]      ";
  echo "tst $ take' 5 facts'                       == [1,2,6,24,120]     ";
  echo
} | tee /dev/tty | $GHCI
