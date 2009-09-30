#!/bin/bash
# Author: Joseph Pecoraro
# Date: Saturday, September 26, 2009
# Description: Run All Haskell Tests

./test_runner -v Tester.lhs
./test_runner -v Subset.lhs
./test_runner -v Select.lhs
./test_runner -v Matrix.lhs
