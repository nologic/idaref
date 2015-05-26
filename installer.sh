#!/bin/bash

IDA_PATH='/Applications/IDA Pro 6.8'
DST="${IDA_PATH}/idaq.app/Contents/MacOS/plugins/"

cp -v idaref.py *.sql "$DST"
chmod +x "$DST/idaref.py"