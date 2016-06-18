#!/bin/bash

function install_idaref {
	IDA_PATH=`mdfind 'kMDItemDisplayName==idaq&&kMDItemKind==Application'`
	DST="${IDA_PATH}/Contents/MacOS/plugins/"

	cp -v -r idaref.py archs "$DST"
	chmod +x "$DST/idaref.py"
}

case `uname` in
	Darwin*) install_idaref ;;
	*)       echo 'Installer only works on OSX at the moment' ;;
esac