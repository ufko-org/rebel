#!/bin/sh

/bin/rm -rf tests-fs/

mkdir -p tests-fs/empty
mkdir -p tests-fs/simple
mkdir -p tests-fs/hidden/.config
mkdir -p tests-fs/links
mkdir -p tests-fs/fifos
mkdir -p tests-fs/execs
mkdir -p tests-fs/perms

echo "aaa" > tests-fs/simple/a.txt
echo -n "" > tests-fs/simple/b.bin

echo "" > tests-fs/hidden/.dotfile
echo "cfg" > tests-fs/hidden/.config/settings

echo "X" > tests-fs/links/file.txt
ln -s file.txt tests-fs/links/file-link
ln -s ../simple tests-fs/links/dir-link

mkfifo tests-fs/fifos/pipe

echo "#!/bin/ksh" > tests-fs/execs/run.sh
chmod +x tests-fs/execs/run.sh

echo "#!/usr/bin/env rebel" > tests-fs/execs/script.rbl
chmod +x tests-fs/execs/script.rbl

echo "ro" > tests-fs/perms/ro.txt
chmod 400 tests-fs/perms/ro.txt

echo "wo" > tests-fs/perms/wo.txt
chmod 200 tests-fs/perms/wo.txt

echo "no" > tests-fs/perms/no.txt
chmod 000 tests-fs/perms/no.txt
