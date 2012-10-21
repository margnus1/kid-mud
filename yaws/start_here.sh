#!/bin/bash
# Copyright (c) 2012 Magnus Lång, Mikael Wiberg and Michael Bergroth, Eric Arnerlöv
# See the file license.txt for copying permission.
echo $*
yaws -i --conf yaws.conf --name kidserver --pa ../ebin --mnesiadir "../database" --runmod kidmud