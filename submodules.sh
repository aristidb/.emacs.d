#!/bin/bash
git submodule init
git submodule update
git submodule foreach "git pull origin master ; make clean ; make all ; true"
