#!/bin/bash
find . -type f -perm -og+rx -depth 1 -delete
