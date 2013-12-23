#!/bin/sh

ruby -ne 'puts $1 if $_ =~ %r{heading\|(.*)\|}' < dragin_io.log > headings
ruby -ne 'puts $1 if $_ =~ %r{bucket_reach\|(.*)\|}' < dragin_io.log > bucket_reach
ruby -ne 'puts $1 if $_ =~ %r{bucket_height\|(.*)\|}' < dragin_io.log > bucket_height

