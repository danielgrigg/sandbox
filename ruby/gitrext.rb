#!/usr/bin/env ruby

require 'fileutils'

abort("need filename") if ARGV.size < 2

to_rename = ARGV[0]
new_ext = ARGV[1]

%x[git mv to_rename "#{File.dirname(to_rename)}/#{File.basename(to_rename, File.extname(to_rename))}.#{new_ext}"]

