
rgrep -siI '#include.*<[a-z|/\_0-9]+(\.hpp)?>' * | egrep -v '^Tests' | egrep -v '_afx.hpp' | egrep -o '#include.*<.*>' | sort | uniq > j2_afx.hpp
# why won't grep like my \w etc? :(
rgrep -siI '#include.*<[a-z|/\_0-9]+(\.hpp)?>' * | egrep -o '#include.*<.*>' | sort | uniq > ../unittest-all_afx.hpp

 # Seperate src from test depends
 diff -u j2_afx.hpp unittest-all_afx.hpp | egrep '^\+'

