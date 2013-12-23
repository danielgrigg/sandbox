ps = (1..50).map {|x| [rand(100), rand(100)] }
h_ps = ps.map {|x, y| "(Vector2 #{x} #{y})" }
puts "[#{h_ps.join ","}]"

g_ps = ps.map {|x, y| "#{x}    #{y}"}
puts g_ps

