require 'benchmark'

a = 'foo'
b = 'foo'
while (true) do
  #100000.times { a += ' foo' }
  1000000.times { b << ' foo' }
  puts b.length

  gets
end
