require 'benchmark'

Benchmark.bm do |x|
  x.report do
    a = 'foo'
    100000.times { a += ' foo' }
  end
  x.report do
    a = 'foo'
    100000.times { a << ' foo' }
  end
end

