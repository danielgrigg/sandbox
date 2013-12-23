def bar(x,&block)
 if (x < 1)
   yield 42
 else
   yield x
   bar(x - 1,&block)
 end
    
end

def foo(&block)
  bar(10, &block)
end

foo() {|y| puts y}
