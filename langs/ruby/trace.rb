class Foo
  def a
    return 42
  end
  def b
    return 23
  end
end

set_trace_func proc {|event, file, line, id, binding, classname|
  if classname == Foo && event == call
    $stderr.puts "#{file}:#{line} #{id}"
  end
}

