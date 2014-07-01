class Foo
  @@x = [1,2,3]

  def self.x
    @@x
  end

  def self.bar
    puts "bar: #{self.x}"
  end
end

Foo.bar
