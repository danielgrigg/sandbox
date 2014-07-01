def floatEqual(x,y)
  (x-y).abs <= 5E-4 
end

class Vertex
  attr_reader :p
#  attr_reader :n
#  attr_reader :uv
  #
  def initialize(x,y,z)
    @p = [x,y,z]
  end

  def eql? (other)
    return floatEqual(p[0], other.p[0]) &&
      floatEqual(p[1], other.p[1]) &&
      floatEqual(p[2], other.p[2]);
  end
end
