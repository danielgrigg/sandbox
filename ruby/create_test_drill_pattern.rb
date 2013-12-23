
def usage
  puts "create_test_drill_pattern <pattern_name> <radius> <num_points> [noise_factor]"
end

def randomRange(min, max)
  min + rand() * (max - min)
end

def stratifiedPoints(n, noise)
  m = n*n
  0.upto(m-1).to_a.map! {|i|
    x,y = (i%n),(i/n)
    [(x.to_f + noise*rand()) / n, (y.to_f + noise*rand()) / n]
  }
end

def uniformPoints(n)
  m = n*n
  0.upto(m-1).to_a.map! {|i|
    x,y = (i%n),(i/n)
    [(x+1).to_f / n, (y+1).to_f / n]
  }
end

def unitPointToBounds(xy, bounds)
    [(bounds[0] + xy[0] * (bounds[2] - bounds[0])).to_i,
     (bounds[1] + xy[1] * (bounds[3] - bounds[1])).to_i]
end

def createDrillPoints(pattern, elevation, ps)
  ps.each_with_index {|xy,i|
#    puts "creating point #{i} at #{xy[0]}, #{xy[1]}"
    p = DrillPoint.new
    p.pattern = pattern
    p.latitude = xy[0]
    p.longitude = xy[1]
    p.elevation = elevation
    p.depth = 20
    p.drilled = false
    p.point_name = "#{i}"
    if (rand() > 0.7)
      p.drilled_latitude = p.latitude
      p.drilled_longitude = p.longitude
      p.drilled_elevation = p.elevation
      p.drilled_depth = 17.0
      p.drilled = true
    end
    p.save!
  }
end

def createTestDrillPointInBounds(pattern, pointName, bounds, elevation)
  p = DrillPoint.new
  p.pattern = pattern
  p.latitude = randomRange(bounds[0], bounds[2])
  p.longitude = randomRange(bounds[1], bounds[3])
  p.elevation = elevation
  p.depth = 20
  p.drilled = false
  p.point_name = pointName
  puts "#{p}"
  p
end

if (ARGV.length < 3)
  usage
  exit
end

require 'jams_connect'
patternName = ARGV[0]
radius = ARGV[1].to_i
radius = radius <= 0 ? 1000 : radius
numPoints = ARGV[2].to_i
numPoints = numPoints <= 0 ? 10 : numPoints
noise = ARGV.length >= 4 ? ARGV[3].to_f : 1.0
noise = noise < 0.0 ? 0.0 : noise
noise = noise > 1.0 ? 1.0 : noise
puts "noise #{noise*100.0}%"

pattern = DrillPattern.find_by_name(patternName)
if (!pattern)
  $stderr.puts "Pattern #{patternName} does not exist, aborting."
  exit
end

loc = pattern.location
if (!loc)
  $stderr.puts "No location for #{patternName}"
  exit
end

bounds = [loc.latitude - radius, loc.longitude - radius, 
          loc.latitude + radius, loc.longitude + radius]
#if mode =~ /replace/
  
  DrillPoint.delete_all
  n = (Math.sqrt(numPoints).ceil).to_i
  puts "Replacing #{patternName} DrillPoints with #{n*n} test points over #{radius} radius."
  ps = stratifiedPoints(n, noise)
#  ps = uniformPoints(n)
  ps.map!{|p| unitPointToBounds(p, bounds)}
  createDrillPoints(pattern, 5600, ps)
#end

