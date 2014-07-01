-- Respond to any events from Turtle
function event(name)
  if name == "finished" then
    print ("finished!")
  end
end

Canvas(500,500)   -- Canvas width, height
Origin(200,200,0) -- Init turtle to an x,y,heading

for v = 1, 4, 1 do
  Draw(50)    -- Draw 50 pixels forward
  Forward(10) -- Throw in a little stipple, mmm stipple.
  Left(90)    -- Turn left without moving
  Draw(50)
  Forward(10)
  Right(45)
  Draw(50)
  Forward(10)
  Left(90)
  Draw(50)
  Forward(10)
  Right(45)
  Forward(10)
end

Save("simple")   -- Save to simple.tga
