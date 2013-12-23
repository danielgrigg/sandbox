require 'jams_connect'

host_cmd = "invokeService:JAMS method:'host' onDevice:%s"
enable_cores_cmd = "invokeService:JAMS method:'propCoredumps:unlimited' onDevice:%s"
disable_traps_segv_cmd = "invokeService:JAMS method:'propTrapsegfaults:0' onDevice:%s"


ActiveRecord::Base.connection.listen("JAMS") {|msg|
  puts "#{msg}"
}

trucks = Truck.find(:all)
trucks.each {|truck|
  puts "Invoking command on : #{truck}"
  ActiveRecord::Base.connection.command('replicator', host_cmd % [truck.name])
  ActiveRecord::Base.connection.command('replicator', enable_cores_cmd % [truck.name])
  ActiveRecord::Base.connection.command('replicator', disable_traps_segv_cmd % [truck.name])
}

