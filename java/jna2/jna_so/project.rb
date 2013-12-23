PROJECT = 
{
  :name => "my_lib", 
  :cmake_version => "2.6",

  :targets => 
  [{
    :name => "my_lib",
    :type => :shared, # (:static, :shared, :executable)
    :install => true, 
    :sources => "src", # Source root

    :common => 
    {
      :packages => [], # list of package dictionaries
      :definitions => [], # string-list of extra compiler definitions
      :include_dirs => [], # string-list of extra include dirs
      :link_dirs => [], # string-list of extra link-dirs
      :libs => [] # string-list of extra libs
    },
    # :apple, :linux and :windows platforms are optional 
    :apple => 
    {
      :packages => [], 
      :definitions => [], 
      :include_dirs => [],
      :link_dirs => [] 
    },
    :linux => 
    {
      :packages => [],
      :definitions => [],
      :include_dirs => [],
      :link_dirs => [] 
    }
  }]
}

