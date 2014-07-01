MACRO(INCLUDE_PRECOMPILED_HEADER _targetName _pch_target)
  ADD_DEPENDENCIES(${_targetName} ${_pch_target} )
  SET_TARGET_PROPERTIES(${_targetName} 
    PROPERTIES      
    COMPILE_FLAGS "-include ${_pch_target}.hpp -Winvalid-pch")
ENDMACRO(INCLUDE_PRECOMPILED_HEADER)

MACRO(ADD_PRECOMPILED_HEADER _pch_target)
ADD_CUSTOM_COMMAND(
  OUTPUT "${_pch_target}.hpp.gch"
  COMMAND ${CMAKE_CXX_COMPILER}
  ${CMAKE_CXX_FLAGS}
  -x c++-header
  -o "${_pch_target}.hpp.gch"
  "${_pch_target}.hpp")
include_directories(${Boost_INCLUDE_DIR})
ADD_CUSTOM_TARGET(${_pch_target} DEPENDS "${_pch_target}.hpp.gch" )
ENDMACRO(ADD_PRECOMPILED_HEADER)


