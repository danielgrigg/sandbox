MACRO(ADD_PRECOMPILED_HEADER _targetName)
  SET(STDAFX "${_targetName}_afx")
  message ("flags ${CMAKE_CXX_FLAGS} ${CMAKE_CURRENT_SOURCE_DIR}")

  ADD_CUSTOM_COMMAND(
    OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/${STDAFX}.hpp"
    COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/${STDAFX}.hpp"
    ${CMAKE_CURRENT_BINARY_DIR} 
    )

  STRING(TOUPPER "CMAKE_CXX_FLAGS_${CMAKE_BUILD_TYPE}" _flags_var_name)
  SET(_compile_FLAGS ${${_flags_var_name}})
  GET_DIRECTORY_PROPERTY(_directory_flags INCLUDE_DIRECTORIES)
  FOREACH(item ${_directory_flags})
    LIST(APPEND _compile_FLAGS "-I${item}")
  ENDFOREACH(item)

  ADD_CUSTOM_COMMAND(
    OUTPUT "${STDAFX}.hpp.gch"
    COMMAND ${CMAKE_CXX_COMPILER}
    ${_compile_FLAGS} 
    #    -I ${CMAKE_CURRENT_SOURCE_DIR}
    -x c++-header
    -o "${STDAFX}.hpp.gch"
    "${CMAKE_CURRENT_SOURCE_DIR}/${STDAFX}.hpp"
    DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${STDAFX}.hpp"
    )
  #  include_directories(${Boost_INCLUDE_DIR})
  ADD_CUSTOM_TARGET(${STDAFX} DEPENDS "${STDAFX}.hpp.gch" )

  #message ("afx_build ${CMAKE_CURRENT_BINARY_DIR}/${STDAFX}.hpp.gch")

  #  ADD_DEPENDENCIES(${_targetName} ${STDAFX})
  #  SET_TARGET_PROPERTIES(${_targetName} 
  #  PROPERTIES      
  #  COMPILE_FLAGS "-include ${CMAKE_CURRENT_BINARY_DIR}/${STDAFX}.hpp -Winvalid-pch"
  #  )

ENDMACRO(ADD_PRECOMPILED_HEADER)

MACRO(INCLUDE_PRECOMPILED_HEADER _targetName _afx)
  ADD_DEPENDENCIES(${_targetName} ${_afx})
  SET_TARGET_PROPERTIES(${_targetName} 
    PROPERTIES      
    COMPILE_FLAGS "-include ${CMAKE_CURRENT_BINARY_DIR}/${_afx}.hpp -Winvalid-pch")
ENDMACRO(INCLUDE_PRECOMPILED_HEADER)


