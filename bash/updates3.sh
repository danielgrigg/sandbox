function sourceReplace {
  find . -type f -regex ".*\.[cph]*" -exec sed -i '' -e 's/[[:<:]]$1[[:>:]]/$2/g' {} \;
}

# Use stdint.h
sourceReplace uint uint32_t
sourceReplace ui8_t uint8_t
sourceReplace i8_t int8_t
sourceReplace ui16_t uint16_t
sourceReplace i16_t int16_t
sourceReplace ui32_t uint32_t
sourceReplace i32_t int32_t
sourceReplace ui64_t uint64_t
sourceReplace i64_t int64_t
find . -type f -regex ".*\.[cph]*" -exec sed -i '' -e 's/"lgstypes.h"/<stdint.h>/g' {} \;

# Update 'friend MyClass' -> 'friend class MyClass'

# Make globals more noticeable...

sourceReplace mach g_mach;
