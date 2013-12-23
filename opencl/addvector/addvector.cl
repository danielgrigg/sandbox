// A program with a single kernel to add two vectors
__kernel void addVector (__global const float* A,
                     __global const float* B,
                     __global float* result,
                     const int count)
{
  const int idx = get_global_id(0);
  if (idx < count)
    result[idx] = A[idx] + B[idx];
}

