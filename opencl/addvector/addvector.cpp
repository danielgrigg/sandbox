#include <fstream>
#include <iostream>
#include <stdint.h>
#include <string>
#include <vector>
#include <algorithm>
#include <iterator>
#include "LTextFileReader.h"

#include <OpenCL/opencl.h>

using namespace std;

template<class T>
void printVector(const char* name, const vector<T>& v)
{
  ostream_iterator<T> iter(cout, " "); 
  cout << name << ": [ ";
  std::copy(v.begin(), v.end(), iter);
  cout << "]\n";
}

float smallRandom()
{
  return random() % 100;
}

float addFloats(float a, float b)
{
  return a + b;
}


#include <map>

map<string, cl_kernel> gKernels;
cl_device_id gDevice;
cl_command_queue gCommands;
cl_context gContext;

// Build context and all kernels.
bool initCL(map<string, cl_kernel>& kernels)
{
  cl_int err;
  // Boring case of a single command-queue paired with a single device.
  clGetDeviceIDs(NULL, CL_DEVICE_TYPE_GPU, 1, &gDevice, NULL);
  gContext = clCreateContext(0, 1, &gDevice, NULL, NULL, &err);
  gCommands = clCreateCommandQueue(gContext, gDevice, 0, &err);

  string source;
  if (!LTextFileReader::read("addvector.cl", source)) { cerr << "where's addvector.cl?";}
  const char* sourceStr = source.c_str();
  cl_program program = clCreateProgramWithSource(gContext, 1, &sourceStr, NULL, &err);
  
  err = clBuildProgram(program, 0, NULL, NULL, NULL, NULL);
  if (err != CL_SUCCESS)
  {
    size_t len;
    char buffer[2048];
    printf("Error: Failed to build program executable!\n");
    clGetProgramBuildInfo(program, gDevice, CL_PROGRAM_BUILD_LOG, sizeof(buffer), buffer, &len);
    printf("%s\n", buffer);
    return false;
  }
  kernels["addVector"] = clCreateKernel(program, "addVector", &err);
  return true;
}

void printError(int err)
{
  static std::map<int,const char*> codes;
  if (codes.empty())
  {
    codes[CL_INVALID_COMMAND_QUEUE] = "invalid command queue";
    codes[CL_INVALID_CONTEXT] = "invalid context";
    codes[CL_INVALID_EVENT_WAIT_LIST] = "invalid event wait list";
    codes[CL_INVALID_GLOBAL_OFFSET] = "invalid global offset";
    codes[CL_INVALID_KERNEL] = "invalid kernel";
    codes[CL_INVALID_KERNEL_ARGS] = "invalid kernel args";
    codes[CL_INVALID_PROGRAM_EXECUTABLE] = "invalid program executable";
    codes[CL_INVALID_WORK_DIMENSION] = "invalid work dimension";
    codes[CL_INVALID_WORK_GROUP_SIZE] = "invalid work group size";
    codes[CL_INVALID_WORK_ITEM_SIZE] = "invalid work item size";
    codes[CL_MEM_OBJECT_ALLOCATION_FAILURE] = "mem object allocation failure";
    codes[CL_OUT_OF_HOST_MEMORY] = "out of host memory";
    codes[CL_OUT_OF_RESOURCES] = "out of resources";
  }
  if (err != CL_SUCCESS)
  {
    cerr << "CL error: " << codes[err] << endl;
  }
}

void addVectorCL(const vector<float>& A,
    const vector<float>& B,
    vector<float>& result)
{
  int err;
  // In practice you'd want to reuse buffers ..
  cl_mem ABuffer = clCreateBuffer(gContext,  CL_MEM_READ_ONLY,  sizeof(float) * A.size(), NULL, NULL);
  cl_mem BBuffer = clCreateBuffer(gContext,  CL_MEM_READ_ONLY,  sizeof(float) * B.size(), NULL, NULL);
  cl_mem resultBuffer = clCreateBuffer(gContext, CL_MEM_WRITE_ONLY, sizeof(float) * result.size(), NULL, NULL);

  // Blocking buffer update
  err = clEnqueueWriteBuffer(gCommands, ABuffer, CL_TRUE, 0, sizeof(float) * A.size(), &A[0], 0, NULL, NULL);
  err = clEnqueueWriteBuffer(gCommands, BBuffer, CL_TRUE, 0, sizeof(float) * B.size(), &B[0], 0, NULL, NULL);

  size_t elementsSize = min(min(A.size(), B.size()), result.size());
  cl_kernel k = gKernels["addVector"];
  err = clSetKernelArg(k, 0, sizeof(cl_mem), &ABuffer);
  err = clSetKernelArg(k, 1, sizeof(cl_mem), &BBuffer);
  err = clSetKernelArg(k, 2, sizeof(cl_mem), &resultBuffer);
  err = clSetKernelArg(k, 3, sizeof(uint32_t), &elementsSize);
  size_t local;
  size_t global = elementsSize;
  err = clGetKernelWorkGroupInfo(k, gDevice, CL_KERNEL_WORK_GROUP_SIZE, sizeof(local), &local, NULL);
  err = clEnqueueNDRangeKernel(gCommands, k, 1, NULL, &global, &local, 0, NULL, NULL);
  printError(err);

  clFinish(gCommands);

  clEnqueueReadBuffer( gCommands, resultBuffer, CL_TRUE, 0, sizeof(float) * elementsSize, &result[0], 0, NULL, NULL );  
}

int main(int argc, char **argv)
{
  size_t global;                      // global domain size for our calculation
  size_t local;                       // local domain size for our calculation

  const int kSize = 10;
  vector<float> A(kSize);
  std::generate(A.begin(), A.end(), smallRandom);
  printVector("A", A);
  vector<float> B(kSize);
  std::generate(B.begin(), B.end(), smallRandom);
  printVector("B", B);

  vector<float> stlResult(kSize);
  std::transform(A.begin(), A.end(), B.begin(), stlResult.begin(), addFloats);
  printVector("addVectorSTL", stlResult);

  vector<float> clResult(kSize);
  
  initCL(gKernels);
  addVectorCL(A, B, clResult);
  printVector("addVectorCL", clResult);

  bool sameResult = std::equal(stlResult.begin(), stlResult.end(), clResult.begin());
  cout << "addVector " << (sameResult ? "succeeded!" : "failed!") << endl;

  return 0;
}
