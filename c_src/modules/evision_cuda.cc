#include "evision_cuda.h"

#ifdef CUDA_ENABLED
#include <cuda_runtime.h>

#include <cstdint>
#include <cstring>
#include <iostream>
#include <sstream>
#include <tuple>
#include <vector>

std::optional<std::pair<std::vector<unsigned char>, int>>
get_cuda_ipc_handle(std::uintptr_t ptr) {
  void *device_ptr = reinterpret_cast<void *>(ptr);
  cudaIpcMemHandle_t ipc_handle;
  cudaError_t status = cudaIpcGetMemHandle(&ipc_handle, device_ptr);
  if (status != cudaSuccess) {
    return std::nullopt;
  }

  cudaPointerAttributes attributes{};
  status = cudaPointerGetAttributes(&attributes, device_ptr);
  if (status != cudaSuccess) {
    return std::nullopt;
  }

  // Assuming sizeof(cudaIpcMemHandle_t) is constant
  const size_t size = sizeof(cudaIpcMemHandle_t);

  // Copy the memory handle to a byte array
  std::vector<unsigned char> result(size);
  memcpy(result.data(), &ipc_handle, size);

  return std::make_pair(result, attributes.device);
}

std::pair<void *, std::string> get_pointer_for_ipc_handle(uint8_t *handle_bin,
                                                          size_t handle_size,
                                                          int device_id) {
  std::ostringstream error_msg_stream;
  if (handle_size != sizeof(cudaIpcMemHandle_t)) {
    error_msg_stream << "IPC handle size does not match "
                        "sizeof(cudaIpcMemHandle_t), IPC handle size = "
                     << handle_size << ", sizeof(cudaIpcMemHandle_t) = "
                     << sizeof(cudaIpcMemHandle_t);
    return std::make_pair(nullptr, error_msg_stream.str());
  }

  unsigned char ipc_handle_data[sizeof(cudaIpcMemHandle_t)];
  for (size_t i = 0; i < sizeof(cudaIpcMemHandle_t); i++) {
    ipc_handle_data[i] = handle_bin[i];
  }

  cudaIpcMemHandle_t ipc_handle;
  memcpy(&ipc_handle, ipc_handle_data, sizeof(cudaIpcMemHandle_t));

  void *ptr = nullptr;
  cudaError_t cuda_status = cudaSetDevice(device_id);
  if (cuda_status != cudaSuccess) {
    error_msg_stream << "Error setting CUDA device: "
                     << cudaGetErrorString(cuda_status);
    return std::make_pair(nullptr, error_msg_stream.str());
  }

  cuda_status = cudaIpcOpenMemHandle((void **)&ptr, ipc_handle,
                                     cudaIpcMemLazyEnablePeerAccess);
  if (cuda_status != cudaSuccess) {
    error_msg_stream << "Error opening CUDA IPC memory handle: "
                     << cudaGetErrorString(cuda_status);
    return std::make_pair(nullptr, error_msg_stream.str());
  }

  return std::make_pair(ptr, "");
}

std::optional<int64_t> get_gpumat_device_id(void * ptr) {
  if (ptr == nullptr) {
    return std::nullopt;
  }
  cudaPointerAttributes attributes{};
  cudaError_t status = cudaPointerGetAttributes(&attributes, ptr);
  if (status != cudaSuccess) {
    return std::nullopt;
  }
  return attributes.device;
}
#else
std::optional<std::pair<std::vector<unsigned char>, int>>
get_cuda_ipc_handle(std::uintptr_t ptr) {
  (void)ptr;
  return std::nullopt;
}

std::pair<void *, std::string> get_pointer_for_ipc_handle(uint8_t *handle_bin,
                                                          size_t handle_size,
                                                          int device_id) {
  (void)handle_bin;
  (void)handle_size;
  (void)device_id;
  return std::make_pair(nullptr, "CUDA is not enabled in this build");
}

std::optional<int64_t> get_gpumat_device_id(void * ptr) {
  (void)ptr;
  return std::nullopt;
}
#endif
