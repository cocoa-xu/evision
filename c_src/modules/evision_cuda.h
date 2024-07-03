#ifndef EVISION_CUDA_H
#define EVISION_CUDA_H
#pragma once

#include <cstdint>
#include <vector>
#include <optional>
#include <string>

std::optional<std::pair<std::vector<unsigned char>, int>> get_cuda_ipc_handle(std::uintptr_t ptr);
std::pair<void *, std::string> get_pointer_for_ipc_handle(uint8_t*, size_t, int);
std::optional<int64_t> get_gpumat_device_id(void * ptr);

#endif  // EVISION_CUDA_H
