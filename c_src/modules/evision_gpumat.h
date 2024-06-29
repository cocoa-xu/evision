#ifndef EVISION_OPENCV_GPUMAT_H
#define EVISION_OPENCV_GPUMAT_H

#include <erl_nif.h>
#include <sstream>
#include <string>
#include "evision_cuda.h"
#include "evision_cuda_ipc.h"
#include "../nif_utils.hpp"
#include "evision_mat_utils.hpp"

// @evision c: cuda_cuda_GpuMat_to_pointer,evision_cv_cuda_cuda_GpuMat_to_pointer,1
// @evision nif: def cuda_cuda_GpuMat_to_pointer(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_cuda_cuda_GpuMat_to_pointer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        std::string pointer_kind;

        if (!evision_to_safe(env, evision_get_kw(env, erl_terms, "mode"), pointer_kind, ArgInfo("mode", 1))) {
            return enif_make_badarg(env);
        }

        ERL_NIF_TERM self = evision_get_kw(env, erl_terms, "img");
        Ptr<cv::cuda::GpuMat> * self1 = 0;
        if (evision_cuda_GpuMat_getp(env, self, self1)) {
            Ptr<cv::cuda::GpuMat> _self_ = *(self1);

            ERL_NIF_TERM out_term{};
            std::uintptr_t ptr = (std::uintptr_t)_self_->cudaPtr();
            ERL_NIF_TERM step_term = enif_make_ulong(env, _self_->step);
            if (pointer_kind == "local") {
                ERL_NIF_TERM ptr_term = enif_make_ulong(env, ptr);
                out_term = enif_make_tuple2(env, ptr_term, step_term);
            }
            else if (pointer_kind == "cuda_ipc") {
                auto result = get_cuda_ipc_handle(ptr);
                if (result.second) {
                    return evision::nif::error(env, "Unable to get cuda IPC handle");
                }
                auto pointer_vec = result.first;

                ErlNifBinary handle_bin;
                enif_alloc_binary(pointer_vec.size(), &handle_bin);
                for (size_t i = 0; i < pointer_vec.size(); i++) {
                    handle_bin.data[i] = pointer_vec[i];
                }
                ERL_NIF_TERM handle_term = enif_make_binary(env, &handle_bin);
                out_term = enif_make_tuple2(env, handle_term, step_term);
            }
            else if (pointer_kind == "host_ipc") {
#ifdef CUDA_HOST_IPC_ENABLED
                size_t host_size = _self_->rows * _self_->channels() * _self_->cols * _self_->elemSize();
                ERL_NIF_TERM size_term = enif_make_ulong(env, host_size);
                std::ostringstream handle_name_stream;
                handle_name_stream << "evision:ipc:" << host_size << ":" << ptr;
                std::string handle_name = handle_name_stream.str();
                int fd = get_ipc_handle((char*)handle_name.c_str(), host_size);

                if (fd == -1) {
                    return evision::nif::error(env, "Unable to get IPC handle");
                }

                void* ipc_ptr = open_ipc_handle(fd, host_size);
                if (ipc_ptr == nullptr) {
                    return evision::nif::error(env, "Unable to open IPC handle");
                }

                if (_self_->isContinuous()) {
                    memcpy(ipc_ptr, (void*)ptr, host_size);
                } else {
                    uint8_t* from_ptr = (uint8_t*)ptr;
                    uint8_t* to_ptr = (uint8_t*)ipc_ptr;

                    // copy the data with correct step
                    for (int i = 0; i < _self_->rows; i++) {
                        auto offset = i * _self_->step;
                        memcpy(to_ptr + offset, from_ptr + offset, _self_->cols * _self_->channels() * _self_->elemSize());
                    }
                }

                ErlNifBinary handle_name_bin;
                enif_alloc_binary(handle_name.size(), &handle_name_bin);
                for (size_t i = 0; i < handle_name.size(); i++) {
                    handle_name_bin.data[i] = handle_name[i];
                }
                ERL_NIF_TERM handle_name_term = enif_make_binary(env, &handle_name_bin);
                ERL_NIF_TERM fd_term = enif_make_int(env, fd);
                out_term = enif_make_tuple3(env, handle_name_term, fd_term, size_term);   
#else
                return evision::nif::error(env, "mode = :host_ipc is not supported yet.");
#endif
            }
            else {
#ifdef CUDA_HOST_IPC_ENABLED
                return evision::nif::error(env, "mode must be either 'local', 'cuda_ipc' or 'host_ipc'");
#else
                return evision::nif::error(env, "mode must be either 'local' or 'cuda_ipc'");
#endif
            }
            return evision::nif::ok(env, out_term);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: cuda_cuda_GpuMat_from_pointer,evision_cv_cuda_cuda_GpuMat_from_pointer,1
// @evision nif: def cuda_cuda_GpuMat_from_pointer(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_cuda_cuda_GpuMat_from_pointer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    int error_flag = false;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        // 0: local, 1: cuda_ipc, 2: host_ipc
        std::string pointer_kind;
        void* ptr = nullptr;
        unsigned long local_pointer = 0;
        ErlNifBinary cuda_ipc_handle{};
        int fd = -1;
        std::string memname;
        uint64_t device_size = 0;

        std::vector<int64_t> shape;
        std::string dtype;
        int device_id = 0;

        if (!evision_to_safe(env, evision_get_kw(env, erl_terms, "kind"), pointer_kind, ArgInfo("kind", 0))) {
            return enif_make_badarg(env);
        }
        if (!evision_to_safe(env, evision_get_kw(env, erl_terms, "dtype"), dtype, ArgInfo("dtype", 0))) {
            return enif_make_badarg(env);
        }
        if (!evision::nif::get_tuple(env, evision_get_kw(env, erl_terms, "shape"), shape)) {
            return enif_make_badarg(env);
        }
        if (shape.size() > 3 || shape.size() == 0) {
            return evision::nif::error(env, "GpuMat expects shape to be 1 <= tuple_size(shape) <= 3");
        }
        if (!evision::nif::get(env, evision_get_kw(env, erl_terms, "size"), &device_size)) {
            return evision::nif::error(env, "size must be an integer");
        }
        evision_to_safe(env, evision_get_kw(env, erl_terms, "device_id"), device_id, ArgInfo("device_id", 0x8));
        int height;
        int width = 1;
        int cn = 1;
        int type;

        int ndims = shape.size();
        height = shape[0];
        if (ndims >= 2) {
            width = shape[1];
        }
        if (ndims == 3) {
            cn = shape[2];
        }

        if (!get_compact_type(dtype, cn, type)) {
            return evision::nif::error(env, "unsupported type");
        }
        
        ERL_NIF_TERM handle = evision_get_kw(env, erl_terms, "handle");
        int tuple_arity = 0;
        const ERL_NIF_TERM* host_ipc_tuple = nullptr;
        if (pointer_kind == "local" && enif_get_ulong(env, handle, &local_pointer)) {
            ptr = reinterpret_cast<void*>(local_pointer);
        } else if (pointer_kind == "cuda_ipc" && enif_inspect_binary(env, handle, &cuda_ipc_handle)) {
            auto result = get_pointer_for_ipc_handle(cuda_ipc_handle.data, cuda_ipc_handle.size, device_id);
            if (result.second) {
                return evision::nif::error(env, "Unable to get pointer for IPC handle.");
            }
            ptr = result.first;
        } 
        else if (pointer_kind == "host_ipc" && enif_get_tuple(env, handle, &tuple_arity, &host_ipc_tuple) && tuple_arity == 2) {
            // todo: we can open and read from the shared memory here
            // but how do we properly close it? --
            //   it's okay when the user just use it as a view
            //   then we can simply add a callback in `evision_res` to close the shared memory
            //
            // however, --
            //   once the user calls functions that allocates memory inplace, 
            //   (by inplace I mean the same GpuMat instance)
            //   we need to make sure that the shared memory is properly closed
#if 0 // defined(CUDA_HOST_IPC_ENABLED)
            // if (evision::nif::get(env, host_ipc_tuple[0], &fd) && fd != -1 && evision::nif::get(env, host_ipc_tuple[1], memname)) {
            //     return evision::nif::error(env, "Unable to get IPC handle.");
            // }
            // ptr = open_ipc_handle(fd, device_size);
            // if (ptr == nullptr) {
            //     return evision::nif::error(env, "Unable to get pointer for IPC handle.");
            // }
#else
            return evision::nif::error(env, "mode = :host_ipc is not supported on this system.");
#endif
        }
        else {
            return evision::nif::error(env, "Unable to get pointer for IPC handle.");
        }

        if (ptr == nullptr) {
            return evision::nif::error(env, "IPC handle is nullptr");
        }

        evision_res<Ptr<cv::cuda::GpuMat>> * self = nullptr;
        if (alloc_resource(&self)) {
            new (&(self->val)) Ptr<cv::cuda::GpuMat>(); // init Ptr with placement new
        }
        if (self) ERRWRAP2(self->val.reset(new cv::cuda::GpuMat(height, width, type, ptr)), env, error_flag, error_term);
        if (!error_flag) {
            ERL_NIF_TERM ret = enif_make_resource(env, self);
            enif_release_resource(self);
            // if (pointer_kind == "host_ipc") {
            //     // Close the IPC handle
            //     self->on_delete_callback = [fd, memname, ptr, device_size]() {
            //         close_ipc_handle(fd, ptr, (char*)memname.c_str(), device_size);
            //     };
            // }
            bool success;
            return evision_from_as_map<Ptr<cv::cuda::GpuMat>>(env, self->val, ret, "Elixir.Evision.CUDA.GpuMat", success);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

#endif
