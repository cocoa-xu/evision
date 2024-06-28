#ifndef EVISION_OPENCV_GPUMAT_H
#define EVISION_OPENCV_GPUMAT_H

#include <erl_nif.h>
#include "evision_cuda.h"
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
            if (_self_->isContinuous() == false) {
                return evision::nif::error(env, "GpuMat must be continuous");
            }

            ERL_NIF_TERM out_term{};
            std::uintptr_t ptr = (std::uintptr_t)_self_->cudaPtr();
            if (pointer_kind == "local") {
                ERL_NIF_TERM ptr_term = enif_make_ulong(env, ptr);
            }
            else if (pointer_kind == "cuda_ipc") {
                auto result = get_cuda_ipc_handle(ptr);
                if (result.second) {
                    return evision::nif::error(env, "Unable to get cuda IPC handle");
                }
                auto pointer_vec = result.first;

                ErlNifBinary handle_bin;
                enif_alloc_binary(pointer_vec.size(), &handle_bin);
                for (int i = 0; i < pointer_vec.size(); i++) {
                    handle_bin.data[i] = pointer_vec[i];
                }
                out_term = enif_make_binary(env, &handle_bin);
            }
            else {
                return evision::nif::error(env, "mode must be either 'local', 'cuda_ipc' or 'host_ipc'");
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
        int pointer_kind = -1; 
        void* ptr = nullptr;
        unsigned long local_pointer = 0;
        ErlNifBinary cuda_ipc_handle{};

        std::vector<int64_t> shape;
        std::string dtype;
        int device_id = 0;

        ERL_NIF_TERM device_pointer = evision_get_kw(env, erl_terms, "device_pointer");
        if (enif_get_ulong(env, device_pointer, &local_pointer)) {
            pointer_kind = 0;
            ptr = reinterpret_cast<void*>(local_pointer);
        } else if (enif_inspect_binary(env, device_pointer, &cuda_ipc_handle)) {
            pointer_kind = 1;
        } else {
            return evision::nif::error(env, "device_pointer must be an integer or a binary");
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

        if (pointer_kind == 1) {
            auto result = get_pointer_for_ipc_handle(cuda_ipc_handle.data, cuda_ipc_handle.size, device_id);
            if (result.second) {
                return evision::nif::error(env, "Unable to get pointer for IPC handle.");
            }
            ptr = result.first;
        }
        if (ptr == nullptr) {
            return evision::nif::error(env, "device_pointer is nullptr");
        }

        evision_res<Ptr<cv::cuda::GpuMat>> * self = nullptr;
        if (alloc_resource(&self)) {
            new (&(self->val)) Ptr<cv::cuda::GpuMat>(); // init Ptr with placement new
        }
        if (self) ERRWRAP2(self->val.reset(new cv::cuda::GpuMat(height, width, type, ptr)), env, error_flag, error_term);
        if (!error_flag) {
            ERL_NIF_TERM ret = enif_make_resource(env, self);
            enif_release_resource(self);
            bool success;
            return evision_from_as_map<Ptr<cv::cuda::GpuMat>>(env, self->val, ret, "Elixir.Evision.CUDA.GpuMat", success);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

#endif
