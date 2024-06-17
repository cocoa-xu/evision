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
            std::vector<unsigned char> pointer_vec;
            std::uintptr_t ptr = (std::uintptr_t)_self_->cudaPtr();
            if (pointer_kind == "local") {
                unsigned char* bytePtr = reinterpret_cast<unsigned char*>(&ptr);
                for (size_t i = 0; i < sizeof(void*); i++) {
                    pointer_vec.push_back(bytePtr[i]);
                }
            }
            else if (pointer_kind == "cuda_ipc") {
                auto result = get_cuda_ipc_handle(ptr);
                if (result.second) {
                    return evision::nif::error(env, "Unable to get cuda IPC handle");
                }
                pointer_vec = result.first;
            }
            if (pointer_vec.size() == 0) {
                return enif_make_badarg(env);
            }
            std::vector<ERL_NIF_TERM> handle_list(pointer_vec.size());
            for (uint64_t i = 0; i < pointer_vec.size(); i++) {
                handle_list[i] = enif_make_uint(env, pointer_vec[i]);
            }

            ERL_NIF_TERM handle_list_term = enif_make_list_from_array(env, handle_list.data(), pointer_vec.size());
            return evision::nif::ok(env, handle_list_term);
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
        std::vector<int64_t> device_pointer;
        std::vector<int64_t> shape;
        std::string dtype;

        if (!evision_to_safe(env, evision_get_kw(env, erl_terms, "device_pointer"), device_pointer, ArgInfo("device_pointer", 1))) {
            return enif_make_badarg(env);
        }
        if (!evision_to_safe(env, evision_get_kw(env, erl_terms, "dtype"), dtype, ArgInfo("dtype", 1))) {
            return enif_make_badarg(env);
        }
        if (!evision::nif::get_tuple(env, evision_get_kw(env, erl_terms, "shape"), shape)) {
            return enif_make_badarg(env);
        }
        if (shape.size() > 3 || shape.size() == 0) {
            return evision::nif::error(env, "GpuMat expects shape to be 1 <= tuple_size(shape) <= 3");
        }

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

        void* ptr = nullptr;
        if (device_pointer.size() == sizeof(void*)) {
            unsigned char* bytePtr = reinterpret_cast<unsigned char*>(&ptr);
            for (size_t i = 0; i < sizeof(void*); i++) {
                bytePtr[i] = device_pointer[i];
            }
        } else {
            auto result = get_pointer_for_ipc_handle(pointer_vec);
            if (result.second) {
                return exla::nif::error(env, "Unable to get pointer for IPC handle.");
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
