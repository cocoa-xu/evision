#ifndef EVISION_OPENCV_GPUMAT_H
#define EVISION_OPENCV_GPUMAT_H

#include <erl_nif.h>
#include "evision_cuda.h"
#include "../nif_utils.hpp"

// @evision c: cuda_cuda_GpuMat_to_pointer,evision_cv_cuda_cuda_GpuMat_to_pointer,1
// @evision nif: def cuda_cuda_GpuMat_to_pointer(_opts \\ []), do: :erlang.nif_error(:undefinedined)
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

#endif
