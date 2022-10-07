/*M///////////////////////////////////////////////////////////////////////////////////////
//
//  IMPORTANT: READ BEFORE DOWNLOADING, COPYING, INSTALLING OR USING.
//
//  By downloading, copying, installing or using the software you agree to this license.
//  If you do not agree to this license, do not download, install,
//  copy or use the software.
//
//
//                           License Agreement
//                For Open Source Computer Vision Library
//
// Copyright (C) 2000-2008, Intel Corporation, all rights reserved.
// Copyright (C) 2009-2011, Willow Garage Inc., all rights reserved.
// Third party copyrights are property of their respective owners.
//
// Redistribution and use in source and binary forms, with or without modification,
// are permitted provided that the following conditions are met:
//
//   * Redistribution's of source code must retain the above copyright notice,
//     this list of conditions and the following disclaimer.
//
//   * Redistribution's in binary form must reproduce the above copyright notice,
//     this list of conditions and the following disclaimer in the documentation
//     and/or other materials provided with the distribution.
//
//   * The name of the copyright holders may not be used to endorse or promote products
//     derived from this software without specific prior written permission.
//
// This software is provided by the copyright holders and contributors "as is" and
// any express or implied warranties, including, but not limited to, the implied
// warranties of merchantability and fitness for a particular purpose are disclaimed.
// In no event shall the Intel Corporation or contributors be liable for any direct,
// indirect, incidental, special, exemplary, or consequential damages
// (including, but not limited to, procurement of substitute goods or services;
// loss of use, data, or profits; or business interruption) however caused
// and on any theory of liability, whether in contract, strict liability,
// or tort (including negligence or otherwise) arising in any way out of
// the use of this software, even if advised of the possibility of such damage.
//
//M*/

// Defines for Erlang compatibility.
#ifndef __ERLCOMPAT_HPP__
#define __ERLCOMPAT_HPP__

#include "nif_utils.hpp"

//==================================================================================================

#define CV_ERL_TO_CLASS(TYPE)                                                                          \
template<>                                                                                            \
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM dst, TYPE& src, const ArgInfo& info)                     \
{                                                                                                     \
    Ptr<TYPE> ptr;                                                                                    \
    if (!evision_to(env, dst, ptr, info)) return false;                                               \
    src = *ptr;                                                                                       \
    return true;                                                                                      \
}

#define CV_ERL_FROM_CLASS(TYPE)                                                                        \
template<>                                                                                            \
ERL_NIF_TERM evision_from(ErlNifEnv *env, const TYPE& src)                                            \
{                                                                                                     \
    Ptr<TYPE> ptr(new TYPE());                                                                        \
                                                                                                      \
    *ptr = src;                                                                                       \
    return evision_from(env, ptr);                                                                    \
}

#define CV_ERL_TO_CLASS_PTR(TYPE)                                                                      \
template<>                                                                                            \
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM dst, TYPE*& src, const ArgInfo& info)                    \
{                                                                                                     \
    Ptr<TYPE> ptr;                                                                                    \
                                                                                                      \
    if (!evision_to(env, dst, ptr, info)) return false;                                               \
    src = ptr;                                                                                        \
    return true;                                                                                      \
}

#define CV_ERL_FROM_CLASS_PTR(TYPE)                                                                    \
static ERL_NIF_TERM evision_from(ErlNifEnv *env, TYPE*& src)                                          \
{                                                                                                     \
    return evision_from(env, Ptr<TYPE>(src));                                                         \
}

#define CV_ERL_TO_ENUM(TYPE)                                                                          \
template<>                                                                                            \
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM dst, TYPE& src, const ArgInfo& info)                     \
{                                                                                                     \
    int i32;                                                                                          \
    if (!evision::nif::get(env, dst, &i32)) {                                                         \
        src = static_cast<TYPE>(i32);                                                                 \
        return true;                                                                                  \
    } else {                                                                                          \
        return false;                                                                                 \
    }                                                                                                 \
}

#define CV_ERL_FROM_ENUM(TYPE)                                                                        \
template<>                                                                                            \
ERL_NIF_TERM evision_from(ErlNifEnv *env, const TYPE& src)                                            \
{                                                                                                     \
    return evision::nif::make(env, (int)src);                                                         \
}

//==================================================================================================

#define CV_ERL_TYPE_DECLARE_DYNAMIC(WNAME, NAME, STORAGE, SNAME) \
    static bool evision_##NAME##_getp(ErlNifEnv *env, ERL_NIF_TERM self, STORAGE * & dst)             \
    {                                                                                                 \
        evision_res<STORAGE> * VAR;                                                                   \
        if (enif_get_resource(env, self, evision_res<STORAGE>::type, (void **)&VAR)) {                \
            dst = &(VAR->val);                                                                        \
            return true;                                                                              \
        }                                                                                             \
        return false;                                                                                 \
    }                                                                                                 \
    static ERL_NIF_TERM evision_##NAME##_Instance(ErlNifEnv *env, const STORAGE &r)                   \
    {                                                                                                 \
        evision_res<STORAGE> * VAR;                                                                   \
        VAR = (decltype(VAR))enif_alloc_resource(evision_res<STORAGE>::type,                          \
                                sizeof(evision_res<STORAGE>));                                        \
        if (!VAR) return evision::nif::error(env, "no memory");                                       \
        new (&(VAR->val)) STORAGE(r);                                                                 \
        ERL_NIF_TERM ret = enif_make_resource(env, VAR);                                              \
        enif_release_resource(VAR);                                                                   \
        return ret;                                                                                   \
    }                                                                                                 \
    static void destruct_##NAME(ErlNifEnv *env, void *args)                                           \
    {                                                                                                 \
        ((evision_res< STORAGE > *)args)->val.STORAGE::~SNAME();                                      \
    }

#define CV_ERL_TYPE_INIT_DYNAMIC(WNAME, NAME, STORAGE, ERROR_HANDLER)                                 \
    {                                                                                                 \
        rt = enif_open_resource_type(env, "evision", "erl_cv_" #NAME "_type", destruct_##NAME ,     \
                ERL_NIF_RT_CREATE, NULL);                                                             \
        if (!rt) ERROR_HANDLER;                                                                       \
        evision_res<STORAGE>::type = rt;                                                              \
    }

#endif // END HEADER GUARD
