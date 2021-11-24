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

#if 1

// Python3 treats all ints as longs, PyInt_X functions have been removed.
#define PyInt_Check PyLong_Check
#define PyInt_CheckExact PyLong_CheckExact
#define PyInt_AsLong PyLong_AsLong
#define PyInt_AS_LONG PyLong_AS_LONG
#define PyInt_FromLong PyLong_FromLong
#define PyNumber_Int PyNumber_Long


#define PyString_FromString PyUnicode_FromString
#define PyString_FromStringAndSize PyUnicode_FromStringAndSize

#endif // PY_MAJOR >=3

#include "nif_utils.hpp"

ERL_NIF_TERM make_atom(ErlNifEnv*, const char*);
ERL_NIF_TERM make_ok_tuple(ErlNifEnv*, ERL_NIF_TERM);
ERL_NIF_TERM make_error_tuple(ErlNifEnv*, const char*);
ERL_NIF_TERM make_binary(ErlNifEnv*, const void*, unsigned int);

ERL_NIF_TERM make_atom(ErlNifEnv *env, const char *atom_name)
{
    ERL_NIF_TERM atom;

    if(enif_make_existing_atom(env, atom_name, &atom, ERL_NIF_LATIN1))
	   return atom;

    return enif_make_atom(env, atom_name);
}

ERL_NIF_TERM make_ok_tuple(ErlNifEnv *env, ERL_NIF_TERM value)
{
    return enif_make_tuple2(env, make_atom(env, "ok"), value);
}

ERL_NIF_TERM make_error_tuple(ErlNifEnv *env, const char *reason)
{
    return enif_make_tuple2(env, make_atom(env, "error"), make_atom(env, reason));
}

ERL_NIF_TERM make_binary(ErlNifEnv *env, const void *bytes, unsigned int size)
{
    ErlNifBinary blob;
    ERL_NIF_TERM term;

    if(!enif_alloc_binary(size, &blob)) {
	    /* TODO: fix this */
	    return make_atom(env, "error");
    }

    memcpy(blob.data, bytes, size);
    term = enif_make_binary(env, &blob);
    enif_release_binary(&blob);

    return term;
}

//==================================================================================================

#define CV_PY_FN_WITH_KW_(fn, flags) (PyCFunction)(void*)(PyCFunctionWithKeywords)(fn), (flags) | METH_VARARGS | METH_KEYWORDS
#define CV_PY_FN_NOARGS_(fn, flags) (PyCFunction)(fn), (flags) | METH_NOARGS

#define CV_PY_FN_WITH_KW(fn) CV_PY_FN_WITH_KW_(fn, 0)
#define CV_PY_FN_NOARGS(fn) CV_PY_FN_NOARGS_(fn, 0)

#define CV_PY_TO_CLASS(TYPE)                                                                          \
template<>                                                                                            \
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM dst, TYPE& src, const ArgInfo& info)                                       \
{                                                                                                     \
    Ptr<TYPE> ptr;                                                                                    \
    if (!evision_to(env, dst, ptr, info)) return false;                                               \
    src = *ptr;                                                                                       \
    return true;                                                                                      \
}

#define CV_PY_FROM_CLASS(TYPE)                                                                        \
template<>                                                                                            \
ERL_NIF_TERM evision_from(ErlNifEnv *env, const TYPE& src)                                                              \
{                                                                                                     \
    Ptr<TYPE> ptr(new TYPE());                                                                        \
                                                                                                      \
    *ptr = src;                                                                                       \
    return evision_from(env, ptr);                                                                        \
}

#define CV_PY_TO_CLASS_PTR(TYPE)                                                                      \
template<>                                                                                            \
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM dst, TYPE*& src, const ArgInfo& info)                                      \
{                                                                                                     \
    Ptr<TYPE> ptr;                                                                                    \
                                                                                                      \
    if (!evision_to(env, dst, ptr, info)) return false;                                                   \
    src = ptr;                                                                                        \
    return true;                                                                                      \
}

#define CV_PY_FROM_CLASS_PTR(TYPE)                                                                    \
static ERL_NIF_TERM evision_from(ErlNifEnv *env, TYPE*& src)                                                            \
{                                                                                                     \
    return evision_from(env, Ptr<TYPE>(src));                                                             \
}

#define CV_ERL_TO_ENUM(TYPE)                                                                          \
template<>                                                                                            \
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM dst, TYPE& src, const ArgInfo& info)                     \
{                                                                                                     \
    std::string str_enum;                                                                             \
    if (!evision::nif::get(env, dst, str_enum)) {                                                     \
        std::cout << "str_enum: " << str_enum << '\n';                                                \
        return true;                                                                                  \
    } else {                                                                                          \
        return false;                                                                                 \
    }                                                                                                 \
}

#define CV_ERL_FROM_ENUM(TYPE)                                                                        \
template<>                                                                                            \
ERL_NIF_TERM evision_from(ErlNifEnv *env, const TYPE& src)                                            \
{                                                                                                     \
    return make_atom(env, "pure_pain_peko");                                                          \
}

//==================================================================================================

#define CVPY_TYPE_DECLARE_DYNAMIC(WNAME, NAME, STORAGE, SNAME) \
    static bool evision_##NAME##_getp(ErlNifEnv *env, ERL_NIF_TERM self, STORAGE * & dst) \
    {                                                                                     \
        evision_res<STORAGE> * VAR; \
        if (!enif_get_resource(env, self, evision_res<STORAGE>::type, (void **)&VAR)) \
            return enif_make_badarg(env);                  \
        else \
        { \
            dst = &(VAR->val); \
            return true; \
        } \
        return false; \
    } \
    static ERL_NIF_TERM evision_##NAME##_Instance(ErlNifEnv *env, const STORAGE &r) \
    {                                                  \
        evision_res< STORAGE > * VAR; \
        VAR = (decltype(VAR))enif_alloc_resource(evision_res< STORAGE >::type, sizeof(evision_res< STORAGE >::type)); \
        if (!VAR) \
            return make_error_tuple(env, "no memory");     \
        new (&(VAR->val)) STORAGE(r);                  \
        ERL_NIF_TERM ret = enif_make_resource(env, VAR); \
        enif_release_resource(VAR); \
        return ret; \
    } \
    static void destruct_##NAME(ErlNifEnv *env, void *args)    \
    {                                                          \
        ((evision_res< STORAGE > *)args)->val.STORAGE::~SNAME(); \
    }

#define CVPY_TYPE_INIT_DYNAMIC(WNAME, NAME, STORAGE, ERROR_HANDLER) \
    { \
        rt = enif_open_resource_type(env, "erl_cv_nif", "erl_cv_##NAME##_type", destruct_##NAME , ERL_NIF_RT_CREATE, NULL); \
        if (!rt) ERROR_HANDLER; \
        evision_res<STORAGE>::type = rt; \
    }

// Debug module load:
//
// else \
// { \
//     printf("Init: " #NAME ", base (" #BASE ") -> %p" "\n", pyopencv_##NAME##_TypePtr); \
// }


#endif // END HEADER GUARD
