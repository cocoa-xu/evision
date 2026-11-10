#ifndef EVISION_MAT_ALLOCATOR_HPP
#define EVISION_MAT_ALLOCATOR_HPP
#pragma once

#include <erl_nif.h>
#include <cstring>
#include <mutex>
#include <new>
#include "opencv2/core.hpp"
#include "evision_consts.h"

// Mat data of at least this many bytes is allocated as an ERTS binary.
static const size_t kEvisionBinaryMatMinBytes = 4096;
static const size_t kEvisionMatAlign = 64;

// A Mat allocation backed by an ERTS refc binary. The BEAM only counts binary
// sizes towards garbage collection, so returning this binary along with a Mat
// lets the GC see how much memory the Mat holds.
struct EvisionBinaryBlock {
    ErlNifBinary bin;
    // Created when the Mat is first returned to Erlang. It then holds the C++
    // side's reference to the binary, and `bin` must no longer be released.
    ErlNifEnv *env = nullptr;
    ERL_NIF_TERM term = 0;
    std::mutex mutex;
};

class EvisionBinaryMatAllocator final : public cv::MatAllocator {
public:
    cv::UMatData* allocate(int dims, const int* sizes, int type, void* data0, size_t* step,
                           cv::AccessFlag, cv::UMatUsageFlags) const override {
        // Same layout as OpenCV's StdMatAllocator.
        size_t total = CV_ELEM_SIZE(type);
        for (int i = dims - 1; i >= 0; i--) {
            if (step) {
                if (data0 && step[i] != CV_AUTOSTEP) {
                    CV_Assert(total <= step[i]);
                    total = step[i];
                } else {
                    step[i] = total;
                }
            }
            CV_Assert(sizes[i] >= 0 && (sizes[i] == 0 || total <= SIZE_MAX / (size_t)sizes[i]));
            total *= sizes[i];
        }

        if (data0) {
            cv::UMatData* u = new cv::UMatData(this);
            u->data = u->origdata = (uchar*)data0;
            u->size = total;
            u->flags |= cv::UMatData::USER_ALLOCATED;
            return u;
        }

        if (total < kEvisionBinaryMatMinBytes) {
            uchar* data = (uchar*)cv::fastMalloc(total);
            cv::UMatData* u = new (std::nothrow) cv::UMatData(this);
            if (!u) {
                cv::fastFree(data);
                CV_Error(cv::Error::StsNoMem, "Failed to allocate UMatData");
            }
            u->data = u->origdata = data;
            u->size = total;
            return u;
        }

        EvisionBinaryBlock* block = new (std::nothrow) EvisionBinaryBlock();
        if (!block || total > SIZE_MAX - kEvisionMatAlign ||
            !enif_alloc_binary(total + kEvisionMatAlign, &block->bin)) {
            delete block;
            CV_Error_(cv::Error::StsNoMem, ("Failed to allocate %zu bytes", total));
        }
        cv::UMatData* u = new (std::nothrow) cv::UMatData(this);
        if (!u) {
            enif_release_binary(&block->bin);
            delete block;
            CV_Error(cv::Error::StsNoMem, "Failed to allocate UMatData");
        }
        // OpenCL treats origdata as the start of the host buffer, so both point at the aligned data.
        uchar* data = cv::alignPtr(block->bin.data, (int)kEvisionMatAlign);
        // The whole binary becomes visible to Erlang, so the padding must not expose stale memory.
        memset(block->bin.data, 0, data - block->bin.data);
        memset(data + total, 0, block->bin.data + block->bin.size - (data + total));
        u->data = u->origdata = data;
        u->size = total;
        u->userdata = block;
        return u;
    }

    bool allocate(cv::UMatData* u, cv::AccessFlag, cv::UMatUsageFlags) const override {
        return u != nullptr;
    }

    void deallocate(cv::UMatData* u) const override {
        if (!u) return;
        CV_Assert(u->urefcount == 0);
        CV_Assert(u->refcount == 0);
        if (!(u->flags & cv::UMatData::USER_ALLOCATED)) {
            if (EvisionBinaryBlock* block = (EvisionBinaryBlock*)u->userdata) {
                if (block->env) {
                    enif_free_env(block->env);
                } else {
                    enif_release_binary(&block->bin);
                }
                delete block;
                u->userdata = nullptr;
            } else {
                cv::fastFree(u->origdata);
            }
            u->origdata = nullptr;
        }
        delete u;
    }
};

static EvisionBinaryMatAllocator& evision_binary_mat_allocator() {
    static EvisionBinaryMatAllocator allocator;
    return allocator;
}

// The binary behind `m`'s data, or nil when its data is not an ERTS binary.
static ERL_NIF_TERM evision_mat_ref_data(ErlNifEnv *env, const cv::Mat& m) {
    cv::UMatData* u = m.u;
    if (!u || u->currAllocator != &evision_binary_mat_allocator() || !u->userdata) {
        return kAtomNil;
    }

    EvisionBinaryBlock* block = (EvisionBinaryBlock*)u->userdata;
    std::lock_guard<std::mutex> lock(block->mutex);
    if (!block->env) {
        ErlNifEnv *keep = enif_alloc_env();
        if (!keep) return kAtomNil;
        block->term = enif_make_binary(keep, &block->bin);
        block->env = keep;
    }
    return enif_make_copy(env, block->term);
}

#endif  // EVISION_MAT_ALLOCATOR_HPP
