#include "evision.hpp"
#include "evision_generated_nif_decls.h"

#define F(ERL_NAME, C_NAME, ARITY)    \
  {#ERL_NAME, ARITY, evision_nif_dispatch<C_NAME>, 0}
#define F_CPU(ERL_NAME, C_NAME, ARITY)    \
  {#ERL_NAME, ARITY, evision_nif_dispatch<C_NAME>, ERL_NIF_DIRTY_JOB_CPU_BOUND}
#define F_IO(ERL_NAME, C_NAME, ARITY)    \
  {#ERL_NAME, ARITY, evision_nif_dispatch<C_NAME>, ERL_NIF_DIRTY_JOB_IO_BOUND}


/************************************************************************/

// manually coded modules
#include "modules/evision_mat.h"
#include "modules/evision_gpumat.h"
#include "modules/evision_highgui.h"
#include "modules/evision_imdecode.h"
#include "modules/evision_backend/backend.h"
#include "modules/evision_videocapture.h"
// #include "modules/evision_dnn.h"

/************************************************************************/

// No C++ exception may cross the Erlang NIF ABI, including exceptions from
// hand-written constructors and allocations outside generated ERRWRAP2 calls.
template <ERL_NIF_TERM (*Function)(ErlNifEnv*, int, const ERL_NIF_TERM*)>
static ERL_NIF_TERM evision_nif_dispatch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    try {
        return Function(env, argc, argv);
    } catch (const cv::Exception& e) {
        return evision::nif::error(env, e.what());
    } catch (const std::exception& e) {
        return evision::nif::error(env, e.what());
    } catch (...) {
        return evision::nif::error(env, "native operation failed");
    }
}

#include "evision_generated_modules_content.h"

static void destruct_Mat(ErlNifEnv *env, void *args) {
    evision_res<cv::Mat *> * res = (evision_res<cv::Mat *> *)args;
    if (res->val) {
        delete res->val;
        res->val = nullptr;
    }

    // unref input if we no longer need it
    if (res->in_buf != nullptr) {
        (res->in_unref)(res->in_buf, res->in_ref);
        res->in_ref = nullptr;
        res->in_buf = nullptr;
    }
}

static int
on_load(ErlNifEnv* env, void**, ERL_NIF_TERM)
{
    ErlNifResourceType *rt;

#define CV_ERL_TYPE(WNAME, NAME, STORAGE, _1, BASE, CONSTRUCTOR, _2) CV_ERL_TYPE_INIT_DYNAMIC(WNAME, NAME, STORAGE, return -1)
#include "evision_generated_types.h"
#undef CV_ERL_TYPE
    rt = enif_open_resource_type(env, "evision", "Evision.Mat.t", destruct_Mat, ERL_NIF_RT_CREATE, NULL);
    if (!rt) return -1;
    evision_res<cv::Mat *>::type = rt;

    kAtomNil = evision::nif::atom(env, "nil");
    kAtomTrue = evision::nif::atom(env, "true");
    kAtomFalse = evision::nif::atom(env, "false");
    kAtomOutOfMemory = evision::nif::atom(env, "out_of_memory");
    kAtomRef = evision::nif::atom(env, "ref");
    kAtomRefData = evision::nif::atom(env, "ref_data");
    kAtomClass = evision::nif::atom(env, "class");
    kAtomDims = evision::nif::atom(env, "dims");
    kAtomChannels = evision::nif::atom(env, "channels");
    kAtomType = evision::nif::atom(env, "type");
    kAtomData = evision::nif::atom(env, "data");
    kAtomRawType = evision::nif::atom(env, "raw_type");
    kAtomElemSize = evision::nif::atom(env, "elemSize");
    kAtomStep = evision::nif::atom(env, "step");
    kAtomDeviceID = evision::nif::atom(env, "device_id");
    kAtomShape = evision::nif::atom(env, "shape");
    kAtomStructKey = evision::nif::atom(env, "__struct__");
    kAtomNxTensorModule = evision::nif::atom(env, "Elixir.Nx.Tensor");
    kAtomNxTensor = evision::nif::atom(env, "nx_tensor");
    kAtomEvisionMatModule = evision::nif::atom(env, "Elixir.Evision.Mat");

    kAtomU = evision::nif::atom(env, "u");
    kAtomS = evision::nif::atom(env, "s");
    kAtomF = evision::nif::atom(env, "f");
    kAtomU8 = evision::nif::atom(env, "u8");
    kAtomS8 = evision::nif::atom(env, "s8");
    kAtomU16 = evision::nif::atom(env, "u16");
    kAtomS16 = evision::nif::atom(env, "s16");
    kAtomU32 = evision::nif::atom(env, "u32");
    kAtomS32 = evision::nif::atom(env, "s32");
    kAtomU64 = evision::nif::atom(env, "u64");
    kAtomS64 = evision::nif::atom(env, "s64");
    kAtomF16 = evision::nif::atom(env, "f16");
    kAtomBF = evision::nif::atom(env, "bf");
    kAtomBF16 = evision::nif::atom(env, "bf16");
    kAtomF8E4M3FN = evision::nif::atom(env, "f8_e4m3fn");
    kAtomF32 = evision::nif::atom(env, "f32");
    kAtomF64 = evision::nif::atom(env, "f64");
    kAtomUser = evision::nif::atom(env, "user");

    kAtomM00 = evision::nif::atom(env, "m00");
    kAtomM01 = evision::nif::atom(env, "m01");
    kAtomM10 = evision::nif::atom(env, "m10");
    kAtomM20 = evision::nif::atom(env, "m20");
    kAtomM11 = evision::nif::atom(env, "m11");
    kAtomM02 = evision::nif::atom(env, "m02");
    kAtomM30 = evision::nif::atom(env, "m30");
    kAtomM21 = evision::nif::atom(env, "m21");
    kAtomM12 = evision::nif::atom(env, "m12");
    kAtomM03 = evision::nif::atom(env, "m03");
    kAtomMu20 = evision::nif::atom(env, "mu20");
    kAtomMu11 = evision::nif::atom(env, "mu11");
    kAtomMu02 = evision::nif::atom(env, "mu02");
    kAtomMu30 = evision::nif::atom(env, "mu30");
    kAtomMu21 = evision::nif::atom(env, "mu21");
    kAtomMu12 = evision::nif::atom(env, "mu12");
    kAtomMu03 = evision::nif::atom(env, "mu03");
    kAtomNu20 = evision::nif::atom(env, "nu20");
    kAtomNu11 = evision::nif::atom(env, "nu11");
    kAtomNu02 = evision::nif::atom(env, "nu02");
    kAtomNu30 = evision::nif::atom(env, "nu30");
    kAtomNu21 = evision::nif::atom(env, "nu21");
    kAtomNu12 = evision::nif::atom(env, "nu12");
    kAtomNu03 = evision::nif::atom(env, "nu03");

    cv::Mat::setDefaultAllocator(&evision_binary_mat_allocator());
    return 0;
}

static int on_reload(ErlNifEnv*, void**, ERL_NIF_TERM)
{
    return 0;
}

static int on_upgrade(ErlNifEnv*, void**, void**, ERL_NIF_TERM)
{
    return 0;
}

ERL_NIF_INIT(evision_nif, nif_functions, on_load, on_reload, on_upgrade, NULL);

#if defined(__GNUC__)
#pragma GCC visibility push(default)
#endif
