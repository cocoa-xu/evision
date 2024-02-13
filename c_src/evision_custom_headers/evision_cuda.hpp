#ifdef HAVE_OPENCV_CORE

#include "opencv2/core/cuda.hpp"

typedef std::vector<cuda::GpuMat> vector_GpuMat;
typedef cuda::GpuMat::Allocator GpuMat_Allocator;
typedef cuda::HostMem::AllocType HostMem_AllocType;
typedef cuda::Event::CreateFlags Event_CreateFlags;
typedef cuda::GpuMat cuda_GpuMat;
typedef cuda::Stream cuda_Stream;
typedef cuda::Event cuda_Event;
typedef cuda::HostMem cuda_HostMem;

template<> struct evisionVecConverter<cuda::GpuMat>
{
    static bool to(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<cuda::GpuMat>& value, const ArgInfo& info)
    {
        return evision_to_generic_vec(env, obj, value, info);
    }

    static ERL_NIF_TERM from(ErlNifEnv *env, const std::vector<cuda::GpuMat>& value)
    {
        return evision_from_generic_vec(env, value);
    }
};

CV_ERL_TO_CLASS(cuda::GpuMat);
CV_ERL_TO_CLASS(cuda::Stream);
CV_ERL_TO_CLASS(cuda::Event);
CV_ERL_TO_CLASS(cuda::HostMem);

CV_ERL_TO_CLASS_PTR(cuda::GpuMat);
CV_ERL_TO_CLASS_PTR(cuda::GpuMat::Allocator);

CV_ERL_FROM_CLASS(cuda::GpuMat);
CV_ERL_FROM_CLASS(cuda::Stream);
CV_ERL_FROM_CLASS(cuda::HostMem);

CV_ERL_FROM_CLASS_PTR(cuda::GpuMat::Allocator);

#endif
