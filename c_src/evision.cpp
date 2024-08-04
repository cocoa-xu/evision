//warning number '5033' not a valid compiler warning in vc12
#if defined(_MSC_VER) && (_MSC_VER > 1800)
// eliminating duplicated round() declaration
#define HAVE_ROUND 1
#pragma warning(push)
#pragma warning(disable:5033)  // 'register' is no longer a supported storage class
#endif

#include <cmath>
#include <erl_nif.h>
#include <limits>
#include <functional>

#if defined(_MSC_VER) && (_MSC_VER > 1800)
#pragma warning(pop)
#endif

#ifdef __GNUC__
#  pragma GCC diagnostic ignored "-Wunused-parameter"
#  pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#  pragma GCC diagnostic ignored "-Wunused-variable"
#  pragma GCC diagnostic ignored "-Wunused-function"
#endif

#define F(ERL_NAME, C_NAME, ARITY)    \
  {#ERL_NAME, ARITY, C_NAME, 0}
#define F_CPU(ERL_NAME, C_NAME, ARITY)    \
  {#ERL_NAME, ARITY, C_NAME, ERL_NIF_DIRTY_JOB_CPU_BOUND}
#define F_IO(ERL_NAME, C_NAME, ARITY)    \
  {#ERL_NAME, ARITY, C_NAME, ERL_NIF_DIRTY_JOB_IO_BOUND}

#include "opencv2/opencv_modules.hpp"
#include "opencv2/core.hpp"
#include "configuration.private.hpp"
#include "opencv2/core/utils/logger.hpp"
#include "opencv2/core/utils/tls.hpp"

#include "evision_generated_include.h"
#include "opencv2/core/types_c.h"
#include "erlcompat.hpp"
#include "ArgInfo.hpp"
#include "evision_consts.h"
#include "modules/evision_cuda.h"
#include "modules/evision_mat_api.h"
#include <map>
#include <optional>

#include <type_traits>  // std::enable_if

using namespace cv;

template<typename R>
struct evision_res {
    R val;
    static ErlNifResourceType * type;
};
template<typename R> ErlNifResourceType * evision_res<R>::type = nullptr;

template<typename R>
int alloc_resource(evision_res<R> **res) {
    *res = (evision_res<R> *)enif_alloc_resource(evision_res<R>::type, sizeof(evision_res<R>));
    return (*res != nullptr);
}

template<>
struct evision_res<cv::Mat *> {
    cv::Mat * val;

    // https://github.com/akash-akya/zero_copy/blob/master/c_src/zero_copy.c
    // pointer to input data
    unsigned char *in_buf;
    // input data specific opaque obj, this will be passed during unref
    void *in_ref = nullptr;
    // function to be called to unref input data
    void (*in_unref)(void *, void *) = nullptr;

    static ErlNifResourceType * type;
};
ErlNifResourceType * evision_res<cv::Mat *>::type = nullptr;

template<>
int alloc_resource(evision_res<cv::Mat *> **res) {
    evision_res<cv::Mat *> * tmp = (evision_res<cv::Mat *> *)enif_alloc_resource(evision_res<cv::Mat *>::type, sizeof(evision_res<cv::Mat *>));

    if (tmp != nullptr) {
        tmp->in_buf = nullptr;
        tmp->in_ref = nullptr;
        tmp->in_unref = nullptr;
        *res = tmp;

        // 1: ok
        return 1;
    }

    // 0: failed
    return 0;
}

static bool isBindingsDebugEnabled()
{
    static bool param_debug = cv::utils::getConfigurationParameterBool("OPENCV_EVISION_DEBUG", false);
    return param_debug;
}

static void emit_failmsg(ErlNifEnv *env, const char * type, const char *msg)
{
    static bool param_debug = isBindingsDebugEnabled();
    if (param_debug)
    {
        fprintf(stderr, "error: %s, msg: %s\r\n", type, msg);
    }
}

static int failmsg(ErlNifEnv *env, const char *fmt, ...)
{
    char str[1000];

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(str, sizeof(str), fmt, ap);
    va_end(ap);

    emit_failmsg(env, "TypeError", str);
    return 0;
}

static ERL_NIF_TERM failmsgp(ErlNifEnv *env, const char *fmt, ...)
{
    char str[1000];

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(str, sizeof(str), fmt, ap);
    va_end(ap);

    emit_failmsg(env, "TypeError", str);
    return evision::nif::error(env, str);
}

#define CV_HAS_CONVERSION_ERROR(x) (((x) == -1))

template<typename T, class TEnable = void>  // TEnable is used for SFINAE checks
struct Evision_Converter
{
    static inline bool to(ErlNifEnv *env, ERL_NIF_TERM obj, T& p, const ArgInfo& info);
    static inline ERL_NIF_TERM from(ErlNifEnv *env, const T& src);
    static inline ERL_NIF_TERM from_as_binary(ErlNifEnv *env, const T& src, bool& success);
    static inline ERL_NIF_TERM from_as_map(ErlNifEnv *env, T src, ERL_NIF_TERM res_term);
};

// exception-safe evision_to
template<typename _Tp> static
bool evision_to_safe(ErlNifEnv *env, ERL_NIF_TERM obj, _Tp& value, const ArgInfo& info)
{
    try
    {
        return evision_to(env, obj, value, info);
    }
    catch (const std::exception &e)
    {
        failmsgp(env, cv::format("Conversion error: %s, what: %s", info.name, e.what()).c_str());
        return false;
    }
    catch (...)
    {
        failmsgp(env, cv::format("Conversion error: %s", info.name).c_str());
        return false;
    }
}

static inline
ERL_NIF_TERM evision_get_kw(ErlNifEnv *env, const std::map<std::string, ERL_NIF_TERM>& erl_terms, const std::string& key) {
    auto iter = erl_terms.find(key);
    if (iter == erl_terms.end()) {
        return kAtomNil;
    }
    return iter->second;
}

template<typename T> static
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, T& p, const ArgInfo& info) { return Evision_Converter<T>::to(env, obj, p, info); }

template<typename T> static
ERL_NIF_TERM evision_from(ErlNifEnv *env, const T& src) { return Evision_Converter<T>::from(env, src); }

template<typename T> static
ERL_NIF_TERM evision_from_as_binary(ErlNifEnv *env, const T& src, bool& success) { return Evision_Converter<T>::from_as_binary(env, src, success); }

template<typename T> static
ERL_NIF_TERM evision_from_as_map(ErlNifEnv *env, const T& src, ERL_NIF_TERM res_term, const char * class_name, bool& success) {
    const size_t num_items = 2;
    size_t item_index = 0;

    ERL_NIF_TERM keys[num_items];
    ERL_NIF_TERM values[num_items];

    keys[item_index] = kAtomRef;
    values[item_index] = res_term;
    item_index++;

    keys[item_index] = kAtomClass;
    values[item_index] = evision::nif::atom(env, class_name);
    item_index++;

    ERL_NIF_TERM map;
    if (enif_make_map_from_arrays(env, keys, values, item_index, &map)) {
        success = true;
        return map;
    } else {
        success = false;
        return evision::nif::error(env, "enif_make_map_from_arrays failed in evision_from_as_map");
    }
}

template<>
ERL_NIF_TERM evision_from_as_map(ErlNifEnv *env, const cv::Ptr<cv::cuda::GpuMat>& src, ERL_NIF_TERM res_term, const char * class_name, bool& success) {
    const size_t max_num_items = 9;
    size_t item_index = 0;

    ERL_NIF_TERM keys[max_num_items];
    ERL_NIF_TERM values[max_num_items];

    keys[item_index] = kAtomRef;
    values[item_index] = res_term;
    item_index++;

    keys[item_index] = kAtomClass;
    values[item_index] = enif_make_atom(env, class_name);
    item_index++;

    keys[item_index] = kAtomChannels;
    values[item_index] = enif_make_int(env, src->channels());
    item_index++;

    keys[item_index] = kAtomType;
    values[item_index] = __evision_get_mat_type(env, src->type());
    item_index++;

    keys[item_index] = kAtomRawType;
    values[item_index] = enif_make_int(env, src->type());
    item_index++;

    keys[item_index] = kAtomElemSize;
    values[item_index] = enif_make_int(env, src->elemSize());
    item_index++;

    keys[item_index] = kAtomStep;
    values[item_index] = enif_make_int(env, src->step);
    item_index++;

    keys[item_index] = kAtomShape;
    ERL_NIF_TERM shape[3];
    shape[0] = enif_make_int(env, src->rows);
    shape[1] = enif_make_int(env, src->cols);
    shape[2] = enif_make_int(env, src->channels());
#ifdef GLEAM_EVISION
    values[item_index] = enif_make_list_from_array(env, shape, 3);
#else
    values[item_index] = enif_make_tuple_from_array(env, shape, 3);
#endif
    item_index++;

    auto device_id = get_gpumat_device_id(src->cudaPtr());
    keys[item_index] = kAtomDeviceID;
    if (device_id) {
        values[item_index] = enif_make_int64(env, device_id.value());
    } else {
        values[item_index] = kAtomNil;
    }
    item_index++;

    ERL_NIF_TERM map;
    if (enif_make_map_from_arrays(env, keys, values, item_index, &map)) {
        success = true;
        return map;
    } else {
        success = false;
        return evision::nif::error(env, "enif_make_map_from_arrays failed in evision_from_as_map");
    }
}

template <>
ERL_NIF_TERM evision_from_as_binary(ErlNifEnv *env, const std::vector<uchar>& src, bool& success) {
    size_t n = static_cast<size_t>(src.size());
    ErlNifBinary binary;
    if ((success = enif_alloc_binary(n, &binary))) {
        memcpy(binary.data, src.data(), n);
        ERL_NIF_TERM ret = enif_make_binary(env, &binary);
        return ret;
    }
    return 0;
}

#include "modules/evision_video_api.h"

#define ERRWRAP2(expr, env, error_flag, error_term)         \
try                                                         \
{                                                           \
    expr;                                                   \
}                                                           \
catch (const cv::Exception &e)                              \
{                                                           \
    error_flag = true;                                      \
    error_term = evision::nif::error(env, e.msg.c_str());   \
}                                                           \
catch (const std::exception &e)                             \
{                                                           \
    error_flag = true;                                      \
    error_term = evision::nif::error(env, e.what());        \
}                                                           \
catch (...)                                                 \
{                                                           \
    error_flag = true;                                      \
    error_term = evision::nif::error(env,                   \
          "Unknown C++ exception from OpenCV code");        \
}

using namespace cv;


namespace {

template <class T, class U>
bool isRepresentable(U value) {
    return (std::numeric_limits<T>::min() <= value) && (value <= std::numeric_limits<T>::max());
}

TLSData<std::vector<std::string> > conversionErrorsTLS;

inline void pyPrepareArgumentConversionErrorsStorage(std::size_t size)
{
    std::vector<std::string>& conversionErrors = conversionErrorsTLS.getRef();
    conversionErrors.clear();
    conversionErrors.reserve(size);
}

template <class T>
class RefWrapper
{
public:
    RefWrapper(T& item) : item_(item) {}

    T& get() CV_NOEXCEPT { return item_; }

private:
    T& item_;
};

// In order to support this conversion on 3.x branch - use custom reference_wrapper
// and C-style array instead of std::array<T, N>
template <class T, std::size_t N>
bool parseSequence(ErlNifEnv *env, ERL_NIF_TERM obj, RefWrapper<T> (&value)[N], const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return true;
    }

    if (enif_is_tuple(env, obj)) {
        const ERL_NIF_TERM *terms;
        int sz = 0;
        enif_get_tuple(env, obj, &sz, &terms);
        if (sz != N)
        {
            failmsgp(env, "Can't parse '%s'. Expected sequence length %lu, got %lu",
                    info.name, N, sz);
            return false;
        }
        for (std::size_t i = 0; i < N; ++i)
        {
            if (!evision_to(env, terms[i], value[i].get(), info))
            {
                failmsgp(env, "Can't parse '%s'. Sequence item with index %lu has a "
                        "wrong type", info.name, i);
                return false;
            }
        }
    } else {
        failmsgp(env, "Can't parse '%s'. Expected a tuple/list with length %lu.",
            info.name, N);
        return false;
    }

    return true;
}
} // namespace

namespace traits {
template <bool Value>
struct BooleanConstant
{
    static const bool value = Value;
    typedef BooleanConstant<Value> type;
};

typedef BooleanConstant<true> TrueType;
typedef BooleanConstant<false> FalseType;

template <class T>
struct VoidType {
    typedef void type;
};

template <class T, class DType = void>
struct IsRepresentableAsMatDataType : FalseType
{
};

template <class T>
struct IsRepresentableAsMatDataType<T, typename VoidType<typename DataType<T>::channel_type>::type> : TrueType
{
};
} // namespace traits

typedef std::vector<uchar> vector_uchar;
typedef std::vector<char> vector_char;
typedef std::vector<int> vector_int;
typedef std::vector<float> vector_float;
typedef std::vector<double> vector_double;
typedef std::vector<size_t> vector_size_t;
typedef std::vector<Point> vector_Point;
typedef std::vector<Point2f> vector_Point2f;
typedef std::vector<Point3f> vector_Point3f;
typedef std::vector<Size> vector_Size;
typedef std::vector<Vec2f> vector_Vec2f;
typedef std::vector<Vec3f> vector_Vec3f;
typedef std::vector<Vec4f> vector_Vec4f;
typedef std::vector<Vec6f> vector_Vec6f;
typedef std::vector<Vec4i> vector_Vec4i;
typedef std::vector<Rect> vector_Rect;
typedef std::vector<Rect2d> vector_Rect2d;
typedef std::vector<RotatedRect> vector_RotatedRect;
typedef std::vector<KeyPoint> vector_KeyPoint;
typedef std::vector<Mat> vector_Mat;
typedef std::vector<std::vector<Mat> > vector_vector_Mat;
typedef std::vector<UMat> vector_UMat;
typedef std::vector<DMatch> vector_DMatch;
typedef std::vector<String> vector_String;
typedef std::vector<std::string> vector_string;
typedef std::vector<Scalar> vector_Scalar;

typedef std::vector<std::vector<char> > vector_vector_char;
typedef std::vector<std::vector<Point> > vector_vector_Point;
typedef std::vector<std::vector<Point2f> > vector_vector_Point2f;
typedef std::vector<std::vector<Point3f> > vector_vector_Point3f;
typedef std::vector<std::vector<DMatch> > vector_vector_DMatch;
typedef std::vector<std::vector<KeyPoint> > vector_vector_KeyPoint;

enum { ARG_NONE = 0, ARG_MAT = 1, ARG_SCALAR = 2 };

template <typename From, typename To>
static inline auto _do_cast_type(const From * ptr) -> To {
    return static_cast<To>(ptr[0]);
}

template <typename To>
auto _do_cast_f16(uint16_t value) -> To {
    // _do_cast_f16 only means to cast from half-precision float to float
    // it should always be encoded in little-endian format
    int n = 1;
    if(*(char *)&n != 1) {
        value = (value >> 8) | (value << 8);
    }

	uint32_t sn = (uint32_t)((value >> 15) & 0x1);
	uint32_t exp = (value >> 10) & 0x1f;
	uint32_t res = exp + 127 - 15;
	uint32_t fc = value & 0x3ff;
	
	if (exp == 0) {
		res = 0;
	} else if (exp == 0x1f) {
		res = 0xff;
	}
	
	union {
		float f;
		uint32_t b;
	} u;
	u.b = (uint32_t)(sn << 31) | (uint32_t)(res << 23) | (uint32_t)(fc << 13);
	return static_cast<To>(u.f);
}

static void * evision_cast(ErlNifBinary& bin, int cast_from, const std::vector<uint64_t> &sizes, const std::vector<uint64_t> &strides) {
    uint64_t num_elems = 1;
    for (size_t i = 0; i < sizes.size(); i++) {
        num_elems *= sizes[i];
    }
    size_t buffer_size = num_elems * sizeof(int32_t);
    int32_t * data = (int32_t *)enif_alloc(buffer_size);
    if (!data) {
        return nullptr;
    }
    
    if (cast_from == 1) {
        for (size_t i = 0; i < num_elems; i++) {
            data[i] = ((int64_t *)bin.data)[i];
        }
    } else if (cast_from == 2) {
        for (size_t i = 0; i < num_elems; i++) {
            data[i] = ((uint64_t *)bin.data)[i];
        }
    } else if (cast_from == 3) {
        for (size_t i = 0; i < num_elems; i++) {
            data[i] = ((uint32_t *)bin.data)[i];
        }
    }
    return data;
}

// special case, when the converter needs full ArgInfo structure
static bool evision_to(ErlNifEnv *env, ERL_NIF_TERM o, Mat& m, const ArgInfo& info)
{
    if(evision::nif::check_nil(env, o)) {
        return true;
    }

    evision_res<cv::Mat *> * in_res;
    if (enif_get_resource(env, o, evision_res<cv::Mat *>::type, (void **)&in_res))
    {
        if (in_res->val) {
            // should we copy the matrix?
            // probably yes so that the original matrix is not modified
            // because erlang/elixir users would expect that the original matrix to be unchanged
            in_res->val->copyTo(m);
            return true;
        }
        return false;
    }

    // Nx.tensor
    if (enif_is_map(env, o))
    {
        // we have to do copy because the incoming binary data is from erlang/elixir (immutable)
        bool needcopy = true, needcast = false;
        ERL_NIF_TERM struct_name_term;
        if (!enif_get_map_value(env, o, kAtomStructKey, &struct_name_term)) {
            return false;
        }
        if (!enif_is_identical(struct_name_term, kAtomNxTensor)) {
            return false;
        }

        ERL_NIF_TERM shape_term;
        if (!enif_get_map_value(env, o, kAtomShape, &shape_term)) {
            return false;
        }
        const ERL_NIF_TERM *shapes;
        int ndims = 0;
        int is_number = 0;
        // iex> Nx.tensor(1, type: :u8).shape
        // {} <- ndims == 0, so it is a number
        if (!enif_get_tuple(env, shape_term, &ndims, &shapes)) {
            return false;
        }
        if (ndims == 0) {
            is_number = 1;
            ndims = 1;
        }

        ERL_NIF_TERM data_term;
        if (!enif_get_map_value(env, o, kAtomData, &data_term)) {
            return false;
        }
        ErlNifBinary data;
        if (!enif_inspect_binary(env, data_term, &data)) {
            return false;
        }

        // now we need to get its type
        ERL_NIF_TERM type_term;
        int type;
        int cast_from = 0;
        // nx_tensor_type is only used when
        // ( info.arithm_op_src ) && ( ndims == 1 ) && ( size[0] <= 4 )
        int nx_tensor_type;
        if (!enif_get_map_value(env, o, kAtomType, &type_term)) {
            return false;
        }
        uint64_t originalElemSize;
        if (enif_is_identical(type_term, kAtomU8)) {
            nx_tensor_type = type = CV_8U;
            originalElemSize = 1;
        }
        else if (enif_is_identical(type_term, kAtomS8)) {
            nx_tensor_type = type = CV_8S;
            originalElemSize = 1;
        }
        else if (enif_is_identical(type_term, kAtomU16)) {
            nx_tensor_type = type = CV_16U;
            originalElemSize = 2;
        }
        else if (enif_is_identical(type_term, kAtomS16)) {
            nx_tensor_type = type = CV_16S;
            originalElemSize = 2;
        }
        else if (enif_is_identical(type_term, kAtomS32)) {
            nx_tensor_type = type = CV_32S;
            originalElemSize = 4;
        }
        else if (enif_is_identical(type_term, kAtomF16)) {
            nx_tensor_type = type = CV_16F;
            originalElemSize = 2;
        }
        else if (enif_is_identical(type_term, kAtomF32)) {
            nx_tensor_type = type = CV_32F;
            originalElemSize = 4;
        }
        else if (enif_is_identical(type_term, kAtomF64)) {
            nx_tensor_type = type = CV_64F;
            originalElemSize = 8;
        } else {
            if (enif_is_identical(type_term, kAtomS64)) {
                type = CV_32S;
                originalElemSize = 8;
                nx_tensor_type = INT32_MAX;
                needcast = true;
                cast_from = 1;
            } else if (enif_is_identical(type_term, kAtomU64)) {
                type = CV_32S;
                originalElemSize = 8;
                nx_tensor_type = INT32_MAX - 1;
                needcast = true;
                cast_from = 2;
            } else if (enif_is_identical(type_term, kAtomU32)) {
                type = CV_32S;
                originalElemSize = 4;
                nx_tensor_type = INT32_MAX - 2;
                needcast = true;
                cast_from = 3;
            } else {
                return false;
            }
        }

#ifndef CV_MAX_DIM
        const int CV_MAX_DIM = 32;
#endif 

        if (ndims > CV_MAX_DIM) {
            return false;
        }
        size_t elemsize = CV_ELEM_SIZE1(type);
        std::vector<uint64_t> _sizes(ndims);
        if (is_number) {
            _sizes[0] = 1;
        } else {
            for (int dim_i = 0; dim_i < ndims; dim_i++) {
                ErlNifUInt64 u64;
                ERL_NIF_TERM shape_i = shapes[dim_i];
                if (enif_get_uint64(env, shape_i, &u64)) {
                    _sizes[dim_i] = u64;
                } else {
                    return false;
                }
            }
        }
        
        std::vector<uint64_t> _strides(ndims);
        _strides[ndims - 1] = originalElemSize;
        if (is_number == 0) {
            for (int dim_i = ndims - 2; dim_i >= 0; dim_i--) {
                _strides[dim_i] = _strides[dim_i + 1] * _sizes[dim_i + 1];
            }
        }

        // ---- debug ----
        // printf("Incoming ndarray '%s': ndims=%d  _sizes=[ ", info.name, ndims);
        // for (int i = 0; i < ndims; i++) {
        //     printf("%llu ", _sizes[i]);
        // }
        // printf("]  _strides=[ ");
        // for (int i = 0; i < ndims; i++) {
        //     printf("%llu ", _strides[i]);
        // }
        // printf("]\r\n");
        // ---- debug ----

        bool ismultichannel = ndims == 3 && _sizes[2] <= CV_CN_MAX;
        // if (pyopencv_Mat_TypePtr && PyObject_TypeCheck(o, pyopencv_Mat_TypePtr))
        // {
        //     bool wrapChannels = false;
        //     PyObject* pyobj_wrap_channels = PyObject_GetAttrString(o, "wrap_channels");
        //     if (pyobj_wrap_channels)
        //     {
        //         if (!pyopencv_to_safe(pyobj_wrap_channels, wrapChannels, ArgInfo("cv.Mat.wrap_channels", 0)))
        //         {
        //             // TODO extra message
        //             Py_DECREF(pyobj_wrap_channels);
        //             return false;
        //         }
        //         Py_DECREF(pyobj_wrap_channels);
        //     }
        //     ismultichannel = wrapChannels && ndims >= 1;
        // }

        for (int i = ndims-1; i >= 0 && !needcopy; i--)
        {
            // these checks handle cases of
            //  a) multi-dimensional (ndims > 2) arrays, as well as simpler 1- and 2-dimensional cases
            //  b) transposed arrays, where _strides[] elements go in non-descending order
            //  c) flipped arrays, where some of _strides[] elements are negative
            // the _sizes[i] > 1 is needed to avoid spurious copies when NPY_RELAXED_STRIDES is set
            if( (i == ndims-1 && _sizes[i] > 1 && (size_t)_strides[i] != elemsize) ||
                (i < ndims-1 && _sizes[i] > 1 && _strides[i] < _strides[i+1]) )
                needcopy = true;
        }

        if (ismultichannel)
        {
            int channels = ndims >= 1 ? (int)_sizes[ndims - 1] : 1;
            if (channels > CV_CN_MAX)
            {
                failmsg(env, "%s unable to wrap channels, too high (%d > CV_CN_MAX=%d)", info.name, (int)channels, (int)CV_CN_MAX);
                return false;
            }
            ndims--;
            type |= CV_MAKETYPE(0, channels);

            if (ndims >= 1 && _strides[ndims - 1] != elemsize*_sizes[ndims])
                needcopy = true;

            elemsize = CV_ELEM_SIZE(type);
        }

        void * mat_data = data.data;
        bool enif_allocator = false;
        if (needcast) {
            // we always cast to int32_t
            mat_data = evision_cast(data, cast_from, _sizes, _strides);
            enif_allocator = true;

            _strides[ndims - 1] = elemsize;
            for (int dim_i = ndims - 2; dim_i >= 0; dim_i--) {
                _strides[dim_i] = _strides[dim_i + 1] * _sizes[dim_i + 1];
            }
        }

        int size[CV_MAX_DIM+1] = {};
        size_t step[CV_MAX_DIM+1] = {};

        // Normalize strides in case NPY_RELAXED_STRIDES is set
        size_t default_step = elemsize;
        for ( int i = ndims - 1; i >= 0; --i )
        {
            size[i] = (int)_sizes[i];
            if ( size[i] > 1 )
            {
                step[i] = (size_t)_strides[i];
                default_step = step[i] * size[i];
            }
            else
            {
                step[i] = default_step;
                default_step *= size[i];
            }
        }

        // see https://github.com/opencv/opencv/issues/24057
        // e.g., Nx.tensor([1,2,3,4]), Nx.tensor(1)
        if ( ( info.arithm_op_src ) && ( ndims == 1 ) && ( size[0] <= 4 ) )
        {
            const int sz  = size[0]; // Real Data Length(1, 2, 3 or 4)
            const int sz2 = 4;       // Scalar has 4 elements.
            m = Mat::zeros(sz2, 1, CV_64F);

            const char *base_ptr = (const char *)data.data;
            for(int i = 0; i < sz; i++ )
            {
                const void * oi = static_cast<const void *>(base_ptr + step[0] * i);
                switch (nx_tensor_type)
                {
                case CV_8S:
                    m.at<double>(i) = _do_cast_type<int8_t, double>((const int8_t *)oi);
                    break;
                case CV_8U:
                    m.at<double>(i) = _do_cast_type<uint8_t, double>((const uint8_t *)oi);
                    break;
                case CV_16S:
                    m.at<double>(i) = _do_cast_type<int16_t, double>((const int16_t *)oi);
                    break;
                case CV_16U:
                    m.at<double>(i) = _do_cast_type<uint16_t, double>((const uint16_t *)oi);
                    break;
                case CV_16F:
                    m.at<double>(i) = _do_cast_f16<double>(*(const uint16_t *)oi);
                    break;
                case CV_32S:
                    m.at<double>(i) = _do_cast_type<int32_t, double>((const int32_t *)oi);
                    break;
                case CV_32F:
                    m.at<double>(i) = _do_cast_type<float, double>((const float *)oi);
                    break;
                case CV_64F:
                    m.at<double>(i) = _do_cast_type<double, double>((const double *)oi);
                    break;
                case INT32_MAX:
                    m.at<double>(i) = _do_cast_type<int64_t, double>((const int64_t *)oi);
                    break;
                case INT32_MAX - 1:
                    m.at<double>(i) = _do_cast_type<uint64_t, double>((const uint64_t *)oi);
                    break;
                case INT32_MAX - 2:
                    m.at<double>(i) = _do_cast_type<uint32_t, double>((const uint32_t *)oi);
                    break;
                default:
                    break;
                }
            }
            if (info.arithm_op_src && is_number) {
                // Normally cv.XXX(x) means cv.XXX( (x, 0., 0., 0.) );
                // However  cv.add(mat,x) means cv::add(mat, (x,x,x,x) ).
                m.at<double>(1) = m.at<double>(2) = m.at<double>(3) = m.at<double>(0);
            }
            return true;
        }

        // handle degenerate case
        // FIXIT: Don't force 1D for Scalars
        if (ndims == 0) {
            size[ndims] = 1;
            step[ndims] = elemsize;
            ndims++;
        }

        Mat casted(ndims, size, type, mat_data, step);
        casted.copyTo(m);
        if (enif_allocator) {
            enif_free(mat_data);
        }
        return true;
    }

    int i32;
    if (enif_get_int(env, o, &i32))
    {
        double v[] = {static_cast<double>(i32), 0., 0., 0.};
        if (info.arithm_op_src)
        {
            // Normally cv.XXX(x) means cv.XXX( (x, 0., 0., 0.) );
            // However  cv.add(mat,x) means cv::add(mat, (x,x,x,x) ).
            v[1] = v[2] = v[3] = v[0];
        }
        m = Mat(4, 1, CV_64F, v).clone();
        return true;
    }

    double f64;
    if (enif_get_double(env, o, &f64))
    {
        double v[] = {f64, 0., 0., 0.};
        if (info.arithm_op_src)
        {
            // Normally cv.XXX(x) means cv.XXX( (x, 0., 0., 0.) );
            // However  cv.add(mat,x) means cv::add(mat, (x,x,x,x) ).
            v[1] = v[2] = v[3] = v[0];
        }
        m = Mat(4, 1, CV_64F, v).clone();
        return true;
    }

    if (enif_is_tuple(env, o))
    {
        const ERL_NIF_TERM *terms;
        int sz = 0, i = 0;
        enif_get_tuple(env, o, &sz, &terms);
        const int sz2 = info.arithm_op_src ? std::max(4, sz) : sz;
        m = Mat(sz2, 1, CV_64F);
        for (i = 0; i < sz; i++)
        {
            int i32;
            double f64;
            ERL_NIF_TERM oi = terms[i];
            if (enif_get_int(env, oi, &i32))
            {
                m.at<double>(i) = (double)(i32);
            }
            else if (enif_get_double(env, oi, &f64))
            {
                m.at<double>(i) = (double)(f64);
            }    
            else
            {
                failmsg(env, "%s is not a numerical tuple", info.name);
                m.release();
                return false;
            }
        }
        for (i = sz; i < sz2; i++) {
            m.at<double>(i) = 0.0;
        }
        return true;
    }

    return false;
}

static bool evision_to(ErlNifEnv *env, ERL_NIF_TERM o, cv::UMat& m, const ArgInfo& info)
{
    if(evision::nif::check_nil(env, o)) {
        return true;
    }

    evision_res<cv::UMat *> * in_res;
    if( enif_get_resource(env, o, evision_res<cv::UMat *>::type, (void **)&in_res) ) {
        if (in_res->val) {
            // should we copy the matrix?
            // probably yes so that the original matrix is not modified
            // because erlang/elixir users would expect that the original matrix to be unchanged
            in_res->val->copyTo(m);
            return true;
        }
        return false;
    }

    return false;
}

template<typename _Tp, int m, int n>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM o, Matx<_Tp, m, n>& mx, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, o)) {
        return true;
    }

    Mat tmp;
    if (!evision_to(env, o, tmp, info)) {
        return false;
    }

    tmp.copyTo(mx);
    return true;
}

template<typename _Tp, int cn>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM o, Vec<_Tp, cn>& vec, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, o)) {
        return true;
    }

    return evision_to(env, o, (Matx<_Tp, cn, 1>&)vec, info);
}

#ifdef HAVE_OPENCV_FEATURES2D
template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, cv::Ptr<cv::Feature2D> &feature, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return false;
    }

    {
        using source_type = evision_res<cv::Ptr<cv::AKAZE>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::AffineFeature>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::AgastFeatureDetector>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::BRISK>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::FastFeatureDetector>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::GFTTDetector>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::KAZE>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::MSER>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::ORB>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::SIFT>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::SimpleBlobDetector>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

#ifdef HAVE_OPENCV_XFEATURES2D
    {
        using source_type = evision_res<cv::Ptr<cv::xfeatures2d::AffineFeature2D>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::xfeatures2d::BEBLID>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::xfeatures2d::BoostDesc>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::xfeatures2d::BriefDescriptorExtractor>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::xfeatures2d::DAISY>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::xfeatures2d::FREAK>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::xfeatures2d::HarrisLaplaceFeatureDetector>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::xfeatures2d::LATCH>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::xfeatures2d::LUCID>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::xfeatures2d::MSDDetector>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::xfeatures2d::SURF>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::xfeatures2d::StarDetector>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::xfeatures2d::TBMR>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::xfeatures2d::TEBLID>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

    {
        using source_type = evision_res<cv::Ptr<cv::xfeatures2d::VGG>>;
        source_type * in_res;
        if (enif_get_resource(env, obj, source_type::type, (void **)&in_res) ) {
            feature = in_res->val;
            return true;
        }
    }

#endif

    return false;
}
#endif

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Mat& m)
{
    if (!m.data) {
        return evision::nif::error(env, "empty matrix");
    }

    evision_res<cv::Mat *> * res;
    if (alloc_resource(&res)) {
        res->val = new cv::Mat();
        // should we copy the matrix?
        // probably no, because all input matrice are copied when calling `evision_to`
        // and this function returns the output/result matrix, which should already be a new matrix
        *res->val = m;
    } else {
        return evision::nif::error(env, "out of memory");
    }

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);

    return _evision_make_mat_resource_into_map(env, m, ret);
}

template<typename _Tp, int m, int n>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Matx<_Tp, m, n>& matx)
{
    return evision_from(env, Mat(matx));
}

template<typename T>
struct Evision_Converter< cv::Ptr<T> >
{
    static ERL_NIF_TERM from(ErlNifEnv *env, const cv::Ptr<T>& p)
    {
        if (!p) {
            return kAtomNil;
        }

        return evision_from(env, *p);
    }
    static bool to(ErlNifEnv * env, ERL_NIF_TERM o, Ptr<T>& p, const ArgInfo& info)
    {
        if (evision::nif::check_nil(env, o)) {
            if (info.outputarg) return true;
            return info.has_default;
        }

        p = makePtr<T>();
        return evision_to(env, o, *p, info);
    }
};

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, void*& ptr, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return true;
    }

    ErlNifSInt64 i64;
    if (!enif_get_int64(env, obj, (ErlNifSInt64 *)&i64)) {
        return info.has_default;
    }
        
    ptr = reinterpret_cast<void *>(i64);
    if (ptr == nullptr && info.has_default) {
        return true;
    }

    return ptr != nullptr;
}

static ERL_NIF_TERM evision_from(ErlNifEnv *env, void*& ptr)
{
    return enif_make_int64(env, (int64_t)(int64_t*)ptr);
}

static bool evision_to(ErlNifEnv *env, ERL_NIF_TERM o, Scalar& s, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, o)) {
        return info.has_default || info.outputarg;
    }

    double f64;
    ErlNifSInt64 i64;
    ErlNifUInt64 u64;

    if (enif_is_tuple(env, o)) {
        int n = 0;
        const ERL_NIF_TERM * terms;
        enif_get_tuple(env, o, &n, &terms);
        if (n > 4) {
            failmsg(env, "Scalar value for argument '%s' is longer than 4", info.name);
            return false;
        }

        for (int i = 0; i < n; i++) {
            if (enif_get_double(env, terms[i], &f64)) {
                s[i] = f64;
            } else if (enif_get_int64(env, terms[i], (ErlNifSInt64 *)&i64)) {
                s[i] = i64;
            } else if (enif_get_uint64(env, terms[i], (ErlNifUInt64 *)&u64)) {
                s[i] = u64;
            } else {
                failmsg(env, "Scalar value for argument '%s' is not numeric", info.name);
                return false;
            }
        }
        for (int i = n; i < 4; i++) {
            s[i] = 0;
        }
        return true;
    } else if (enif_is_number(env, o)) {
        if (enif_get_double(env, o, &f64)) {
            s[0] = f64;
        } else if (enif_get_int64(env, o, (ErlNifSInt64 *)&i64)) {
            s[0] = i64;
        } else if (enif_get_uint64(env, o, (ErlNifUInt64 *)&u64)) {
            s[0] = u64;
        } else {
            failmsg(env, "Scalar value for argument '%s' cannot fit in a double value", info.name);
            return false;
        }
        for (int i = 1; i < 4; i++) {
            s[i] = 0;
        }
        return true;
    } else {
        failmsg(env, "Scalar value for argument '%s' is not numeric", info.name);
        return false;
    }

    return true;
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Scalar& src)
{
    return enif_make_tuple4(env, 
        enif_make_double(env, src[0]),
        enif_make_double(env, src[1]),
        enif_make_double(env, src[2]),
        enif_make_double(env, src[3])
    );
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const bool& value)
{
    if (value) return kAtomTrue;
    return kAtomFalse;
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, bool& value, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return info.has_default || info.outputarg;
    }

    if (enif_is_atom(env, obj))
    {
        std::string boolean_val;
        if (evision::nif::get_atom(env, obj, boolean_val)) {
            value = (boolean_val == "true");
            return true;
        }
    }
    else if (enif_is_number(env, obj)) {
        double f64;
        ErlNifSInt64 i64;
        ErlNifUInt64 u64;
        if (enif_get_double(env, obj, &f64)) {
            if (f64 != 0) {
                value = true;
                return true;
            }
        } else if (enif_get_int64(env, obj, (ErlNifSInt64 *)&i64)) {
            if (i64 != 0) {
                value = true;
                return true;
            }
        } else if (enif_get_uint64(env, obj, (ErlNifUInt64 *)&u64)) {
            if (u64 != 0) {
                value = true;
                return true;
            }
        }
    }

    failmsg(env, "Argument '%s' is not convertible to bool", info.name);
    return false;
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const size_t& value)
{
    return enif_make_uint64(env, value);
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const int& value)
{
    return enif_make_int(env, value);
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, unsigned int& value, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return info.has_default || info.outputarg;
    }

    uint32_t u32;

    if (enif_get_uint(env, obj, &u32))
    {
        value = u32;
    }
    else
    {
        failmsg(env, "Argument '%s' is required to be an integer", info.name);
        return false;
    }
    return true;
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, int& value, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return info.has_default || info.outputarg;
    }

    int32_t i32;

    if (enif_get_int(env, obj, &i32))
    {
        value = i32;
    }
    else
    {
        failmsg(env, "Argument '%s' is required to be an integer", info.name);
        return false;
    }
    return true;
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, unsigned long &val, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return info.has_default || info.outputarg;
    }

    ErlNifUInt64 u64;
    if (!enif_get_uint64(env, obj, (ErlNifUInt64 *)&u64))
        return false;
    val = (unsigned long)u64;
    return 1;
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, unsigned long long & value, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return info.has_default || info.outputarg;
    }

    ErlNifUInt64 u64;

    if (enif_get_uint64(env, obj, (ErlNifUInt64 *)&u64))
    {
        value = u64;
    }
    else
    {
        failmsg(env, "Argument '%s' is required to be an integer", info.name);
        return false;
    }
    return true;
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, int64_t& value, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return info.has_default || info.outputarg;
    }

    ErlNifSInt64 i64;

    if (enif_get_int64(env, obj, (ErlNifSInt64 *)&i64))
    {
        value = i64;
    }
    else
    {
        failmsg(env, "Argument '%s' is required to be an integer", info.name);
        return false;
    }
    return true;
}

// There is conflict between "size_t" and "unsigned int".
// They are the same type on some 32-bit platforms.
template<typename T>
struct Evision_Converter
    < T, typename std::enable_if< std::is_same<unsigned int, T>::value && !std::is_same<unsigned int, size_t>::value >::type >
{
    static inline ERL_NIF_TERM from(ErlNifEnv *env, const unsigned int& value)
    {
        return enif_make_uint(env, value);
    }

    static inline bool to(ErlNifEnv *env, ERL_NIF_TERM obj, unsigned int& value, const ArgInfo& info)
    {
        CV_UNUSED(info);
        if(evision::nif::check_nil(env, obj))
            return true;
        int i32;
        ErlNifSInt64 i64;
        if(enif_get_int(env, obj, &i32))
            value = i32;
        else if(enif_get_int64(env, obj, (ErlNifSInt64 *)&i64))
            value = (unsigned int)i64;
        else
            return false;
        return value != (unsigned int)-1;
    }
};

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const uchar& value)
{
    return enif_make_int(env, value);
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, uchar& value, const ArgInfo& info)
{
    CV_UNUSED(info);
    int32_t i32;

    if (enif_get_int(env, obj, &i32))
    {
        value = cv::saturate_cast<uchar>(i32);
        return i32 != -1;
    } else {
        return info.has_default || info.outputarg;
    }
}

template <>
bool evision_to_safe(ErlNifEnv *env, ERL_NIF_TERM o, std::vector<uchar>& data, const ArgInfo& info)
{
    ErlNifBinary erl_bin;
    if (enif_inspect_binary(env, o, &erl_bin)) {
        data.assign(erl_bin.data, erl_bin.data + erl_bin.size);
        return true;
    }

    if (enif_is_list(env, o)) {
        unsigned n = 0;
        enif_get_list_length(env, o, &n);

        ERL_NIF_TERM head, tail, obj = o;
        size_t i = 0;
        while (i < n) {
            if (enif_get_list_cell(env, obj, &head, &tail)) {
                uchar item = 0;
                if (!evision_to(env, head, item, info)) {
                    return false;
                }
                data.push_back(item);
                obj = tail;
                i++;
            } else {
                return false;
            }
        }
        return true;
    }

    return false;
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, char& value, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return info.has_default || info.outputarg;
    }

    int32_t i32;
    if (enif_get_int(env, obj, &i32))
    {
        value = saturate_cast<char>(i32);
    } else {
        failmsg(env, "Argument '%s' is required to be an integer", info.name);
        return false;
    }

    return true;
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const double& value)
{
    return enif_make_double(env, value);
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, double& value, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return info.has_default || info.outputarg;
    }

    double f64;
    long i64;
    if (enif_get_double(env, obj, &f64))
    {
        value = f64;
    } else if (enif_get_int64(env, obj, (ErlNifSInt64 *)&i64)) {
        value = i64;
    } else {
        failmsg(env, "Argument '%s' is required to be an integer", info.name);
        return false;
    }

    return true;
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const float& value)
{
    return enif_make_double(env, value);
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, float& value, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return info.has_default || info.outputarg;
    }

    ErlNifSInt64 i64;
    double f64;
    if (enif_get_int64(env, obj, (ErlNifSInt64 *)&i64))
    {
        value = static_cast<float>(i64);
    }
    else if (enif_get_double(env, obj, &f64))
    {
        value = static_cast<float>(f64);
    }
    else
    {
        failmsg(env, "Argument '%s' can't be treated as a float", info.name);
        return false;
    }
    return true;
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const int64& value)
{
    return enif_make_int64(env, value);
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const std::vector<char>& value)
{
    ERL_NIF_TERM erl_string;
    unsigned char * ptr;
    size_t len = value.size();
    if ((ptr = enif_make_new_binary(env, len, &erl_string)) != nullptr) {
        strncpy((char *)ptr, value.data(), len);
        return erl_string;
    } else {
        return kAtomOutOfMemory;
    }
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const String& value)
{
    ERL_NIF_TERM erl_string;
    unsigned char * ptr;
    size_t len = strlen(value.c_str());
    if ((ptr = enif_make_new_binary(env, len, &erl_string)) != nullptr) {
        strncpy((char *)ptr, value.c_str(), len);
        return erl_string;
    } else {
        return kAtomOutOfMemory;
    }
}

#if CV_VERSION_MAJOR == 3
template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const std::string& value)
{
    ERL_NIF_TERM erl_string;
    unsigned char * ptr;
    size_t len = strlen(value.c_str());
    if ((ptr = enif_make_new_binary(env, len, &erl_string)) != nullptr) {
        strncpy((char *)ptr, value.c_str(), len);
        return erl_string;
    } else {
        return kAtomOutOfMemory;
    }
}
#endif

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, String &value, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        if (strncmp(info.name, "nodeName", 8) == 0) {
            return true;
        }
        return info.has_default;
    }

    std::string str;
    int ret = evision::nif::get(env, obj, str);
    value = str;
    if (ret > 0) return true;

    ret = evision::nif::get_atom(env, obj, str);
    value = str;
    return (ret > 0);
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Size& sz, const ArgInfo& info)
{
    RefWrapper<int> values[] = {RefWrapper<int>(sz.width),
                                RefWrapper<int>(sz.height)};
    return parseSequence(env, obj, values, info);
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Size& sz)
{
    return enif_make_tuple2(env, evision_from(env, sz.width), evision_from(env, sz.height));
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Size_<float>& sz, const ArgInfo& info)
{
    RefWrapper<float> values[] = {RefWrapper<float>(sz.width),
                                  RefWrapper<float>(sz.height)};
    return parseSequence(env, obj, values, info);
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Size_<float>& sz)
{
    return enif_make_tuple2(env, evision_from(env, sz.width), evision_from(env, sz.height));
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Rect& r, const ArgInfo& info)
{
    RefWrapper<int> values[] = {RefWrapper<int>(r.x), RefWrapper<int>(r.y),
                                RefWrapper<int>(r.width),
                                RefWrapper<int>(r.height)};
    return parseSequence(env, obj, values, info);
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Rect& r)
{
    return enif_make_tuple4(env,
        evision_from(env, r.x),
        evision_from(env, r.y),
        evision_from(env, r.width),
        evision_from(env, r.height)
    );
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Rect2f& r, const ArgInfo& info)
{
    RefWrapper<float> values[] = {
        RefWrapper<float>(r.x), RefWrapper<float>(r.y),
        RefWrapper<float>(r.width), RefWrapper<float>(r.height)};
    return parseSequence(env, obj, values, info);
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Rect2f& r)
{
    return enif_make_tuple4(env,
        evision_from(env, r.x),
        evision_from(env, r.y),
        evision_from(env, r.width),
        evision_from(env, r.height)
    );
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Rect2d& r, const ArgInfo& info)
{
    RefWrapper<double> values[] = {
        RefWrapper<double>(r.x), RefWrapper<double>(r.y),
        RefWrapper<double>(r.width), RefWrapper<double>(r.height)};
    return parseSequence(env, obj, values, info);
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Rect2d& r)
{
    return enif_make_tuple4(env,
        evision_from(env, r.x),
        evision_from(env, r.y),
        evision_from(env, r.width),
        evision_from(env, r.height)
    );
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Range& r, const ArgInfo& info)
{
    const ERL_NIF_TERM *terms;
    int length = 0;
    if (enif_get_tuple(env, obj, &length, &terms) && length == 2) {
        if (evision_to(env, terms[0], r.start, info) && evision_to(env, terms[1], r.end, info)) {
            return true;
        }
    }

    String all;
    if (evision::nif::get_atom(env, obj, all)) {
        if (all == "all") {
            r = Range::all();
            return true;
        }
    }
    
    return info.has_default;
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Range& r)
{
    return enif_make_tuple2(env,
        evision_from(env, r.start),
        evision_from(env, r.end)
    );
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Point& p, const ArgInfo& info)
{
    RefWrapper<int> values[] = {RefWrapper<int>(p.x), RefWrapper<int>(p.y)};
    return parseSequence(env, obj, values, info);
}

template <>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Point2f& p, const ArgInfo& info)
{
    RefWrapper<float> values[] = {RefWrapper<float>(p.x),
                                  RefWrapper<float>(p.y)};
    return parseSequence(env, obj, values, info);
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Point2d& p, const ArgInfo& info)
{
    RefWrapper<double> values[] = {RefWrapper<double>(p.x),
                                   RefWrapper<double>(p.y)};
    return parseSequence(env, obj, values, info);
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Point3f& p, const ArgInfo& info)
{
    RefWrapper<float> values[] = {RefWrapper<float>(p.x),
                                  RefWrapper<float>(p.y),
                                  RefWrapper<float>(p.z)};
    return parseSequence(env, obj, values, info);
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Point3d& p, const ArgInfo& info)
{
    RefWrapper<double> values[] = {RefWrapper<double>(p.x),
                                   RefWrapper<double>(p.y),
                                   RefWrapper<double>(p.z)};
    return parseSequence(env, obj, values, info);
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Point& p)
{
    return enif_make_tuple2(env,
        evision_from(env, p.x),
        evision_from(env, p.y)
    );
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Point2f& p)
{
    return enif_make_tuple2(env,
        evision_from(env, p.x),
        evision_from(env, p.y)
    );
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Point3f& p)
{
    return enif_make_tuple3(env,
        evision_from(env, p.x),
        evision_from(env, p.y),
        evision_from(env, p.x)
    );
}

static bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Vec4d& v, ArgInfo& info)
{
    RefWrapper<double> values[] = {RefWrapper<double>(v[0]), RefWrapper<double>(v[1]),
                                   RefWrapper<double>(v[2]), RefWrapper<double>(v[3])};
    return parseSequence(env, obj, values, info);
}

static bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Vec4f& v, ArgInfo& info)
{
    RefWrapper<float> values[] = {RefWrapper<float>(v[0]), RefWrapper<float>(v[1]),
                                  RefWrapper<float>(v[2]), RefWrapper<float>(v[3])};
    return parseSequence(env, obj, values, info);
}

static bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Vec4i& v, ArgInfo& info)
{
    RefWrapper<int> values[] = {RefWrapper<int>(v[0]), RefWrapper<int>(v[1]),
                                RefWrapper<int>(v[2]), RefWrapper<int>(v[3])};
    return parseSequence(env, obj, values, info);
}

static bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Vec3d& v, ArgInfo& info)
{
    RefWrapper<double> values[] = {RefWrapper<double>(v[0]),
                                   RefWrapper<double>(v[1]),
                                   RefWrapper<double>(v[2])};
    return parseSequence(env, obj, values, info);
}

static bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Vec3f& v, ArgInfo& info)
{
    RefWrapper<float> values[] = {RefWrapper<float>(v[0]),
                                  RefWrapper<float>(v[1]),
                                  RefWrapper<float>(v[2])};
    return parseSequence(env, obj, values, info);
}

static bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Vec3i& v, ArgInfo& info)
{
    RefWrapper<int> values[] = {RefWrapper<int>(v[0]), RefWrapper<int>(v[1]),
                                RefWrapper<int>(v[2])};
    return parseSequence(env, obj, values, info);
}

static bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Vec2d& v, ArgInfo& info)
{
    RefWrapper<double> values[] = {RefWrapper<double>(v[0]),
                                   RefWrapper<double>(v[1])};
    return parseSequence(env, obj, values, info);
}

static bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Vec2f& v, ArgInfo& info)
{
    RefWrapper<float> values[] = {RefWrapper<float>(v[0]),
                                  RefWrapper<float>(v[1])};
    return parseSequence(env, obj, values, info);
}

static bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, Vec2i& v, ArgInfo& info)
{
    RefWrapper<int> values[] = {RefWrapper<int>(v[0]), RefWrapper<int>(v[1])};
    return parseSequence(env, obj, values, info);
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Vec4d& v)
{
    return enif_make_tuple4(env,
        evision_from(env, v[0]),
        evision_from(env, v[1]),
        evision_from(env, v[2]),
        evision_from(env, v[3])
    );
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Vec4f& v)
{
    return enif_make_tuple4(env,
        evision_from(env, v[0]),
        evision_from(env, v[1]),
        evision_from(env, v[2]),
        evision_from(env, v[3])
    );
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Vec4i& v)
{
    return enif_make_tuple4(env,
        evision_from(env, v[0]),
        evision_from(env, v[1]),
        evision_from(env, v[2]),
        evision_from(env, v[3])
    );
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Vec3d& v)
{
    return enif_make_tuple3(env,
        evision_from(env, v[0]),
        evision_from(env, v[1]),
        evision_from(env, v[2])
    );
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Vec3f& v)
{
    return enif_make_tuple3(env,
        evision_from(env, v[0]),
        evision_from(env, v[1]),
        evision_from(env, v[2])
    );
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Vec3i& v)
{
    return enif_make_tuple3(env,
        evision_from(env, v[0]),
        evision_from(env, v[1]),
        evision_from(env, v[2])
    );
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Vec2d& v)
{
    return enif_make_tuple2(env,
        evision_from(env, v[0]),
        evision_from(env, v[1])
    );
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Vec2f& v)
{
    return enif_make_tuple2(env,
        evision_from(env, v[0]),
        evision_from(env, v[1])
    );
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Vec2i& v)
{
    return enif_make_tuple2(env,
        evision_from(env, v[0]),
        evision_from(env, v[1])
    );
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Point2d& p)
{
    return enif_make_tuple2(env,
        evision_from(env, p.x),
        evision_from(env, p.y)
    );
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Point3d& p)
{
    return enif_make_tuple3(env,
        evision_from(env, p.x),
        evision_from(env, p.y),
        evision_from(env, p.z)
    );
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const std::pair<int, double>& src)
{
    return enif_make_tuple2(env,
        evision_from(env, src.first),
        evision_from(env, src.second)
    );
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, TermCriteria& dst, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return info.has_default || info.outputarg;
    }

    const ERL_NIF_TERM *terms;
    int length;
    if (!enif_get_tuple(env, obj, &length, &terms)) {
        failmsg(env, "Can't parse '%s' as TermCriteria."
                "Input argument is not a tuple",
                info.name);
        return false;
    }

    const std::size_t sequenceSize = length;
    if (sequenceSize != 3) {
        failmsg(env, "Can't parse '%s' as TermCriteria. Expected sequence length 3, "
                "got %lu",
                info.name, sequenceSize);
        return false;
    }
    {
        const String typeItemName = format("'%s' criteria type", info.name);
        const ArgInfo typeItemInfo(typeItemName.c_str(), false);
        if (!evision_to(env, terms[0], dst.type, typeItemInfo))
        {
            return false;
        }
    }
    {
        const String maxCountItemName = format("'%s' max count", info.name);
        const ArgInfo maxCountItemInfo(maxCountItemName.c_str(), false);
        if (!evision_to(env, terms[1], dst.maxCount, maxCountItemInfo))
        {
            return false;
        }
    }
    {
        const String epsilonItemName = format("'%s' epsilon", info.name);
        const ArgInfo epsilonItemInfo(epsilonItemName.c_str(), false);
        if (!evision_to(env, terms[2], dst.epsilon, epsilonItemInfo))
        {
            return false;
        }
    }
    return true;
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const TermCriteria& src)
{
    return enif_make_tuple3(env,
        evision_from(env, src.type),
        evision_from(env, src.maxCount),
        evision_from(env, src.epsilon)
    );
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, RotatedRect& dst, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return true;
    }

    const ERL_NIF_TERM *terms;
    int length;
    if (!enif_get_tuple(env, obj, &length, &terms)) {
        failmsg(env, "Can't parse '%s' as RotatedRect."
                "Input argument is not a tuple",
                info.name);
        return false;
    }

    const std::size_t sequenceSize = length;
    if (sequenceSize != 3)
    {
        failmsg(env, "Can't parse '%s' as RotatedRect. Expected sequence length 3, got %lu",
                info.name, sequenceSize);
        return false;
    }
    {
        const String centerItemName = format("'%s' center point", info.name);
        const ArgInfo centerItemInfo(centerItemName.c_str(), false);
        if (!evision_to(env, terms[0], dst.center, centerItemInfo))
        {
            return false;
        }
    }
    {
        const String sizeItemName = format("'%s' size", info.name);
        const ArgInfo sizeItemInfo(sizeItemName.c_str(), false);
        if (!evision_to(env, terms[1], dst.size, sizeItemInfo))
        {
            return false;
        }
    }
    {
        const String angleItemName = format("'%s' angle", info.name);
        const ArgInfo angleItemInfo(angleItemName.c_str(), false);
        if (!evision_to(env, terms[2], dst.angle, angleItemInfo))
        {
            return false;
        }
    }
    return true;
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const RotatedRect& src)
{
    return enif_make_tuple3(env,
        enif_make_tuple2(env,
            evision_from(env, src.center.x),
            evision_from(env, src.center.y)
        ),
        enif_make_tuple2(env,
            evision_from(env, src.size.width),
            evision_from(env, src.size.height)
        ),
        evision_from(env, src.angle)
    );
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const Moments& m)
{
    ERL_NIF_TERM ret = enif_make_new_map(env);
    ERL_NIF_TERM iter = ret;
    if (
        enif_make_map_put(env, iter, kAtomM00, evision_from(env, m.m00), &iter) &&
        enif_make_map_put(env, iter, kAtomM10, evision_from(env, m.m10), &iter) &&
        enif_make_map_put(env, iter, kAtomM01, evision_from(env, m.m01), &iter) &&

        enif_make_map_put(env, iter, kAtomM20, evision_from(env, m.m20), &iter) &&
        enif_make_map_put(env, iter, kAtomM11, evision_from(env, m.m11), &iter) &&
        enif_make_map_put(env, iter, kAtomM02, evision_from(env, m.m02), &iter) &&

        enif_make_map_put(env, iter, kAtomM30, evision_from(env, m.m30), &iter) &&
        enif_make_map_put(env, iter, kAtomM21, evision_from(env, m.m21), &iter) &&
        enif_make_map_put(env, iter, kAtomM12, evision_from(env, m.m12), &iter) &&
        enif_make_map_put(env, iter, kAtomM03, evision_from(env, m.m03), &iter) &&

        enif_make_map_put(env, iter, kAtomMu20, evision_from(env, m.mu20), &iter) &&
        enif_make_map_put(env, iter, kAtomMu11, evision_from(env, m.mu11), &iter) &&
        enif_make_map_put(env, iter, kAtomMu02, evision_from(env, m.mu02), &iter) &&

        enif_make_map_put(env, iter, kAtomMu30, evision_from(env, m.mu30), &iter) &&
        enif_make_map_put(env, iter, kAtomMu21, evision_from(env, m.mu21), &iter) &&
        enif_make_map_put(env, iter, kAtomMu12, evision_from(env, m.mu12), &iter) &&
        enif_make_map_put(env, iter, kAtomMu03, evision_from(env, m.mu03), &iter) &&

        enif_make_map_put(env, iter, kAtomNu20, evision_from(env, m.nu20), &iter) &&
        enif_make_map_put(env, iter, kAtomNu11, evision_from(env, m.nu11), &iter) &&
        enif_make_map_put(env, iter, kAtomNu02, evision_from(env, m.nu02), &iter) &&

        enif_make_map_put(env, iter, kAtomNu30, evision_from(env, m.nu30), &iter) &&
        enif_make_map_put(env, iter, kAtomNu21, evision_from(env, m.nu21), &iter) &&
        enif_make_map_put(env, iter, kAtomNu12, evision_from(env, m.nu12), &iter) &&
        enif_make_map_put(env, iter, kAtomNu03, evision_from(env, m.nu03), &iter)
    ) {
        return ret;
    } else {
        return evision::nif::error(env, "error: Moments: map");
    }
}

template <typename Tp>
struct evisionVecConverter;

static bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<int64_t>& value, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return true;
    }

    return evision::nif::get_list(env, obj, value);
}

static bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<int>& value, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return true;
    }

    return evision::nif::get_list(env, obj, value);
}

template <typename Tp>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<Tp>& value, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        if (info.name != nullptr && strncmp(info.name, "netInputShape", 13) == 0) {
            return false;
        }
        return info.has_default || info.outputarg;
    }
    return evisionVecConverter<Tp>::to(env, obj, value, info);
}

template <typename Tp>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const std::vector<Tp>& value)
{
    return evisionVecConverter<Tp>::from(env, value);
}

template <typename Tp>
static bool evision_to_generic_vec(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<Tp>& value, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        if (info.name != nullptr && strncmp(info.name, "netInputShape", 13) == 0) {
            return false;
        }
        return info.has_default || info.outputarg;
    }

    if (!enif_is_list(env, obj))
    {
        failmsg(env, "Can't parse '%s'. Input argument is not a list", info.name);
        return false;
    }
    unsigned n = 0;
    enif_get_list_length(env, obj, &n);
    value.resize(n);

    std::vector<ERL_NIF_TERM> cells;
    ERL_NIF_TERM head, tail, arr = obj;
    for (size_t i = 0; i < n; i++) {
        if (enif_get_list_cell(env, arr, &head, &tail)) {
            arr = tail;
            cells.push_back(head);
        } else {
            return false;
        }
    }

    for (size_t i = 0; i < n; i++)
    {
        if (!evision_to(env, cells[i], value[i], info))
        {
            failmsg(env, "Can't parse '%s'. Sequence item with index %lu has a wrong type", info.name, i);
            return false;
        }
    }
    if (info.name != nullptr && strncmp(info.name, "netInputShape", 13) == 0) {
        return false;
    }
    return true;
}

template <>
inline bool evision_to_generic_vec(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<cv::Rect2d>& value, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return true;
    }

    if (!enif_is_list(env, obj)) {
        ErlNifBinary data;
        if (enif_inspect_binary(env, obj, &data)) {
            const size_t rect2d_size = sizeof(double) * 4;
            if (data.size > 0 && data.size % rect2d_size == 0) {
                size_t num_elements = data.size / rect2d_size;
                value.resize(num_elements);
                memcpy(value.data(), data.data, data.size);
                return true;
            }
        }
        return false;
    }
    unsigned n = 0;
    if (!enif_get_list_length(env, obj, &n)) {
        return false;
    }
    // printf("arg: %s, list length: %d\r\n", info.name, n);

    value.resize(n);
    ERL_NIF_TERM head, tail, arr = obj;
    for (size_t i = 0; i < n; i++)
    {
        cv::Rect2d inner_val;
        if (!enif_get_list_cell(env, arr, &head, &tail)) {
            return false;
        }

        if (!evision_to(env, head, inner_val, info)) {
            return false;
        }

        value[i] = inner_val;
        arr = tail;
    }

    return true;
}

template <>
inline bool evision_to_generic_vec(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<cv::Rect>& value, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return true;
    }

    if (!enif_is_list(env, obj)) {
        ErlNifBinary data;
        if (enif_inspect_binary(env, obj, &data)) {
            const size_t rect_size = sizeof(int) * 4;
            if (data.size > 0 && data.size % rect_size == 0) {
                size_t num_elements = data.size / rect_size;
                value.resize(num_elements);
                memcpy(value.data(), data.data, data.size);
                return true;
            }
        }
        return false;
    }
    unsigned n = 0;
    if (!enif_get_list_length(env, obj, &n)) {
        return false;
    }
    // printf("arg: %s, list length: %d\r\n", info.name, n);

    value.resize(n);
    ERL_NIF_TERM head, tail, arr = obj;
    for (size_t i = 0; i < n; i++)
    {
        cv::Rect inner_val;
        if (!enif_get_list_cell(env, arr, &head, &tail)) {
            return false;
        }

        if (!evision_to(env, head, inner_val, info)) {
            return false;
        }

        value[i] = inner_val;
        arr = tail;
    }

    return true;
}

template <>
inline bool evision_to_generic_vec(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<std::vector<int>>& value, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        if (info.name != nullptr && strncmp(info.name, "netInputShape", 13) == 0) {
            return false;
        }
        return true;
    }

    if (!enif_is_list(env, obj)) {
        return false;
    }
    unsigned n = 0;
    if (!enif_get_list_length(env, obj, &n)) {
        return false;
    }
    // printf("arg: %s, list length: %d\r\n", info.name, n);

    value.resize(n);
    ERL_NIF_TERM head, tail, arr = obj;
    for (size_t i = 0; i < n; i++)
    {
        std::vector<int> inner_val;
        if (!enif_get_list_cell(env, arr, &head, &tail)) {
            return false;
        }

        if (!enif_is_list(env, head)) {
            return false;
        }

        unsigned inner_n = 0;
        if (!enif_get_list_length(env, head, &inner_n)) {
            return false;
        }
        inner_val.resize(inner_n);
        ERL_NIF_TERM inner_head, inner_tail, inner_arr = head;
        for (size_t j = 0; j < inner_n; ++j) {
            if (!enif_get_list_cell(env, inner_arr, &inner_head, &inner_tail)) {
                return false;
            }
            int val;
            if (!enif_get_int(env, inner_head, &val)) {
                return false;
            }
            inner_val[j] = val;
            inner_arr = inner_tail;
        }

        value.push_back(inner_val);
        arr = tail;
    }

    return true;
}

template<> inline bool evision_to_generic_vec(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<bool>& value, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return true;
    }

    const ERL_NIF_TERM *terms;
    int length;
    if (enif_get_tuple(env, obj, &length, &terms)) {
        const size_t n = static_cast<size_t>(length);
        value.resize(n);
        for (size_t i = 0; i < n; i++)
        {
            bool elem{};
            if (!evision_to(env, terms[i], elem, info))
            {
                failmsg(env, "Can't parse '%s'. Sequence item with index %lu has a wrong type", info.name, i);
                return false;
            }
            value[i] = elem;
        }
        
        return true;
    }

    // also try parsing from list
    if (enif_is_list(env, obj))
    {
        unsigned n = 0;
        enif_get_list_length(env, obj, &n);
        value.resize(n);

        std::vector<ERL_NIF_TERM> cells;
        ERL_NIF_TERM head, tail, arr = obj;
        for (size_t i = 0; i < n; i++) {
            if (enif_get_list_cell(env, arr, &head, &tail)) {
                arr = tail;
                cells.push_back(head);
            } else {
                return false;
            }
        }

        for (size_t i = 0; i < n; i++)
        {
            bool elem{};
            if (!evision_to(env, cells[i], elem, info))
            {
                failmsgp(env, "Can't parse '%s'. Sequence item with index %lu has a wrong type", info.name, i);
                return false;
            }
            value[i] = elem;
        }

        return true;
    }

    failmsgp(env, "Can't parse '%s' to a generic array."
            "Input argument is not a tuple or a list",
            info.name);
    return false;
}


template <typename Tp>
static ERL_NIF_TERM evision_from_generic_vec(ErlNifEnv *env, const std::vector<Tp>& value)
{
    size_t n = static_cast<size_t>(value.size());
    ERL_NIF_TERM * arr = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * n);
    for (size_t i = 0; i < n; i++)
    {
        arr[i] = evision_from(env, value[i]);
    }
    ERL_NIF_TERM ret = enif_make_list_from_array(env, arr, n);
    enif_free(arr);
    return ret;
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const cv::Vec<float, 6>& p)
{
    ERL_NIF_TERM * arr = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * 6);
    for (size_t i = 0; i < 6; i++)
    {
        arr[i] = evision_from(env, p[i]);
    }
    ERL_NIF_TERM ret = enif_make_list_from_array(env, arr, 6);
    enif_free(arr);
    return ret;
}

template<>
bool evision_to(ErlNifEnv * env, ERL_NIF_TERM o, std::vector<Range>& p, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, o)) {
        return info.has_default;
    }

    if (!enif_is_list(env, o)) {
        return false;
    }

    unsigned n = 0;
    enif_get_list_length(env, o, &n);

    ERL_NIF_TERM head, tail, obj = o;
    size_t i = 0;
    while (i < n) {
        if (enif_get_list_cell(env, obj, &head, &tail)) {
            Range item;
            if (!evision_to(env, head, item, info)) {
                return false;
            }
            p.push_back(item);
            obj = tail;
            i++;
        } else {
            return false;
        }
    }

    return true;
}

template<>
bool evision_to(ErlNifEnv * env, ERL_NIF_TERM o, cv::Vec<float, 6>& p, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, o)) {
        return true;
    }

    if (enif_is_tuple(env, o)) {
        int n = 0;
        const ERL_NIF_TERM * terms;
        enif_get_tuple(env, o, &n, &terms);

        if (n != 6) {
            return false;
        }

        ERL_NIF_TERM head, tail, obj = o;
        int i = 0;
        while (i < n) {
            if (enif_get_list_cell(env, obj, &head, &tail)) {
                if (!evision_to(env, head, p[i], info))
                {
                    failmsg(env, "Can't parse '%s'. Sequence item with index %lu has a wrong type", info.name, i);
                    return false;
                }

                obj = tail;
                i++;
            } else {
                return false;
            }
        }
        return true;
    }

    if (enif_is_list(env, o)) {
        unsigned n = 0;
        enif_get_list_length(env, o, &n);
        if (n != 6) {
            return false;
        }

        ERL_NIF_TERM head, tail, obj = o;
        size_t i = 0;
        while (i < n) {
            if (enif_get_list_cell(env, obj, &head, &tail)) {
                if (!evision_to(env, head, p[i], info))
                {
                    failmsg(env, "Can't parse '%s'. Sequence item with index %lu has a wrong type", info.name, i);
                    return false;
                }

                obj = tail;
                i++;
            } else {
                return false;
            }
        }

        return true;
    }

    return false;
}

template<> inline ERL_NIF_TERM evision_from_generic_vec(ErlNifEnv *env, const std::vector<bool>& value)
{
    size_t n = static_cast<size_t>(value.size());
    ERL_NIF_TERM * arr = (ERL_NIF_TERM *)malloc(sizeof(ERL_NIF_TERM) * n);
    for (size_t i = 0; i < n; i++)
    {
        bool elem = value[i];
        if (elem) arr[i] = kAtomTrue;
        else arr[i] = kAtomFalse;
    }
    ERL_NIF_TERM ret = enif_make_list_from_array(env, arr, n);
    free(arr);
    return ret;
}


template<std::size_t I = 0, typename... Tp>
inline typename std::enable_if<I == sizeof...(Tp), void>::type
convert_to_erlang_tuple(ErlNifEnv *env, const std::tuple<Tp...>&, std::vector<ERL_NIF_TERM>&) {  }

template<std::size_t I = 0, typename... Tp>
inline typename std::enable_if<I < sizeof...(Tp), void>::type
convert_to_erlang_tuple(ErlNifEnv *env, const std::tuple<Tp...>& cpp_tuple, std::vector<ERL_NIF_TERM>& erl_tuple)
{
    ERL_NIF_TERM item = evision_from(std::get<I>(cpp_tuple));
    erl_tuple.push_back(item);
    convert_to_erlang_tuple<I + 1, Tp...>(cpp_tuple, erl_tuple);
}


template<typename... Ts>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const std::tuple<Ts...>& cpp_tuple)
{
    std::vector<ERL_NIF_TERM> erl_tuple;
    convert_to_erlang_tuple(env, cpp_tuple, erl_tuple);
    ERL_NIF_TERM * terms = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * erl_tuple.size());
    for (size_t i = 0; i < erl_tuple.size(); i++) {
        terms[i] = erl_tuple[i];
    }
    ERL_NIF_TERM ret = enif_make_list_from_array(env, terms, erl_tuple.size());
    enif_free(terms);
    return ret;
}

template <typename Tp, size_t N>
static ERL_NIF_TERM evision_from_generic_vec(ErlNifEnv *env, const cv::Vec<Tp, N>& value)
{
    size_t n = static_cast<size_t>(value.size());
    ERL_NIF_TERM * arr = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * N);
    for (size_t i = 0; i < n; i++)
    {
        arr[i] = evision_from(env, value[i]);
    }
    ERL_NIF_TERM ret = enif_make_list_from_array(env, arr, N);
    enif_free(arr);
    return ret;
}

template <typename Tp>
struct evisionVecConverter
{
    typedef typename std::vector<Tp>::iterator VecIt;

    static bool to(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<Tp>& value, const ArgInfo& info)
    {
        return evision_to_generic_vec(env, obj, value, info);
    }

    static ERL_NIF_TERM from(ErlNifEnv *env, const std::vector<Tp>& value)
    {
        return evision_from_generic_vec(env, value);
    }

};

static int OnError(int status, const char *func_name, const char *err_msg, const char *file_name, int line, void *userdata)
{
    // todo:evision on_error
//    PyGILState_STATE gstate;
//    gstate = PyGILState_Ensure();
//
//    PyObject *on_error = (PyObject*)userdata;
//    PyObject *args = Py_BuildValue("isssi", status, func_name, err_msg, file_name, line);
//
//    PyObject *r = PyObject_Call(on_error, args, NULL);
//    if (r == NULL) {
//        PyErr_Print();
//    } else {
//        Py_DECREF(r);
//    }
//
//    Py_DECREF(args);
//    PyGILState_Release(gstate);

    return 0; // The return value isn't used
}

static ERL_NIF_TERM evisionRedirectError(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // const char *keywords[] = { "on_error", NULL };
    ERL_NIF_TERM on_error = argv[0];

    // todo:evision check callback
    if ((!evision::nif::check_nil(env, on_error)))  {
        return evision::nif::atom(env, "not implemented");
    }
    return evision::nif::atom(env, "not implemented");

//    // Keep track of the previous handler parameter, so we can decref it when no longer used
//    static PyObject* last_on_error = NULL;
//    if (last_on_error) {
//        Py_DECREF(last_on_error);
//        last_on_error = NULL;
//    }
//
//    if (on_error == Py_None) {
//        ERRWRAP2(redirectError(NULL));
//    } else {
//        last_on_error = on_error;
//        Py_INCREF(last_on_error);
//        ERRWRAP2(redirectError(OnError, last_on_error));
//    }
//    Py_RETURN_NONE;
}

static void OnMouse(int event, int x, int y, int flags, void* param)
{
    // todo:evision on_mouse
//    PyGILState_STATE gstate;
//    gstate = PyGILState_Ensure();
//
//    PyObject *o = (PyObject*)param;
//    PyObject *args = Py_BuildValue("iiiiO", event, x, y, flags, PyTuple_GetItem(o, 1));
//
//    PyObject *r = PyObject_Call(PyTuple_GetItem(o, 0), args, NULL);
//    if (r == NULL)
//        PyErr_Print();
//    else
//        Py_DECREF(r);
//    Py_DECREF(args);
//    PyGILState_Release(gstate);
}

#ifdef HAVE_OPENCV_HIGHGUI
static ERL_NIF_TERM evisionSetMouseCallback(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    // const char *keywords[] = { "window_name", "on_mouse", "param", NULL };
//    char* name;
//    ERL_NIF_TERM on_mouse;
//    ERL_NIF_TERM param;
//
//    if (!evision::nif::parse_arg(env, argc, argv, (char**)keywords, "sO|O", &name, &on_mouse, &param))
//        return evision::nif::atom(env, "not implemented");
    return evision::nif::atom(env, "not implemented");

    // todo: check callback
//    if (!PyCallable_Check(on_mouse)) {
//        PyErr_SetString(PyExc_TypeError, "on_mouse must be callable");
//        return NULL;
//    }
//    if (param == NULL) {
//        param = Py_None;
//    }
//    PyObject* py_callback_info = Py_BuildValue("OO", on_mouse, param);
//    static std::map<std::string, PyObject*> registered_callbacks;
//    std::map<std::string, PyObject*>::iterator i = registered_callbacks.find(name);
//    if (i != registered_callbacks.end())
//    {
//        Py_DECREF(i->second);
//        i->second = py_callback_info;
//    }
//    else
//    {
//        registered_callbacks.insert(std::pair<std::string, PyObject*>(std::string(name), py_callback_info));
//    }
//    ERRWRAP2(setMouseCallback(name, OnMouse, py_callback_info));
//    Py_RETURN_NONE;
}
#endif

static void OnChange(int pos, void *param)
{
    // todo:evision on_change
//    PyGILState_STATE gstate;
//    gstate = PyGILState_Ensure();
//
//    PyObject *o = (PyObject*)param;
//    PyObject *args = Py_BuildValue("(i)", pos);
//    PyObject *r = PyObject_Call(PyTuple_GetItem(o, 0), args, NULL);
//    if (r == NULL)
//        PyErr_Print();
//    else
//        Py_DECREF(r);
//    Py_DECREF(args);
//    PyGILState_Release(gstate);
}

#ifdef HAVE_OPENCV_HIGHGUI
// workaround for #20408, use nullptr, set value later
static int _createTrackbar(const String &trackbar_name, const String &window_name, int value, int count,
                    TrackbarCallback onChange, ERL_NIF_TERM erl_callback_info)
{
    // todo:evision _createTrackbar
//    int n = createTrackbar(trackbar_name, window_name, NULL, count, onChange, py_callback_info);
//    setTrackbarPos(trackbar_name, window_name, value);
//    return n;
    return 0;
}
static ERL_NIF_TERM evisionCreateTrackbar(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
//    ERL_NIF_TERM on_change;
//    char* trackbar_name;
//    char* window_name;
//    int value;
//    int count;
//
//    // todo:evision evisionCreateTrackbar
//    if (!evision::nif::parse_arg(env, argc, argv, nullptr, "ssiiO", &trackbar_name, &window_name, &value, &count, &on_change))
//        return evision::nif::atom(env, "not implemented");
    return evision::nif::atom(env, "not implemented");
//
//    if (!PyCallable_Check(on_change)) {
//        PyErr_SetString(PyExc_TypeError, "on_change must be callable");
//        return NULL;
//    }
//    PyObject* py_callback_info = Py_BuildValue("OO", on_change, Py_None);
//    std::string name = std::string(window_name) + ":" + std::string(trackbar_name);
//    static std::map<std::string, PyObject*> registered_callbacks;
//    std::map<std::string, PyObject*>::iterator i = registered_callbacks.find(name);
//    if (i != registered_callbacks.end())
//    {
//        Py_DECREF(i->second);
//        i->second = py_callback_info;
//    }
//    else
//    {
//        registered_callbacks.insert(std::pair<std::string, PyObject*>(name, py_callback_info));
//    }
//    ERRWRAP2(_createTrackbar(trackbar_name, window_name, value, count, OnChange, py_callback_info));
//    Py_RETURN_NONE;
}

static void OnButtonChange(int state, void *param)
{
    // todo:evision OnButtonChange
//    PyGILState_STATE gstate;
//    gstate = PyGILState_Ensure();
//
//    PyObject *o = (PyObject*)param;
//    PyObject *args;
//    if(PyTuple_GetItem(o, 1) != NULL)
//    {
//        args = Py_BuildValue("(iO)", state, PyTuple_GetItem(o,1));
//    }
//    else
//    {
//        args = Py_BuildValue("(i)", state);
//    }
//
//    PyObject *r = PyObject_Call(PyTuple_GetItem(o, 0), args, NULL);
//    if (r == NULL)
//        PyErr_Print();
//    else
//        Py_DECREF(r);
//    Py_DECREF(args);
//    PyGILState_Release(gstate);
}

static ERL_NIF_TERM evisionCreateButton(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

//    const char* keywords[] = {"buttonName", "onChange", "userData", "buttonType", "initialButtonState", NULL};
//    ERL_NIF_TERM on_change;
//    ERL_NIF_TERM userdata = evision::nif::atom(env, "nil");
//    char* button_name;
//    int button_type = 0;
//    int initial_button_state = 0;
//
//    if (!evision::nif::parse_arg(env, argc, argv, (char**)keywords, "sO|Oii", &button_name, &on_change, &userdata, &button_type, &initial_button_state))
//        return evision::nif::atom(env, "not implemented");
    return evision::nif::atom(env, "not implemented");

    // todo: check callback
//    if (!PyCallable_Check(on_change)) {
//        PyErr_SetString(PyExc_TypeError, "onChange must be callable");
//        return NULL;
//    }
//    if (userdata == NULL) {
//        userdata = Py_None;
//    }
//
//    PyObject* py_callback_info = Py_BuildValue("OO", on_change, userdata);
//    std::string name(button_name);
//
//    static std::map<std::string, PyObject*> registered_callbacks;
//    std::map<std::string, PyObject*>::iterator i = registered_callbacks.find(name);
//    if (i != registered_callbacks.end())
//    {
//        Py_DECREF(i->second);
//        i->second = py_callback_info;
//    }
//    else
//    {
//        registered_callbacks.insert(std::pair<std::string, PyObject*>(name, py_callback_info));
//    }
//    ERRWRAP2(createButton(button_name, OnButtonChange, py_callback_info, button_type, initial_button_state != 0));
//    Py_RETURN_NONE;
}
#endif

///////////////////////////////////////////////////////////////////////////////////////

static int convert_to_char(ErlNifEnv *env, ERL_NIF_TERM o, char **dst, const ArgInfo& info)
{
    std::string str;
    if (evision::nif::get(env, o, str))
    {
        *dst = (char *)str.c_str();
        return 1;
    }
    (*dst) = 0;
    return failmsg(env, "Expected single character string for argument '%s'", info.name);
}

static int convert_to_char(ErlNifEnv *env, ERL_NIF_TERM o, char *dst, const ArgInfo& info)
{
    int i32;
    if (evision::nif::get(env, o, &i32))
    {
        *dst = (char)i32;
        return 1;
    }
    (*dst) = 0;
    return failmsg(env, "Expected a char [-128, 127] '%s'", info.name);
}

#include "evision_generated_enums.h"
#define CV_ERL_TYPE(WNAME, NAME, STORAGE, SNAME, _1, _2, MODULE_NAME) CV_ERL_TYPE_DECLARE_DYNAMIC(WNAME, NAME, STORAGE, SNAME, MODULE_NAME)
#include "evision_generated_types.h"
#undef CV_ERL_TYPE
#include "evision_custom_headers.h"

#include "evision_generated_types_content.h"
#include "evision_generated_funcs.h"

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
