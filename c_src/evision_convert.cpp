// Conversions and error messages that are too large to compile into every translation unit.
#include "evision.hpp"

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

int failmsg(ErlNifEnv *env, const char *fmt, ...)
{
    char str[1000];

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(str, sizeof(str), fmt, ap);
    va_end(ap);

    emit_failmsg(env, "TypeError", str);
    return 0;
}

ERL_NIF_TERM failmsgp(ErlNifEnv *env, const char *fmt, ...)
{
    char str[1000];

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(str, sizeof(str), fmt, ap);
    va_end(ap);

    emit_failmsg(env, "TypeError", str);
    return evision::nif::error(env, str);
}

void evision_conversion_failed(ErlNifEnv *env, const ArgInfo& info, const char *what)
{
    if (what) {
        failmsgp(env, "Conversion error: %s, what: %s", info.name, what);
    } else {
        failmsgp(env, "Conversion error: %s", info.name);
    }
}

// special case, when the converter needs full ArgInfo structure
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM o, Mat& m, const ArgInfo& info)
{
    if(evision::nif::check_nil(env, o)) {
        return true;
    }

    evision_res<cv::Mat *> * in_res;
    if (enif_get_resource(env, o, evision_res<cv::Mat *>::type, (void **)&in_res))
    {
        if (in_res->val) {
            // By default the source matrix is deep-copied, so the caller's
            // (immutable) matrix is never modified -- even if this NIF writes
            // to the argument in place.
            //
            // Read-only inputs can share OpenCV-owned storage. Binary-backed
            // data may only be borrowed by calls that never retain or return a
            // view of it; the NIF argument keeps the binary alive for the call.
            // OpenCV kernels may use typed loads, so an unaligned sub-binary
            // still needs an aligned copy even for a read-only operation.
            if (info.input_only && (in_res->in_buf == nullptr ||
                (info.borrowed_input &&
                 (uintptr_t)in_res->val->data % in_res->val->elemSize1() == 0))) {
                m = *in_res->val;
            } else {
                in_res->val->copyTo(m);
            }
            return true;
        }
        return false;
    }

    // Nx.tensor
    if (enif_is_map(env, o))
    {
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
        }
        else if (enif_is_identical(type_term, kAtomS64)) {
            nx_tensor_type = type = CV_64S;
            originalElemSize = 8;
        }
        else if (enif_is_identical(type_term, kAtomU32)) {
            nx_tensor_type = type = CV_32U;
            originalElemSize = 4;
        }
        else if (enif_is_identical(type_term, kAtomU64)) {
            nx_tensor_type = type = CV_64U;
            originalElemSize = 8;
        }
        else if (enif_is_identical(type_term, kAtomBF16)) {
            nx_tensor_type = type = CV_16BF;
            originalElemSize = 2;
        }
        else if (enif_is_identical(type_term, kAtomF8E4M3FN)) {
            nx_tensor_type = type = CV_8F_E4M3FN;
            originalElemSize = 1;
        } else {
            return false;
        }

#ifndef CV_MAX_DIM
        const int CV_MAX_DIM = 32;
#endif 

        if (ndims > CV_MAX_DIM) {
            return false;
        }
        std::vector<uint64_t> _sizes(ndims);
        if (is_number) {
            _sizes[0] = 1;
        } else {
            for (int dim_i = 0; dim_i < ndims; dim_i++) {
                ErlNifUInt64 u64;
                ERL_NIF_TERM shape_i = shapes[dim_i];
                if (enif_get_uint64(env, shape_i, &u64) && u64 > 0 &&
                    u64 <= (uint64_t)std::numeric_limits<int>::max()) {
                    _sizes[dim_i] = u64;
                } else {
                    return false;
                }
            }
        }
        
        // A map can be forged without going through Nx or the Elixir wrappers.
        // Verify the complete byte extent before any scalar read or Mat view.
        size_t expected_bytes = (size_t)originalElemSize;
        for (uint64_t dim : _sizes) {
            if (!evision_checked_mul_size(expected_bytes, (size_t)dim, expected_bytes)) return false;
        }
        if (expected_bytes != data.size ||
            expected_bytes > (size_t)std::numeric_limits<ptrdiff_t>::max()) return false;

        // see https://github.com/opencv/opencv/issues/24057
        // e.g., Nx.tensor([1,2,3,4]), Nx.tensor(1)
        if ( ( info.arithm_op_src ) && ( ndims == 1 ) && ( _sizes[0] <= 4 ) )
        {
            const int sz  = (int)_sizes[0]; // Real Data Length(1, 2, 3 or 4)
            const int sz2 = 4;       // Scalar has 4 elements.
            m = Mat::zeros(sz2, 1, CV_64F);

            const char *base_ptr = (const char *)data.data;
            for(int i = 0; i < sz; i++ )
            {
                const void * oi = static_cast<const void *>(base_ptr + originalElemSize * i);
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
                    m.at<double>(i) = _do_cast_f16<double>(_do_cast_type<uint16_t, uint16_t>((const uint16_t *)oi));
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
                case CV_64S:
                    m.at<double>(i) = _do_cast_type<int64_t, double>((const int64_t *)oi);
                    break;
                case CV_64U:
                    m.at<double>(i) = _do_cast_type<uint64_t, double>((const uint64_t *)oi);
                    break;
                case CV_32U:
                    m.at<double>(i) = _do_cast_type<uint32_t, double>((const uint32_t *)oi);
                    break;
                case CV_16BF:
                    m.at<double>(i) = _do_cast_bf16<double>(_do_cast_type<uint16_t, uint16_t>((const uint16_t *)oi));
                    break;
                case CV_8F_E4M3FN:
                    m.at<double>(i) = cv::fp8_t::decodeLUT()[_do_cast_type<uint8_t, uint8_t>((const uint8_t *)oi)];
                    break;
                default:
                    break;
                }
            }
            if (is_number) {
                // Normally cv.XXX(x) means cv.XXX( (x, 0., 0., 0.) );
                // However  cv.add(mat,x) means cv::add(mat, (x,x,x,x) ).
                m.at<double>(1) = m.at<double>(2) = m.at<double>(3) = m.at<double>(0);
            }
            return true;
        }

        size_t elemsize = CV_ELEM_SIZE1(type);
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

        bool ismultichannel = ndims == 3 && _sizes[2] <= CV_CN_MAX && !info.nd_mat;
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

            elemsize = CV_ELEM_SIZE(type);
        }

        void * mat_data = data.data;

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

        // handle degenerate case
        // FIXIT: Don't force 1D for Scalars
        if (ndims == 0) {
            size[ndims] = 1;
            step[ndims] = elemsize;
            ndims++;
        }

        Mat casted(ndims, size, type, mat_data, step);
        // Nx strides are derived from its contiguous binary above. Borrow only
        // for calls that cannot retain this data.
        if (info.borrowed_input &&
            (uintptr_t)mat_data % CV_ELEM_SIZE1(type) == 0) {
            m = casted;
        } else {
            casted.copyTo(m);
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

bool evision_to(ErlNifEnv *env, ERL_NIF_TERM o, cv::UMat& m, const ArgInfo& info)
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

ERL_NIF_TERM evision_from_mat(ErlNifEnv *env, const Mat& m)
{
    if (!m.data) {
        return evision::nif::error(env, "empty matrix");
    }

    evision_res<cv::Mat *> * res;
    if (alloc_resource(&res)) {
        try {
            // Share the output's refcounted storage, with exception-safe ownership.
            res->val = new cv::Mat(m);
        } catch (...) {
            enif_release_resource(res);
            throw;
        }
    } else {
        return evision::nif::error(env, "out of memory");
    }

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);

    return _evision_make_mat_resource_into_map(env, m, ret);
}
