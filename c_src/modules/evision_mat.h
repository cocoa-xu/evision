#ifndef EVISION_OPENCV_MAT_H
#define EVISION_OPENCV_MAT_H

#include <erl_nif.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#if defined(_WIN32) || defined(WINRT) || defined(_WIN32_WCE) || defined(__WIN32__) || defined(_MSC_VER)
#include <io.h>
#else
#include <unistd.h>
#endif

#include "../nif_utils.hpp"
#include "evision_mat_utils.hpp"
#include "evision_backend/backend.h"
#include "evision_mat_api.h"

using namespace evision::nif;

// @evision c: mat_empty,evision_cv_mat_empty,0
// @evision nif: def mat_empty(), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_empty(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    evision_res<cv::Mat *> * res;
    if (alloc_resource(&res)) {
        res->val = new cv::Mat();
    } else {
        return evision::nif::error(env, "out of memory");
    }

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);

    return _evision_make_mat_resource_into_map(env, *res->val, ret);
}

// @evision c: mat_to_pointer,evision_cv_mat_to_pointer,1
// @evision nif: def mat_to_pointer(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_to_pointer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);
    std::string pointer_kind;

    {
        evision_res<cv::Mat *> * res;
        if( enif_get_resource(env, evision_get_kw(env, erl_terms, "img"), evision_res<cv::Mat *>::type, (void **)&res) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "mode"), pointer_kind, ArgInfo("mode", 1))) {
            std::vector<unsigned char> pointer_vec;
            std::uintptr_t ptr = (std::uintptr_t)res->val->data;
            if (pointer_kind == "local") {
                unsigned char* bytePtr = reinterpret_cast<unsigned char*>(&ptr);
                for (size_t i = 0; i < sizeof(void*); i++) {
                    pointer_vec.push_back(bytePtr[i]);
                }
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

// @evision c: mat_type,evision_cv_mat_type,1
// @evision nif: def mat_type(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_type(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            return _evision_get_mat_type(env, img);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_as_type,evision_cv_mat_as_type,1
// @evision nif: def mat_as_type(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_as_type(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;
        std::string t;
        int l = 0;
        int type;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0))) {
            if (get_binary_type(t, l, 0, type)) {
                Mat ret;
                img.convertTo(ret, type);
                return evision_from(env, ret);
            } else {
                return evision::nif::error(env, "unsupported target type");
            }
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_shape,evision_cv_mat_shape,1
// @evision nif: def mat_shape(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_shape(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        // const char *keywords[] = {"img", NULL};
        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            ERL_NIF_TERM ret = _evision_get_mat_shape(env, img);
            return ret;
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_roi,evision_cv_mat_roi,1
// @evision nif: def mat_roi(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_roi(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    int error_flag = false;
    Mat img;
    if (evision_to_safe(env, evision_get_kw(env, erl_terms, "mat"), img, ArgInfo("mat", 0)))
    {
        {
            // Mat operator()( const Rect& roi ) const;
            cv::Rect2i rect;
            if (erl_terms.find("rect") != erl_terms.end() &&
                evision_to_safe(env, evision_get_kw(env, erl_terms, "rect"), rect, ArgInfo("rect", 0))) {
                Mat ret;
                ERRWRAP2(ret = img(rect), env, error_flag, error_term);
                if (!error_flag) {
                    return evision_from(env, ret.clone());
                }
            }
        }

        {
            // Mat operator()( Range rowRange, Range colRange ) const;
            cv::Range rowRange, colRange;
            if (erl_terms.find("rowRange") != erl_terms.end() &&
                erl_terms.find("colRange") != erl_terms.end() &&
                evision_to_safe(env, evision_get_kw(env, erl_terms, "rowRange"), rowRange, ArgInfo("rowRange", 0)) &&
                evision_to_safe(env, evision_get_kw(env, erl_terms, "colRange"), colRange, ArgInfo("colRange", 0))) {
                Mat ret;
                ERRWRAP2(ret = img(rowRange, colRange), env, error_flag, error_term);
                if (!error_flag) {
                    return evision_from(env, ret.clone());
                }
            }
        }

        {
            // Mat operator()(const std::vector<Range>& ranges) const;
            std::vector<cv::Range> ranges;
            if (erl_terms.find("ranges") != erl_terms.end() &&
                evision_to_safe(env, evision_get_kw(env, erl_terms, "ranges"), ranges, ArgInfo("ranges", 0))) {
                Mat ret;
                int dims = img.size.dims();
                if ((int)ranges.size() == dims + 1) {
                    int img_channels = img.channels();
                    Range last = ranges[dims];
                    ranges.pop_back();

                    // deal with cv::Range::all()
                    if (last == cv::Range::all()) {
                        last.start = 0;
                        last.end = img_channels;
                    }

                    // if definitely out of range
                    if (last.start > img_channels - 1 || last.end < 0 || last.start < 0 || last.end > img_channels) {
                        char msg[128] = {'\0'};
                        if (enif_snprintf(msg, 127, "index %d is out of bounds for axis %d with size %d", last.start, ranges.size(), img_channels)) {
                            return evision::nif::error(env, msg);
                        } else {
                            return evision::nif::error(env, "index out of bounds");
                        }
                    }

                    if (last.end <= last.start) {
                        return evision_cv_mat_empty(env, 0, argv);
                    }

                    if (last.start == 0 && last.end == img_channels) {
                        ERRWRAP2(ret = img(ranges), env, error_flag, error_term);
                        ret = ret.clone();
                    } else {
                        // because we are taking channel in [start, end)
                        // if start == end, we would still have 1 channel to extract
                        // this follows the convention in elixir as 0..0 is effectively [0]
                        std::vector<cv::Mat> by_channel(last.end - last.start);
                        size_t by_channel_index = 0;
                        for (int channel_index = last.start; channel_index < last.end; channel_index++) {
                            ERRWRAP2(cv::extractChannel(img, by_channel[by_channel_index], channel_index), env, error_flag, error_term);
                            if (error_flag) break;

                            ERRWRAP2(by_channel[by_channel_index] = by_channel[by_channel_index](ranges), env, error_flag, error_term);
                            if (error_flag) break;

                            by_channel_index++;
                        }

                        if (!error_flag) {
                            ERRWRAP2(cv::merge(by_channel, ret), env, error_flag, error_term);
                        }
                    }
                } else {
                    ERRWRAP2(ret = img(ranges), env, error_flag, error_term);
                    ret = ret.clone();
                }

                if (!error_flag) {
                    return evision_from(env, ret);
                }
            }
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

/// `matrix` and `patch` should be contiguous.
void mat_update_roi(cv::Mat& matrix, cv::Mat& patch, 
    std::vector<cv::Range>& ranges, size_t elem_size,
    size_t step_size, size_t from_step_size,
    int dim_index, size_t base_offset, size_t from_offset)
{
    if (dim_index >= matrix.size.dims()) return;

    step_size /= matrix.size.p[dim_index];
    from_step_size /= patch.size.p[dim_index];

    auto& range = ranges[dim_index];

    if (step_size == elem_size) {
        uint8_t* data = matrix.data;
        uint8_t* from = patch.data;
        memcpy(
            (uint64_t *)(((uint64_t)(uint64_t *)(data)) + base_offset + range.start * elem_size),
            (uint64_t *)(((uint64_t)(uint64_t *)(from)) + from_offset),
            elem_size * (range.end - range.start)
        );
    } else {
        for (int pos = range.start, from_pos = 0; pos < range.end; pos++, from_pos++) {
            mat_update_roi(matrix, patch, ranges, elem_size, step_size, from_step_size, dim_index + 1, base_offset + step_size * pos, from_offset + from_step_size * from_pos);
        }
    }
}

// @evision c: mat_update_roi,evision_cv_mat_update_roi,1
// @evision nif: def mat_update_roi(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_update_roi(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    int error_flag = false;
    Mat mat, with_mat;
    std::vector<cv::Range> ranges;

    if (erl_terms.find("mat") != erl_terms.end() &&
        erl_terms.find("with_mat") != erl_terms.end() &&
        erl_terms.find("ranges") != erl_terms.end() &&
        evision_to_safe(env, evision_get_kw(env, erl_terms, "mat"), mat, ArgInfo("mat", 0)) &&
        evision_to_safe(env, evision_get_kw(env, erl_terms, "with_mat"), with_mat, ArgInfo("with_mat", 0)) &&
        evision_to_safe(env, evision_get_kw(env, erl_terms, "ranges"), ranges, ArgInfo("ranges", 0)))
    {
        if (mat.type() != with_mat.type()) {
            error_flag = true;
            error_term = evision::nif::error(env, "Cannot update roi: type mismatch");
        } else if (mat.dims != with_mat.dims) {
            error_flag = true;
            error_term = evision::nif::error(env, "Cannot update roi: dimension mismatch");
        } else {
            if (!mat.isContinuous()) mat = mat.clone();
            if (!with_mat.isContinuous()) with_mat = with_mat.clone();
            size_t step_size = mat.elemSize();
            for (int i = 0; i < mat.size.dims(); i++) {
                step_size *= mat.size.p[i];
            }
            size_t from_step_size = mat.elemSize();
            for (int i = 0; i < with_mat.size.dims(); i++) {
                from_step_size *= with_mat.size.p[i];
            }
            size_t elem_size = mat.elemSize();
            mat_update_roi(mat, with_mat, ranges, elem_size, step_size, from_step_size, 0, 0, 0);
        }

        if (!error_flag) {
            return evision_from(env, mat);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_clone,evision_cv_mat_clone,1
// @evision nif: def mat_clone(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_clone(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        // const char *keywords[] = {"img", NULL};
        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            // no need to do clone here as `evision_to_safe` will copy the data to img.
            return evision_from(env, img);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_zeros, evision_cv_mat_zeros, 1
// @evision nif: def mat_zeros(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_zeros(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        std::vector<int> shape;
        std::string t;
        int l = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "shape"), shape, ArgInfo("shape", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0))) {
            int type;
            if (!get_binary_type(t, l, 0, type)) return evision::nif::error(env, "not implemented for the given type");
            Mat out = Mat(Mat::zeros(shape.size(), shape.data(), type));
            return evision_from(env, out);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_ones, evision_cv_mat_ones, 1
// @evision nif: def mat_ones(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_ones(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        std::vector<int> shape;
        std::string t;
        int l = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "shape"), shape, ArgInfo("shape", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0))) {
            int type;
            if (!get_binary_type(t, l, 0, type)) return evision::nif::error(env, "not implemented for the given type");
            Mat out = Mat(Mat::ones(shape.size(), shape.data(), type));
            return evision_from(env, out);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_arange, evision_cv_mat_arange, 1
// @evision nif: def mat_arange(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_arange(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        int32_t from = 0, to = 0, step = 0;
        std::string t;
        int l = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "from"), from, ArgInfo("from", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "to"), to, ArgInfo("to", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "step"), step, ArgInfo("step", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0))) {
            int type;
            if (!get_binary_type(t, l, 0, type)) return evision::nif::error(env, "not implemented for the given type");

            int32_t count = (to - from) / step;
            int dims[1] = {0};
            dims[0] = (int)count;
            if (count <= 0) return evision::nif::error(env, "invalid values for start/end/step");

            std::vector<int32_t> values(count);
            int32_t v = from;
            for (int32_t i = 0; i < count; i++) {
                values[i] = v;
                v += step;
            }

            Mat out = Mat(1, dims, CV_32S, values.data());
            Mat ret;
            out.convertTo(ret, type);
            return evision_from(env, ret);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_full, evision_cv_mat_full, 1
// @evision nif: def mat_full(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_full(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        double number;
        std::vector<int> shape;
        std::string t;
        int l = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "number"), number, ArgInfo("from", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "shape"), shape, ArgInfo("to", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0))) {
            int type;
            if (!get_binary_type(t, l, 0, type)) return evision::nif::error(env, "not implemented for the given type");

            Mat out = Mat(Mat::ones(shape.size(), shape.data(), CV_64F) * number);
            Mat ret;
            out.convertTo(ret, type);
            return evision_from(env, ret);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_at, evision_cv_mat_at, 1
// @evision nif: def mat_at(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_at(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;
        size_t pos = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "pos"), pos, ArgInfo("img", 0))) {

            int type = img.type();
            uint8_t depth = type & CV_MAT_DEPTH_MASK;

            int i32;
            double f64;
            type = -1;

            switch ( depth ) {
                case CV_8U: {
                    i32 = ((uint8_t *)img.data)[pos]; type = 0;
                    break;
                }
                case CV_16U: {
                    i32 = ((uint16_t *)img.data)[pos]; type = 0;
                    break;
                }
                case CV_8S: {
                    i32 = ((int8_t *)img.data)[pos]; type = 0;
                    break;
                }
                case CV_16S: {
                    i32 = ((int16_t *)img.data)[pos]; type = 0;
                    break;
                }
                case CV_32S: {
                    i32 = ((int32_t *)img.data)[pos]; type = 0;
                    break;
                }

                case CV_32F: {
                    f64 = ((float *)img.data)[pos]; type = 1;
                    break;
                }
                case CV_64F: {
                    f64 = ((double *)img.data)[pos]; type = 1;
                    break;
                }
            }

            ERL_NIF_TERM ret;
            if (type == 0) {
                ret = enif_make_int(env, i32);
            } else if (type == 1) {
                ret = enif_make_double(env, f64);
            } else {
                ret = evision::nif::error(env, "unknown data type");
            }

            return ret;
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_set_to, evision_cv_mat_set_to, 1
// @evision nif: def mat_set_to(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_set_to(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;
        double value;
        Mat mask;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "value"), value, ArgInfo("value", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "mask"), mask, ArgInfo("mask", 0))) {
            img.setTo(value, mask);
            return evision_from(env, img);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_dot, evision_cv_mat_dot, 1
// @evision nif: def mat_dot(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_dot(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat a, b;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "a"), a, ArgInfo("a", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "b"), b, ArgInfo("b", 0))) {
            Mat out = a.cross(b);
            return evision_from(env, out);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_channels, evision_cv_mat_channels, 1
// @evision nif: def mat_channels(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_channels(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            int channels = img.channels();
            return enif_make_int64(env, channels);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_depth, evision_cv_mat_depth, 1
// @evision nif: def mat_depth(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_depth(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            int depth = img.depth();
            return enif_make_int64(env, depth);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_isSubmatrix, evision_cv_mat_isSubmatrix, 1
// @evision nif: def mat_isSubmatrix(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_isSubmatrix(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            bool isSubmatrix = img.isSubmatrix();
            if (isSubmatrix) {
                return evision::nif::atom(env, "true");
            } else {
                return evision::nif::atom(env, "false");
            }
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_isContinuous, evision_cv_mat_isContinuous, 1
// @evision nif: def mat_isContinuous(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_isContinuous(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            bool isContinuous = img.isContinuous();
            if (isContinuous) {
                return evision::nif::atom(env, "true");
            } else {
                return evision::nif::atom(env, "false");
            }
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_total, evision_cv_mat_total, 1
// @evision nif: def mat_total(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_total(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;
        int start_dim = -1;
        int end_dim = INT_MAX;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            evision_to_safe(env, evision_get_kw(env, erl_terms, "start_dim"), start_dim, ArgInfo("start_dim", 0));
            evision_to_safe(env, evision_get_kw(env, erl_terms, "end_dim"), end_dim, ArgInfo("end_dim", 0));
            size_t total_elems;
            if (start_dim == -1) {
                total_elems = img.total();
            } else {
                total_elems = img.total(start_dim, end_dim);
            }
            
            return enif_make_int64(env, total_elems);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_elemSize, evision_cv_mat_elemSize, 1
// @evision nif: def mat_elemSize(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_elemSize(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            return enif_make_int64(env, img.elemSize());
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_elemSize1, evision_cv_mat_elemSize1, 1
// @evision nif: def mat_elemSize1(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_elemSize1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            return enif_make_int64(env, img.elemSize1());
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_raw_type, evision_cv_mat_raw_type, 1
// @evision nif: def mat_raw_type(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_raw_type(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            return enif_make_int64(env, img.type());
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_dims, evision_cv_mat_dims, 1
// @evision nif: def mat_dims(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_dims(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            return enif_make_int64(env, img.dims);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_size, evision_cv_mat_size, 1
// @evision nif: def mat_size(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_size(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            ERL_NIF_TERM p = 0;
            evision::nif::make_i32_list_from_c_array(env, img.size.dims(), img.size.p, p);
            ERL_NIF_TERM dims = enif_make_int64(env, img.size.dims());
            return enif_make_tuple2(env, dims, p);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_as_shape, evision_cv_mat_as_shape, 1
// @evision nif: def mat_as_shape(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_as_shape(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;
        std::vector<int> as_shape;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "as_shape"), as_shape, ArgInfo("as_shape", 0))) {
            Mat ret = Mat((int)as_shape.size(), as_shape.data(), img.depth(), img.data);
            return evision_from(env, ret.clone());
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: mat_last_dim_as_channel, evision_cv_mat_last_dim_as_channel, 1
// @evision nif: def mat_last_dim_as_channel(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_last_dim_as_channel(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat src;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "src"), src, ArgInfo("src", 0))) {
            if ((src.type() == CV_8S || src.type() == CV_8U || src.type() == CV_16F \
                || src.type() == CV_16S || src.type() == CV_16U || src.type() == CV_32S \
                || src.type() == CV_32F || src.type() == CV_64F)) {
                int ndims = src.size.dims();
                if (ndims <= 1) {
                    return evision::nif::error(env, "image only has 1 dimension");
                }

                std::vector<int> new_shape(ndims - 1);
                for (size_t i = 0; i < (size_t)(ndims - 1); i++) {
                    new_shape[i] = src.size.p[i];
                }
                int type = CV_MAKETYPE(src.type(), src.size.p[ndims - 1]);
                Mat ret = Mat(ndims - 1, new_shape.data(), type, src.data);
                return evision_from(env, ret.clone());
            } else {
                return evision::nif::error(env, "image already has channel info");
            }
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

#endif // EVISION_OPENCV_MAT_H
