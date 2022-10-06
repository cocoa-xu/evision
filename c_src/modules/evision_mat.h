#ifndef EVISION_OPENCV_MAT_H
#define EVISION_OPENCV_MAT_H

#include <erl_nif.h>
#include <math.h>
#include "../nif_utils.hpp"
#include "evision_mat_utils.hpp"
#include "evision_backend/backend.h"
#include "evision_mat_api.h"

using namespace evision::nif;

// @evision c: mat_empty,evision_cv_mat_empty,0
// @evision nif: def mat_empty(), do: :erlang.nif_error("Mat::empty not loaded")
static ERL_NIF_TERM evision_cv_mat_empty(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    evision_res<cv::Mat *> * res;
    if (alloc_resource(&res)) {
        res->val = new cv::Mat();
    } else {
        return evision::nif::error(env, "no memory");
    }

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);

    return evision::nif::ok(env, _evision_make_mat_resource_into_map(env, *res->val, ret));
}

// @evision c: mat_type,evision_cv_mat_type,1
// @evision nif: def mat_type(_opts \\ []), do: :erlang.nif_error("Mat::type not loaded")
static ERL_NIF_TERM evision_cv_mat_type(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            return evision::nif::ok(env, _evision_get_mat_type(env, img));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_as_type,evision_cv_mat_as_type,1
// @evision nif: def mat_as_type(_opts \\ []), do: :erlang.nif_error("Mat::as_type not loaded")
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
                return evision::nif::ok(env, evision_from(env, ret));
            } else {
                return evision::nif::error(env, "unsupported target type");
            }
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_shape,evision_cv_mat_shape,1
// @evision nif: def mat_shape(_opts \\ []), do: :erlang.nif_error("Mat::shape not loaded")
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
            return evision::nif::ok(env, ret);
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_clone,evision_cv_mat_clone,1
// @evision nif: def mat_clone(_opts \\ []), do: :erlang.nif_error("Mat::clone not loaded")
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
            // no need to do clone here as evision_from will copy the data
            return evision::nif::ok(env, evision_from(env, img));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_zeros, evision_cv_mat_zeros, 1
// @evision nif: def mat_zeros(_opts \\ []), do: :erlang.nif_error("Mat::zeros not loaded")
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
            return evision::nif::ok(env, evision_from(env, out));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_ones, evision_cv_mat_ones, 1
// @evision nif: def mat_ones(_opts \\ []), do: :erlang.nif_error("Mat::ones not loaded")
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
            return evision::nif::ok(env, evision_from(env, out));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_arange, evision_cv_mat_arange, 1
// @evision nif: def mat_arange(_opts \\ []), do: :erlang.nif_error("Mat::arange not loaded")
static ERL_NIF_TERM evision_cv_mat_arange(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        int64_t from = 0, to = 0, step = 0;
        std::string t;
        int l = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "from"), from, ArgInfo("from", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "to"), to, ArgInfo("to", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "step"), step, ArgInfo("step", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0))) {
            int type;
            if (!get_binary_type(t, l, 0, type)) return evision::nif::error(env, "not implemented for the given type");

            int64_t count = (to - from) / step;
            int dims[1] = {0};
            dims[0] = (int)count;
            if (count <= 0) return evision::nif::error(env, "invalid values for start/end/step");

            std::vector<double> values(count);
            int64_t v = from;
            for (int64_t i = 0; i < count; i++) {
                values[i] = v;
                v += step;
            }

            Mat out = Mat(1, dims, CV_64F, values.data());
            Mat ret;
            out.convertTo(ret, type);
            return evision::nif::ok(env, evision_from(env, ret));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_full, evision_cv_mat_full, 1
// @evision nif: def mat_full(_opts \\ []), do: :erlang.nif_error("Mat::full not loaded")
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
            return evision::nif::ok(env, evision_from(env, ret));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_at, evision_cv_mat_at, 1
// @evision nif: def mat_at(_opts \\ []), do: :erlang.nif_error("Mat::at not loaded")
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
                ret = evision::nif::ok(env, enif_make_int(env, i32));
            } else if (type == 1) {
                ret = evision::nif::ok(env, enif_make_double(env, f64));
            } else {
                ret = evision::nif::error(env, "unknown data type");
            }

            return ret;
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_set_to, evision_cv_mat_set_to, 1
// @evision nif: def mat_set_to(_opts \\ []), do: :erlang.nif_error("Mat::setTo not loaded")
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
            return evision::nif::ok(env, evision_from(env, img));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_dot, evision_cv_mat_dot, 1
// @evision nif: def mat_dot(_opts \\ []), do: :erlang.nif_error("Mat::dot not loaded")
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
            return evision::nif::ok(env, evision_from(env, out));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_channels, evision_cv_mat_channels, 1
// @evision nif: def mat_channels(_opts \\ []), do: :erlang.nif_error("Mat::channels not loaded")
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
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_depth, evision_cv_mat_depth, 1
// @evision nif: def mat_depth(_opts \\ []), do: :erlang.nif_error("Mat::depth not loaded")
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
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_isSubmatrix, evision_cv_mat_isSubmatrix, 1
// @evision nif: def mat_isSubmatrix(_opts \\ []), do: :erlang.nif_error("Mat::isSubmatrix not loaded")
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
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_isContinuous, evision_cv_mat_isContinuous, 1
// @evision nif: def mat_isContinuous(_opts \\ []), do: :erlang.nif_error("Mat::isContinuous not loaded")
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
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_total, evision_cv_mat_total, 1
// @evision nif: def mat_total(_opts \\ []), do: :erlang.nif_error("Mat::total not loaded")
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
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_elemSize, evision_cv_mat_elemSize, 1
// @evision nif: def mat_elemSize(_opts \\ []), do: :erlang.nif_error("Mat::elemSize not loaded")
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
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_elemSize1, evision_cv_mat_elemSize1, 1
// @evision nif: def mat_elemSize1(_opts \\ []), do: :erlang.nif_error("Mat::elemSize1 not loaded")
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
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_raw_type, evision_cv_mat_raw_type, 1
// @evision nif: def mat_raw_type(_opts \\ []), do: :erlang.nif_error("Mat::raw_type not loaded")
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
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_dims, evision_cv_mat_dims, 1
// @evision nif: def mat_dims(_opts \\ []), do: :erlang.nif_error("Mat::dims not loaded")
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
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_size, evision_cv_mat_size, 1
// @evision nif: def mat_size(_opts \\ []), do: :erlang.nif_error("Mat::size not loaded")
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
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_as_shape, evision_cv_mat_as_shape, 1
// @evision nif: def mat_as_shape(_opts \\ []), do: :erlang.nif_error("Mat::as_shape not loaded")
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
            return evision::nif::ok(env, evision_from(env, ret.clone()));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_last_dim_as_channel, evision_cv_mat_last_dim_as_channel, 1
// @evision nif: def mat_last_dim_as_channel(_opts \\ []), do: :erlang.nif_error("Mat::last_dim_as_channel not loaded")
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
                return evision::nif::ok(env, evision_from(env, ret.clone()));
            } else {
                return evision::nif::error(env, "image already has channel info");
            }
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

#endif // EVISION_OPENCV_MAT_H
