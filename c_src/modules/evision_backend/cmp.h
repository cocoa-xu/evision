#ifndef EVISION_BACKEND_CMP_H
#define EVISION_BACKEND_CMP_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"

// @evision c: mat_cmp, evision_cv_mat_cmp, 1
// @evision nif: def mat_cmp(_opts \\ []), do: :erlang.nif_error("Mat::greater not loaded")
static ERL_NIF_TERM evision_cv_mat_cmp(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);
    int error_flag = false;

    {
        Mat l;
        Mat r;
        std::string type;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "r"), r, ArgInfo("r", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "type"), type, ArgInfo("type", 0))) {

            int cmpop = -1;
            if (type == "eq") cmpop = cv::CMP_EQ;
            else if (type == "gt") cmpop = cv::CMP_GT;
            else if (type == "ge") cmpop = cv::CMP_GE;
            else if (type == "lt") cmpop = cv::CMP_LT;
            else if (type == "le") cmpop = cv::CMP_LE;
            else if (type == "ne") cmpop = cv::CMP_NE;
            else {
                return evision::nif::error(env, "not implemented for the requested compare type, only 'eq', 'gt', 'ge', 'lt', 'le' and 'ne' are supported.");
            }

            Mat ret;

            ERRWRAP2(cv::compare(l, r, ret, cmpop), env, error_flag, error_term);
            if (!error_flag) {
                ERRWRAP2(ret = ret / 255, env, error_flag, error_term);
                if (!error_flag) {
                    return evision::nif::ok(env, evision_from(env, ret));
                }
            }
        }
    }

    if (error_flag) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

#endif // EVISION_BACKEND_CMP_H
