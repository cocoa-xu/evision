#ifndef EVISION_MAT_UTILS_HPP
#define EVISION_MAT_UTILS_HPP

int get_binary_type(const std::string& t, int l, int n, int& type) {
    if (t == "u") {
        if (l == 8) {
            if (n != 0) type = CV_8UC(n);
            else type = CV_8U;
            return true;
        }
        if (l == 16) {
            if (n != 0) type = CV_16UC(n);
            else type = CV_16U;
            return true;
        }
    } else if (t == "s") {
        if (l == 8) {
            if (n != 0) type = CV_8SC(n);
            else type = CV_8S;
            return true;
        }
        if (l == 16) {
            if (n != 0) type = CV_16SC(n);
            else type = CV_16S;
            return true;
        }
        if (l == 32) {
            if (n != 0) type = CV_32SC(n);
            else type = CV_32S;
            return true;
        }
    } else if (t == "f") {
        if (l == 32) {
            if (n != 0) type = CV_32FC(n);
            else type = CV_32F;
            return true;
        }
        if (l == 64) {
            if (n != 0) type = CV_64FC(n);
            else type = CV_64F;
            return true;
        }
    }
    return false;
}

static void _evision_binary_unref(void *buf, ErlNifEnv *env) {
    // enif_fprintf(stderr, "freed nif_env\n");
    // freeing env frees unref all terms it contains
    enif_free_env(env);
    return;
}

static ERL_NIF_TERM _evision_binary_ref(ERL_NIF_TERM bin_term, evision_res<cv::Mat *> * zero_copy_mat) {
    // adapted from https://github.com/akash-akya/zero_copy/blob/master/c_src/zero_copy.c
    ErlNifBinary bin;
    ERL_NIF_TERM term;
    ErlNifEnv *new_env;

    zero_copy_mat->in_buf = nullptr;
    zero_copy_mat->in_ref = nullptr;

    // keep reference to binary by creating new nif-env and copying
    // binary-term reference to it
    new_env = enif_alloc_env();
    term = enif_make_copy(new_env, bin_term);

    if (!enif_inspect_binary(new_env, term, &bin)) {
        enif_free_env(new_env);
        return 1;
    }

    // Note that we are *NOT* copying the binary data
    zero_copy_mat->in_buf = bin.data;

    // input buffer specific opaque data which will be passed as second
    // argument to finalizer during unref
    zero_copy_mat->in_ref = (void *)new_env;
    // function to be called to unref the input data
    zero_copy_mat->in_unref = (void (*)(void *, void *))_evision_binary_unref;

    return 0;
}

#endif // EVISION_MAT_UTILS_HPP
