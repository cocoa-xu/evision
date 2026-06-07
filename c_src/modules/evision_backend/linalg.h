#ifndef EVISION_BACKEND_LINALG_H
#define EVISION_BACKEND_LINALG_H

#include <algorithm>
#include <cmath>
#include <utility>
#include <vector>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"

// Decompositions OpenCV exposes no factor-extraction for. All inputs/outputs are f64
// (the caller casts to/from out.type) and a single n*n (or m*n) matrix; the caller loops
// over batch dims. determinant/solve/SVD/eigen are handled directly via cv in Elixir.

// Cholesky-Banachiewicz lower factor L (L*L^T = A), upper entries zero. A must be
// symmetric positive definite.
// @evision c: mat_cholesky, evision_cv_mat_cholesky, 1
// @evision nif: def mat_cholesky(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_cholesky(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat a;
        int n = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "a"), a, ArgInfo("a", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "n"), n, ArgInfo("n", 0))) {
            Mat a_c = a.isContinuous() ? a : a.clone();
            const double *ap = (const double *)a_c.data;
            Mat l = Mat::zeros(n, n, CV_64F);
            double *lp = (double *)l.data;

            for (int i = 0; i < n; i++) {
                for (int j = 0; j <= i; j++) {
                    double sum = ap[i * n + j];
                    for (int k = 0; k < j; k++) sum -= lp[i * n + k] * lp[j * n + k];

                    if (i == j) {
                        if (sum <= 0.0)
                            return evision::nif::error(env, "cholesky: matrix is not positive definite");
                        lp[i * n + j] = std::sqrt(sum);
                    } else {
                        lp[i * n + j] = sum / lp[j * n + j];
                    }
                }
            }
            return evision_from(env, l);
        }
    }

    return enif_make_badarg(env);
}

// Partial-pivot LU returning {P, L, U} (all n*n) with A = P*L*U, L unit lower, U upper.
// @evision c: mat_lu, evision_cv_mat_lu, 1
// @evision nif: def mat_lu(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_lu(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat a;
        int n = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "a"), a, ArgInfo("a", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "n"), n, ArgInfo("n", 0))) {
            Mat m = a.clone();  // mutated in place into the row-permuted A
            double *mp = (double *)m.data;
            std::vector<int> perm((size_t)n);
            for (int i = 0; i < n; i++) perm[(size_t)i] = i;

            // bring the max-abs element of each column onto the diagonal
            for (int j = 0; j < n - 1; j++) {
                int maxi = j;
                double maxv = std::fabs(mp[j * n + j]);
                for (int i = j + 1; i < n; i++) {
                    double v = std::fabs(mp[i * n + j]);
                    if (v > maxv) { maxv = v; maxi = i; }
                }
                if (maxi != j) {
                    for (int k = 0; k < n; k++) std::swap(mp[j * n + k], mp[maxi * n + k]);
                    std::swap(perm[(size_t)j], perm[(size_t)maxi]);
                }
            }

            // Doolittle: m (= permuted A) = L * U
            Mat L = Mat::zeros(n, n, CV_64F);
            Mat U = Mat::zeros(n, n, CV_64F);
            double *lp = (double *)L.data;
            double *up = (double *)U.data;
            for (int i = 0; i < n; i++) {
                for (int k = i; k < n; k++) {
                    double sum = 0.0;
                    for (int t = 0; t < i; t++) sum += lp[i * n + t] * up[t * n + k];
                    up[i * n + k] = mp[i * n + k] - sum;
                }
                lp[i * n + i] = 1.0;
                double diag = up[i * n + i];
                for (int k = i + 1; k < n; k++) {
                    double sum = 0.0;
                    for (int t = 0; t < i; t++) sum += lp[k * n + t] * up[t * n + i];
                    lp[k * n + i] = diag != 0.0 ? (mp[k * n + i] - sum) / diag : 0.0;
                }
            }

            // P with A = P*L*U: row perm[i] of P is the i-th unit column
            Mat P = Mat::zeros(n, n, CV_64F);
            double *pp = (double *)P.data;
            for (int i = 0; i < n; i++) pp[perm[(size_t)i] * n + i] = 1.0;

            return enif_make_tuple3(env, evision_from(env, P), evision_from(env, L), evision_from(env, U));
        }
    }

    return enif_make_badarg(env);
}

// Householder QR returning {Q, R} with A = Q*R. reduced (complete=0): Q is m*k, R is k*n;
// complete: Q is m*m, R is m*n, where k = min(m, n).
// @evision c: mat_qr, evision_cv_mat_qr, 1
// @evision nif: def mat_qr(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_qr(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat a;
        int m = 0, n = 0, complete = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "a"), a, ArgInfo("a", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "m"), m, ArgInfo("m", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "n"), n, ArgInfo("n", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "complete"), complete, ArgInfo("complete", 0))) {
            Mat R = a.clone();                 // m x n, mutated into the upper factor
            Mat Q = Mat::eye(m, m, CV_64F);     // accumulated orthogonal factor
            double *rp = (double *)R.data;
            double *qp = (double *)Q.data;
            std::vector<double> v((size_t)m);
            int kmax = std::min(m, n);

            for (int k = 0; k < kmax; k++) {
                double normx = 0.0;
                for (int i = k; i < m; i++) normx += rp[i * n + k] * rp[i * n + k];
                normx = std::sqrt(normx);
                if (normx == 0.0) continue;

                double alpha = (rp[k * n + k] > 0.0) ? -normx : normx;
                for (int i = 0; i < m; i++) v[(size_t)i] = 0.0;
                v[(size_t)k] = rp[k * n + k] - alpha;
                for (int i = k + 1; i < m; i++) v[(size_t)i] = rp[i * n + k];

                double vnorm2 = 0.0;
                for (int i = k; i < m; i++) vnorm2 += v[(size_t)i] * v[(size_t)i];
                if (vnorm2 == 0.0) continue;

                // R <- R - (2/vnorm2) v (v^T R)
                for (int j = 0; j < n; j++) {
                    double s = 0.0;
                    for (int i = k; i < m; i++) s += v[(size_t)i] * rp[i * n + j];
                    s = 2.0 * s / vnorm2;
                    for (int i = k; i < m; i++) rp[i * n + j] -= s * v[(size_t)i];
                }
                // Q <- Q - (2/vnorm2) (Q v) v^T
                for (int i = 0; i < m; i++) {
                    double s = 0.0;
                    for (int l = k; l < m; l++) s += qp[i * m + l] * v[(size_t)l];
                    s = 2.0 * s / vnorm2;
                    for (int l = k; l < m; l++) qp[i * m + l] -= s * v[(size_t)l];
                }
            }

            if (complete) {
                return enif_make_tuple2(env, evision_from(env, Q), evision_from(env, R));
            }

            // reduced: first k columns of Q (m x k), first k rows of R (k x n)
            Mat Qr(m, kmax, CV_64F);
            double *qrp = (double *)Qr.data;
            for (int i = 0; i < m; i++)
                for (int j = 0; j < kmax; j++) qrp[i * kmax + j] = qp[i * m + j];

            Mat Rr(kmax, n, CV_64F);
            double *rrp = (double *)Rr.data;
            for (int i = 0; i < kmax; i++)
                for (int j = 0; j < n; j++) rrp[i * n + j] = rp[i * n + j];

            return enif_make_tuple2(env, evision_from(env, Qr), evision_from(env, Rr));
        }
    }

    return enif_make_badarg(env);
}

// Triangular system solve by substitution, reading only the relevant triangle (Nx
// semantics). a is n*n; b is br*bc (2D, vectors passed as n*1). lower/left_side/transpose
// select the variant; the result has b's shape.
// @evision c: mat_triangular_solve, evision_cv_mat_triangular_solve, 1
// @evision nif: def mat_triangular_solve(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_triangular_solve(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat a, b;
        int n = 0, lower = 0, left_side = 0, transpose = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "a"), a, ArgInfo("a", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "b"), b, ArgInfo("b", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "n"), n, ArgInfo("n", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "lower"), lower, ArgInfo("lower", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "left_side"), left_side, ArgInfo("left_side", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "transpose"), transpose, ArgInfo("transpose", 0))) {
            Mat a_c = a.isContinuous() ? a : a.clone();
            Mat b_c = b.isContinuous() ? b : b.clone();
            const double *ad = (const double *)a_c.data;
            const double *bd = (const double *)b_c.data;
            int br = b_c.rows, bc = b_c.cols;
            Mat X(br, bc, CV_64F);
            double *xd = (double *)X.data;

            // op(A)(i,j) is A or A^T; eff_lower says whether op(A) is lower-triangular
            auto opA = [&](int i, int j) -> double { return transpose ? ad[j * n + i] : ad[i * n + j]; };
            bool eff_lower = ((lower != 0) != (transpose != 0));

            if (left_side) {
                // op(A) X = B, B is n x bc, solve each column
                bool lo = eff_lower;
                for (int c = 0; c < bc; c++) {
                    if (lo) {
                        for (int i = 0; i < n; i++) {
                            double s = bd[i * bc + c];
                            for (int j = 0; j < i; j++) s -= opA(i, j) * xd[j * bc + c];
                            double d = opA(i, i);
                            if (d == 0.0) return evision::nif::error(env, "triangular_solve: singular matrix");
                            xd[i * bc + c] = s / d;
                        }
                    } else {
                        for (int i = n - 1; i >= 0; i--) {
                            double s = bd[i * bc + c];
                            for (int j = i + 1; j < n; j++) s -= opA(i, j) * xd[j * bc + c];
                            double d = opA(i, i);
                            if (d == 0.0) return evision::nif::error(env, "triangular_solve: singular matrix");
                            xd[i * bc + c] = s / d;
                        }
                    }
                }
            } else {
                // X op(A) = B, B is br x n. Transpose to M y = row, M = op(A)^T
                bool lo = !eff_lower;
                auto M = [&](int i, int j) -> double { return opA(j, i); };
                for (int r = 0; r < br; r++) {
                    if (lo) {
                        for (int i = 0; i < n; i++) {
                            double s = bd[r * bc + i];
                            for (int j = 0; j < i; j++) s -= M(i, j) * xd[r * bc + j];
                            double d = M(i, i);
                            if (d == 0.0) return evision::nif::error(env, "triangular_solve: singular matrix");
                            xd[r * bc + i] = s / d;
                        }
                    } else {
                        for (int i = n - 1; i >= 0; i--) {
                            double s = bd[r * bc + i];
                            for (int j = i + 1; j < n; j++) s -= M(i, j) * xd[r * bc + j];
                            double d = M(i, i);
                            if (d == 0.0) return evision::nif::error(env, "triangular_solve: singular matrix");
                            xd[r * bc + i] = s / d;
                        }
                    }
                }
            }

            return evision_from(env, X);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_LINALG_H
