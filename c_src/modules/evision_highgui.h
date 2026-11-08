#ifdef HAVE_OPENCV_HIGHGUI

// @evision enable_with: highgui

#ifndef OPENCV_HIGHGUI_ELIXIR
#define OPENCV_HIGHGUI_ELIXIR

#include <erl_nif.h>
#include <vector>
#include <mutex>
#include <condition_variable>
#include <map>
#include <functional>
#include <string>
#include "opencv2/core.hpp"
#include "../nif_utils.hpp"

using namespace evision::nif;

#define IMSHOW_TYPE_MAT     1
#define IMSHOW_TYPE_GPU_MAT 2
#define IMSHOW_TYPE_UMAT    3
struct imshow_cmd {
    static int ID;
    union {
        cv::Mat * mat;
        cv::cuda::GpuMat * gpuMat;
        cv::UMat * uMat;
    } mat;
    int type;
    const char * winname;
};
int imshow_cmd::ID = __LINE__;

struct waitkey_cmd {
    static int ID;
    int delay;
};
int waitkey_cmd::ID = __LINE__;

struct destroy_window_cmd {
    static int ID;
    const char * winname;
};
int destroy_window_cmd::ID = __LINE__;

struct destroy_all_windows_cmd {
    static int ID;
};
int destroy_all_windows_cmd::ID = __LINE__;

struct set_mouse_callback_cmd {
    static int ID;
    std::string winname;
    ErlNifEnv *env = nullptr;
    ERL_NIF_TERM on_mouse;
    ~set_mouse_callback_cmd() { if (env) enif_free_env(env); }
};
int set_mouse_callback_cmd::ID = __LINE__;

struct create_trackbar_cmd {
    static int ID;
    std::string trackbarname;
    std::string winname;
    int value;
    int count;
    ErlNifEnv *env = nullptr;
    ERL_NIF_TERM on_change;
    ~create_trackbar_cmd() { if (env) enif_free_env(env); }
};
int create_trackbar_cmd::ID = __LINE__;

#define SHUTDOWN_CMD_ID -1

std::vector<std::pair<int, void *>> gCommands;
std::mutex gMtx;
std::condition_variable gCv;

#define EVISION_NOT_INITIATED 100
#define EVISION_INITIATING 150
#define EVISION_INITIATED 200
#define EVISION_EXITED 900
#define EVISION_ERROR 1000

ErlNifTid evision_thread;

int evision_status = EVISION_NOT_INITIATED;
int new_commands = 0;

ErlNifPid init_caller;

#ifdef __APPLE__
extern "C" {
  int erl_drv_stolen_main_thread_join(ErlNifTid tid, void **respp);
  int erl_drv_steal_main_thread(char *name,
				ErlNifTid *dtid,
				void* (*func)(void*),
				void* arg,
				ErlNifThreadOpts *opts);
}
#endif

void *evision_main_loop(void * );

static void queue_command(int cmd_id, void *cmd)
{
    {
        std::unique_lock<std::mutex> lock(gMtx);
        gCommands.emplace_back(cmd_id, cmd);
        new_commands = true;
    }
    gCv.notify_one();
}

struct highgui_callback {
    std::string winname;
    std::string trackbarname;
    bool mouse;
    ErlNifEnv *env;
    ERL_NIF_TERM fun;
};

// Only the GUI thread touches these. OpenCV gets an id rather than a pointer
// because a destroyed window can still deliver events that were already queued.
std::map<intptr_t, highgui_callback> gCallbacks;
intptr_t gNextCallbackId = 1;

static void forget_callbacks(const std::function<bool(const highgui_callback &)> &match)
{
    for (auto it = gCallbacks.begin(); it != gCallbacks.end();) {
        if (match(it->second)) {
            enif_free_env(it->second.env);
            it = gCallbacks.erase(it);
        } else {
            ++it;
        }
    }
}

static void send_to_runner(const std::function<ERL_NIF_TERM(ErlNifEnv *)> &make_message)
{
    ErlNifEnv *msg_env = enif_alloc_env();
    ErlNifPid runner;
    if (enif_whereis_pid(nullptr, enif_make_atom(msg_env, "evision_highgui"), &runner)) {
        enif_send(nullptr, &runner, msg_env, make_message(msg_env));
    }
    enif_free_env(msg_env);
}

static void run_callback(void *userdata, const std::function<ERL_NIF_TERM(ErlNifEnv *)> &make_args)
{
    auto it = gCallbacks.find((intptr_t)userdata);
    if (it == gCallbacks.end()) return;

    ERL_NIF_TERM fun = it->second.fun;
    send_to_runner([&](ErlNifEnv *env) {
        return enif_make_tuple3(env,
            enif_make_atom(env, "evision_highgui_callback"),
            enif_make_copy(env, fun),
            make_args(env));
    });
}

static void on_mouse(int event, int x, int y, int flags, void *userdata)
{
    run_callback(userdata, [=](ErlNifEnv *env) {
        return enif_make_list4(env,
            enif_make_int(env, event),
            enif_make_int(env, x),
            enif_make_int(env, y),
            enif_make_int(env, flags));
    });
}

static void on_trackbar(int pos, void *userdata)
{
    run_callback(userdata, [=](ErlNifEnv *env) {
        return enif_make_list1(env, enif_make_int(env, pos));
    });
}

static void report_error(const char *message)
{
    send_to_runner([=](ErlNifEnv *env) {
        ERL_NIF_TERM binary;
        size_t len = strlen(message);
        memcpy(enif_make_new_binary(env, len, &binary), message, len);
        return enif_make_tuple2(env, enif_make_atom(env, "evision_highgui_error"), binary);
    });
}

/* ************************************************************
 *  START AND STOP of driver thread
 * ************************************************************/

int start_native_gui(ErlNifEnv *env)
{
    {
        std::unique_lock<std::mutex> lock(gMtx);
        if (evision_status == EVISION_INITIATED) {
            return evision_status;
        }
        if (evision_status == EVISION_INITIATING) {
            gCv.wait(lock, []{return evision_status != EVISION_INITIATING;});
            return evision_status;
        }
        evision_status = EVISION_INITIATING;
    }

    int res;
#ifdef __APPLE__
    res = erl_drv_steal_main_thread((char *)"evision", &evision_thread, evision_main_loop, (void *)NULL, NULL);
#else
    ErlNifThreadOpts *opts = enif_thread_opts_create((char *)"evision thread");
    opts->suggested_stack_size = 8192;
    res = enif_thread_create((char *)"evision", &evision_thread, evision_main_loop, (void *)NULL, opts);
    enif_thread_opts_destroy(opts);
#endif
    if (res != 0) {
        std::unique_lock<std::mutex> lock(gMtx);
        evision_status = EVISION_ERROR;
        lock.unlock();
        gCv.notify_all();
        return EVISION_ERROR;
    }

    {
        std::unique_lock<std::mutex> lock(gMtx);
        gCv.wait(lock, []{return evision_status != EVISION_INITIATING;});
        return evision_status;
    }
}

void stop_native_gui(ErlNifEnv* env)
{
    cv::destroyAllWindows();
#ifdef __APPLE__
  erl_drv_stolen_main_thread_join(evision_thread, NULL);
#else
  enif_thread_join(evision_thread, NULL);
#endif
}

static void set_mouse_callback(set_mouse_callback_cmd *cmd)
{
    try {
        intptr_t id = cmd->env ? gNextCallbackId++ : 0;
        cv::setMouseCallback(cmd->winname, id ? on_mouse : nullptr, (void *)id);
        forget_callbacks([&](const highgui_callback &cb) {
            return cb.mouse && cb.winname == cmd->winname;
        });
        if (id) {
            gCallbacks[id] = {cmd->winname, "", true, cmd->env, cmd->on_mouse};
            cmd->env = nullptr;
        }
    } catch (const cv::Exception &e) {
        report_error(e.what());
    }
}

static void create_trackbar(create_trackbar_cmd *cmd)
{
    try {
        intptr_t id = cmd->env ? gNextCallbackId++ : 0;
        // No value pointer, then set the position, as OpenCV's Python binding does (opencv#20408).
        cv::createTrackbar(cmd->trackbarname, cmd->winname, nullptr, cmd->count, id ? on_trackbar : nullptr, (void *)id);
        forget_callbacks([&](const highgui_callback &cb) {
            return !cb.mouse && cb.winname == cmd->winname && cb.trackbarname == cmd->trackbarname;
        });
        if (id) {
            gCallbacks[id] = {cmd->winname, cmd->trackbarname, false, cmd->env, cmd->on_change};
            cmd->env = nullptr;
        }
        cv::setTrackbarPos(cmd->trackbarname, cmd->winname, cmd->value);
    } catch (const cv::Exception &e) {
        report_error(e.what());
    }
}

void *evision_main_loop(void * _unused)
{
    {
        std::unique_lock<std::mutex> lk(gMtx);
        evision_status = EVISION_INITIATED;
    }
    gCv.notify_all();

    bool should_wait = true;
    while (should_wait) {
        std::vector<std::pair<int, void *>> commands;
        {
            std::unique_lock<std::mutex> lk(gMtx);
            gCv.wait(lk, []{return new_commands != false;});
            commands.swap(gCommands);
            new_commands = false;
        }

        // Run without gMtx: a callback may queue commands while waitKey is waiting.
        for (auto &command : commands) {
            int cmd_id = command.first;
            if (cmd_id == imshow_cmd::ID) {
                struct imshow_cmd * cmd = (struct imshow_cmd *)command.second;

                if (cmd->type == IMSHOW_TYPE_MAT) {
                    cv::Mat show = cmd->mat.mat->clone();
                    cv::imshow(cmd->winname, show);
                    delete cmd->mat.mat;
                } else if (cmd->type == IMSHOW_TYPE_GPU_MAT) {
                    cv::cuda::GpuMat show = cmd->mat.gpuMat->clone();
                    cv::imshow(cmd->winname, show);
                    delete cmd->mat.gpuMat;
                } else if (cmd->type == IMSHOW_TYPE_UMAT) {
                    cv::UMat show = cmd->mat.uMat->clone();
                    cv::imshow(cmd->winname, show);
                    delete cmd->mat.uMat;
                }

                free((void *)cmd->winname);
                delete cmd;
            } else if (cmd_id == waitkey_cmd::ID) {
                struct waitkey_cmd * cmd = (struct waitkey_cmd *)command.second;
                cv::waitKey(cmd->delay);
                delete cmd;
            } else if (cmd_id == destroy_window_cmd::ID) {
                struct destroy_window_cmd * cmd = (struct destroy_window_cmd *)command.second;
                cv::destroyWindow(cmd->winname);
                // it seems that we have to call cv::waitKey(n), n>=1
                // otherwise the window won't close
                cv::waitKey(1);
                std::string winname = cmd->winname;
                forget_callbacks([&](const highgui_callback &cb) { return cb.winname == winname; });
                free((void *)cmd->winname);
                delete cmd;
            } else if (cmd_id == destroy_all_windows_cmd::ID) {
                struct destroy_all_windows_cmd * cmd = (struct destroy_all_windows_cmd *)command.second;
                cv::destroyAllWindows();
                // it seems that we have to call cv::waitKey(n), n>=1
                // otherwise the window won't close
                cv::waitKey(1);
                forget_callbacks([](const highgui_callback &) { return true; });
                delete cmd;
            } else if (cmd_id == set_mouse_callback_cmd::ID) {
                struct set_mouse_callback_cmd * cmd = (struct set_mouse_callback_cmd *)command.second;
                set_mouse_callback(cmd);
                delete cmd;
            } else if (cmd_id == create_trackbar_cmd::ID) {
                struct create_trackbar_cmd * cmd = (struct create_trackbar_cmd *)command.second;
                create_trackbar(cmd);
                delete cmd;
            } else if (cmd_id == SHUTDOWN_CMD_ID) {
                should_wait = false;
                break;
            }
        }
    }
    if (evision_status == EVISION_INITIATED) {
        /* We are done try to make a clean exit */
        evision_status = EVISION_EXITED;
#ifndef __APPLE__
        enif_thread_exit(NULL);
#endif
        return NULL;
    } else {
        return NULL;
    }
}

// @evision c: imshow,evision_cv_imshow,1
// @evision nif: def imshow(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_imshow(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        String winname;
        Mat mat;

        // const char* keywords[] = { "winname", "mat", NULL }; // <- no more in use, left for debugging purpose
        if( evision_to_safe(env, evision_get_kw(env, erl_terms, "winname"), winname, ArgInfo("winname", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "mat"), mat, ArgInfo("mat", 0)) )
        {
            struct imshow_cmd * cmd = new imshow_cmd;
            cmd->mat.mat = new Mat;
            mat.copyTo(*cmd->mat.mat);
            cmd->type = IMSHOW_TYPE_MAT;
            cmd->winname = strdup(winname.c_str());

            start_native_gui(env);
            queue_command(cmd->ID, cmd);
            return evision::nif::ok(env);
        }
    }

    {
        String winname;
        cuda::GpuMat mat;

        // const char* keywords[] = { "winname", "mat", NULL }; // <- no more in use, left for debugging purpose
        if( evision_to_safe(env, evision_get_kw(env, erl_terms, "winname"), winname, ArgInfo("winname", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "mat"), mat, ArgInfo("mat", 0)) )
        {
            struct imshow_cmd * cmd = new imshow_cmd;
            cmd->mat.gpuMat = new cuda::GpuMat;
            mat.copyTo(*cmd->mat.gpuMat);
            cmd->type = IMSHOW_TYPE_GPU_MAT;
            cmd->winname = strdup(winname.c_str());

            start_native_gui(env);
            queue_command(cmd->ID, cmd);
            return evision::nif::ok(env);
        }
    }

    {
        String winname;
        UMat mat;

        // const char* keywords[] = { "winname", "mat", NULL }; // <- no more in use, left for debugging purpose
        if( evision_to_safe(env, evision_get_kw(env, erl_terms, "winname"), winname, ArgInfo("winname", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "mat"), mat, ArgInfo("mat", 0)) )
        {
            struct imshow_cmd * cmd = new imshow_cmd;
            cmd->mat.uMat = new cv::UMat;
            mat.copyTo(*cmd->mat.uMat);
            cmd->type = IMSHOW_TYPE_UMAT;
            cmd->winname = strdup(winname.c_str());

            start_native_gui(env);
            queue_command(cmd->ID, cmd);
            return evision::nif::ok(env);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: waitKey,evision_cv_waitKey,1
// @evision nif: def waitKey(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_waitKey(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);
    int delay=0;
    // const char* keywords[] = { "delay", NULL }; // <- no more in use, left for debugging purpose
    if( evision_to_safe(env, evision_get_kw(env, erl_terms, "delay"), delay, ArgInfo("delay", 0)) )
    {
        start_native_gui(env);

        struct waitkey_cmd * cmd = new waitkey_cmd;
        cmd->delay = delay;
        queue_command(cmd->ID, cmd);
        return evision::nif::ok(env);
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: destroyWindow,evision_cv_destroyWindow,1
// @evision nif: def destroyWindow(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_destroyWindow(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);
    String winname;

    // const char* keywords[] = { "winname", NULL }; // <- no more in use, left for debugging purpose
    if( evision_to_safe(env, evision_get_kw(env, erl_terms, "winname"), winname, ArgInfo("winname", 0)) )
    {
        struct destroy_window_cmd * cmd = new destroy_window_cmd;
        cmd->winname = strdup(winname.c_str());

        start_native_gui(env);
        queue_command(cmd->ID, cmd);
        return evision::nif::ok(env);
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

// @evision c: destroyAllWindows,evision_cv_destroyAllWindows,0
// @evision nif: def destroyAllWindows(), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_destroyAllWindows(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct destroy_all_windows_cmd * cmd = new destroy_all_windows_cmd;

    start_native_gui(env);
    queue_command(cmd->ID, cmd);
    return evision::nif::ok(env);
}

// @evision c: setMouseCallback,evision_cv_setMouseCallback,1
// @evision nif: def setMouseCallback(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_setMouseCallback(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    cv::String winname;
    if (!evision_to_safe(env, evision_get_kw(env, erl_terms, "winname"), winname, ArgInfo("winname", 0))) {
        return enif_make_badarg(env);
    }

    struct set_mouse_callback_cmd * cmd = new set_mouse_callback_cmd;
    cmd->winname = winname;
    ERL_NIF_TERM on_mouse = evision_get_kw(env, erl_terms, "on_mouse");
    if (enif_is_fun(env, on_mouse)) {
        cmd->env = enif_alloc_env();
        cmd->on_mouse = enif_make_copy(cmd->env, on_mouse);
    }

    start_native_gui(env);
    queue_command(cmd->ID, cmd);
    return evision::nif::ok(env);
}

// @evision c: createTrackbar,evision_cv_createTrackbar,1
// @evision nif: def createTrackbar(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_createTrackbar(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    cv::String trackbarname;
    cv::String winname;
    int value = 0;
    int count = 0;
    if (!evision_to_safe(env, evision_get_kw(env, erl_terms, "trackbarname"), trackbarname, ArgInfo("trackbarname", 0)) ||
        !evision_to_safe(env, evision_get_kw(env, erl_terms, "winname"), winname, ArgInfo("winname", 0)) ||
        !evision_to_safe(env, evision_get_kw(env, erl_terms, "value"), value, ArgInfo("value", 0)) ||
        !evision_to_safe(env, evision_get_kw(env, erl_terms, "count"), count, ArgInfo("count", 0))) {
        return enif_make_badarg(env);
    }

    struct create_trackbar_cmd * cmd = new create_trackbar_cmd;
    cmd->trackbarname = trackbarname;
    cmd->winname = winname;
    cmd->value = value;
    cmd->count = count;
    ERL_NIF_TERM on_change = evision_get_kw(env, erl_terms, "on_change");
    if (enif_is_fun(env, on_change)) {
        cmd->env = enif_alloc_env();
        cmd->on_change = enif_make_copy(cmd->env, on_change);
    }

    start_native_gui(env);
    queue_command(cmd->ID, cmd);
    return evision::nif::ok(env);
}

#endif
#endif
