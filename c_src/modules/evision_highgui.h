#ifdef HAVE_OPENCV_HIGHGUI

// @evision enable_with: highgui

#ifndef OPENCV_HIGHGUI_ELIXIR
#define OPENCV_HIGHGUI_ELIXIR

#include <erl_nif.h>
#include <vector>
#include <mutex>
#include <condition_variable>
#include <map>
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

#define SHUTDOWN_CMD_ID -1

std::vector<std::pair<int, void *>> gCommands;
std::mutex gMtx;
std::condition_variable gCv;

#define EVISION_NOT_INITIATED 100
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

/* ************************************************************
 *  START AND STOP of driver thread
 * ************************************************************/

int start_native_gui(ErlNifEnv *env)
{
    int res;
#ifdef __APPLE__
    res = erl_drv_steal_main_thread((char *)"evision", &evision_thread, evision_main_loop, (void *)NULL, NULL);
#else
    ErlNifThreadOpts *opts = enif_thread_opts_create((char *)"evision thread");
    opts->suggested_stack_size = 8192;
    res = enif_thread_create((char *)"evision", &evision_thread, evision_main_loop, (void *)NULL, opts);
    enif_thread_opts_destroy(opts);
#endif
    {
        std::unique_lock<std::mutex> lock(gMtx);
        gCv.wait(lock, []{return evision_status != EVISION_NOT_INITIATED;});
    }

    if (res == 0) {
        return evision_status;
    } else {
        return EVISION_ERROR;
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

void *evision_main_loop(void * _unused)
{
    {
        std::unique_lock<std::mutex> lk(gMtx);
        evision_status = EVISION_INITIATED;
    }
    gCv.notify_one();

    int should_wait = true;
    while (should_wait) {
        {
            std::unique_lock<std::mutex> lk(gMtx);
            gCv.wait(lk, []{return new_commands != false;});
            unsigned long i = 0;
            while (i < gCommands.size()) {
                auto &command = gCommands[i];
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
                    free((void *)cmd->winname);
                    delete cmd;
                } else if (cmd_id == destroy_all_windows_cmd::ID) {
                    struct destroy_all_windows_cmd * cmd = (struct destroy_all_windows_cmd *)command.second;
                    cv::destroyAllWindows();
                    // it seems that we have to call cv::waitKey(n), n>=1
                    // otherwise the window won't close
                    cv::waitKey(1);
                    delete cmd;
                } else if (cmd_id == SHUTDOWN_CMD_ID) {
                    should_wait = 0;
                    i = gCommands.size();
                }
                i++;
            }
            gCommands.clear();
            new_commands = false;
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
// @evision nif: def imshow(_opts \\ []), do: :erlang.nif_error("cv::imshow not loaded")
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

            {
                std::unique_lock<std::mutex> lock(gMtx);
                gCommands.emplace_back(cmd->ID, cmd);
                new_commands = true;
                lock.unlock();
                gCv.notify_one();
            }
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

            {
                std::unique_lock<std::mutex> lock(gMtx);
                gCommands.emplace_back(cmd->ID, cmd);
                new_commands = true;
                lock.unlock();
                gCv.notify_one();
            }
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

            {
                std::unique_lock<std::mutex> lock(gMtx);
                gCommands.emplace_back(cmd->ID, cmd);
                new_commands = true;
                lock.unlock();
                gCv.notify_one();
            }
            return evision::nif::ok(env);
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: waitKey,evision_cv_waitKey,1
// @evision nif: def waitKey(_opts \\ []), do: :erlang.nif_error("cv::waitkey not loaded")
static ERL_NIF_TERM evision_cv_waitKey(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);
    int delay=0;
    int retval;

    // const char* keywords[] = { "delay", NULL }; // <- no more in use, left for debugging purpose
    if( evision_to_safe(env, evision_get_kw(env, erl_terms, "delay"), delay, ArgInfo("delay", 0)) )
    {
        start_native_gui(env);

        {
            std::unique_lock<std::mutex> lock(gMtx);
            struct waitkey_cmd * cmd = new waitkey_cmd;
            cmd->delay = delay;
            gCommands.emplace_back(cmd->ID, cmd);
            new_commands = true;
            lock.unlock();
            gCv.notify_one();
        }
        return evision::nif::ok(env);
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: destroyWindow,evision_cv_destroyWindow,1
// @evision nif: def destroyWindow(_opts \\ []), do: :erlang.nif_error("cv::destroy_window not loaded")
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

        {
            std::unique_lock<std::mutex> lock(gMtx);
            gCommands.emplace_back(cmd->ID, cmd);
            new_commands = true;
            lock.unlock();
            gCv.notify_one();
        }
        return evision::nif::ok(env);
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: destroyAllWindows,evision_cv_destroyAllWindows,0
// @evision nif: def destroyAllWindows(), do: :erlang.nif_error("cv::destroyAllWindows not loaded")
static ERL_NIF_TERM evision_cv_destroyAllWindows(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct destroy_all_windows_cmd * cmd = new destroy_all_windows_cmd;

    start_native_gui(env);

    {
        std::unique_lock<std::mutex> lock(gMtx);
        gCommands.emplace_back(cmd->ID, cmd);
        new_commands = true;
        lock.unlock();
        gCv.notify_one();
    }
    return evision::nif::ok(env);
}

#endif
#endif
