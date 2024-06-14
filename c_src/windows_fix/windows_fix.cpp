#include <string>
#include <cstring>
#include <stdlib.h>
#include <sstream>
#include <vector>
#include <stdio.h>
#include <windows.h>
#include <libloaderapi.h>
#include <winbase.h>
#include <wchar.h>
#include <erl_nif.h>

ERL_NIF_TERM atom(ErlNifEnv *env, const char *msg) {
    ERL_NIF_TERM a;
    if (enif_make_existing_atom(env, msg, &a, ERL_NIF_LATIN1)) {
        return a;
    } else {
        return enif_make_atom(env, msg);
    }
}

ERL_NIF_TERM error(ErlNifEnv *env, const char *msg) {
    ERL_NIF_TERM error_atom = atom(env, "error");
    ERL_NIF_TERM msg_term = enif_make_string(env, msg, ERL_NIF_LATIN1);
    return enif_make_tuple2(env, atom, msg_term);
}

ERL_NIF_TERM ok(ErlNifEnv *env) {
    return atom(env, "ok");
}

int evision_windows_fix_dir_exists(LPCWSTR path) {
    DWORD dwAttrib = GetFileAttributesW(path);
    return (dwAttrib != INVALID_FILE_ATTRIBUTES && (dwAttrib & FILE_ATTRIBUTE_DIRECTORY));
}

static ERL_NIF_TERM evision_windows_fix_run_once(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    static bool opencv_path_updated = false;
    static bool cuda_path_updated = false;

    if (opencv_path_updated && cuda_path_updated) return ok(env);

    char err_msg[128] = { '\0' };
    SYSTEM_INFO systemInfo;
    GetNativeSystemInfo(&systemInfo);

    std::wstring cpu_arch;
    if (systemInfo.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64) {
        cpu_arch = L"x64";
    } else if (systemInfo.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_ARM64) {
        cpu_arch = L"ARM64";
    } else {
        snprintf(err_msg, sizeof(err_msg) - 1, "Cannot determine current processor architecture: %d", systemInfo.wProcessorArchitecture);
        return error(env, err_msg);
    }

    if (!opencv_path_updated) {
        std::wstring directory;
        wchar_t path[65536];
        
        HMODULE hm = NULL;
        if (GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT, (LPCWSTR)&evision_windows_fix_run_once, &hm) == 0) {
            int ret = GetLastError();
            snprintf(err_msg, sizeof(err_msg) - 1, "GetModuleHandle failed, error = %d", ret);
            return error(env, err_msg);
        }

        if (GetModuleFileNameW(hm, (LPWSTR)path, sizeof(path)) == 0) {
            int ret = GetLastError();
            snprintf(err_msg, sizeof(err_msg) - 1, "GetModuleFileName failed, error = %d", ret);
            return error(env, err_msg);
        }

        directory = path;
        wchar_t backslash = '\\';
        auto pos = directory.find_last_of(backslash);
        auto priv_dir = directory.substr(0, pos);
        std::wstring lib_dir;

        // test from vc10 to vc99
        // this should last for a while
        if (sizeof(void*) == 8) {
            for (int vc_version = 10; vc_version < 99; vc_version++) {
                std::wstringstream t;
                t << priv_dir << L"\\" << cpu_arch << L"\\vc";
                t << vc_version;
                t << L"\\bin";
                auto probe = t.str();
                if (evision_windows_fix_dir_exists((LPCWSTR)probe.c_str())) {
                    lib_dir = probe;
                    break;
                }
            }
        }
        else if (sizeof(void*) == 4) {
            for (int vc_version = 10; vc_version < 99; vc_version++) {
                std::wstringstream t;
                t << priv_dir + L"\\x32\\vc";
                t << vc_version;
                t << L"\\bin";
                auto probe = t.str();
                if (evision_windows_fix_dir_exists((LPCWSTR)probe.c_str())) {
                    lib_dir = probe;
                    break;
                }
            }
        }

        if (lib_dir.length() == 0) {
            ERL_NIF_TERM ret_term = error(env, "Cannot detect OpenCV lib directory");
            return ret_term;
        }
        printf("Found OpenCV Lib Directory: %ls\r\n", lib_dir.c_str());

        PCWSTR opencvLibDirectoryPCWSTR = lib_dir.c_str();
        WCHAR pathBuffer[65536];
        DWORD pathLen = GetEnvironmentVariableW(L"PATH", pathBuffer, 65536);

        WCHAR newPath[65536];
        newPath[0] = L'\0';
        wcscpy_s(newPath, _countof(newPath), (const wchar_t*)pathBuffer);
        wcscat_s(newPath, _countof(newPath), (const wchar_t*)L";");
        wcscat_s(newPath, _countof(newPath), (const wchar_t*)opencvLibDirectoryPCWSTR);

        SetEnvironmentVariableW(L"PATH", newPath);
        SetDefaultDllDirectories(LOAD_LIBRARY_SEARCH_DEFAULT_DIRS | LOAD_LIBRARY_SEARCH_USER_DIRS);
        DLL_DIRECTORY_COOKIE ret = AddDllDirectory(opencvLibDirectoryPCWSTR);
        if (ret == 0) {
            DWORD last_error = GetLastError();
            LPTSTR error_text = nullptr;
            FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, HRESULT_FROM_WIN32(last_error), MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR)&error_text, 0, NULL);
            if (error_text != nullptr) {
                ERL_NIF_TERM ret_term = error(env, error_text);
                LocalFree(error_text);
                return ret_term;
            } else {
                ERL_NIF_TERM ret_term = error(env, "error happened when adding OpenCV runtime DLL path, but cannot get formatted error message");
                return ret_term;
            }
        }
        opencv_path_updated = true;
    }

    if (!cuda_path_updated) {
        WCHAR CUDARuntimeDIRBuffer[65536];
        memset(CUDARuntimeDIRBuffer, 0, sizeof(CUDARuntimeDIRBuffer));
        DWORD cudaLen = GetEnvironmentVariableW(L"EVISION_CUDA_RUNTIME_DIR", CUDARuntimeDIRBuffer, 65536);

        if (cudaLen > 0) {
            WCHAR pathBuffer[65536];
            DWORD pathLen = GetEnvironmentVariableW(L"PATH", pathBuffer, 65536);

            WCHAR newPath[65536];
            newPath[0] = L'\0';
            wcscpy_s(newPath, _countof(newPath), (const wchar_t*)pathBuffer);
            wcscat_s(newPath, _countof(newPath), (const wchar_t*)L";");
            wcscat_s(newPath, _countof(newPath), (const wchar_t*)CUDARuntimeDIRBuffer);

            SetEnvironmentVariableW(L"PATH", newPath);
            SetDefaultDllDirectories(LOAD_LIBRARY_SEARCH_DEFAULT_DIRS | LOAD_LIBRARY_SEARCH_USER_DIRS);
            DLL_DIRECTORY_COOKIE ret = AddDllDirectory(CUDARuntimeDIRBuffer);
            if (ret == 0) {
                DWORD last_error = GetLastError();
                LPTSTR error_text = nullptr;
                FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, HRESULT_FROM_WIN32(last_error), MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR)&error_text, 0, NULL);
                if (error_text != nullptr) {
                    ERL_NIF_TERM ret_term = error(env, error_text);
                    LocalFree(error_text);
                    return ret_term;
                } else {
                    ERL_NIF_TERM ret_term = error(env, "error happened when adding CUDA runtime DLL path, but cannot get formatted error message");
                    return ret_term;
                }
            }
        } else {
#ifdef EVISION_ENABLE_CUDA
            printf("Evision is compiled with CUDA support, but CUDA runtime directory is not set.\r\nPlease set EVISION_CUDA_RUNTIME_DIR environment variable to the directory containing CUDA runtime DLLs.\r\n\r\n");
            printf("  If you're using cmd.exe, the command should be something like:\r\n\r\n");
            printf("    set EVISION_CUDA_RUNTIME_DIR=C:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\v12.1\\bin\r\n\r\n");
            printf("  Please replace the path with the actual path to CUDA runtime DLLs on your system.\r\n");
            printf("  Also, please DO NOT QUOTE even if there are spaces in the path.\r\n\r\n");
            printf("  If you're using PowerShell, the command should be something like:\r\n\r\n");
            printf("    $Env:EVISION_CUDA_RUNTIME_DIR=\"C:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\v12.1\\bin\"\r\n\r\n");
            printf("  Please replace the path with the actual path to CUDA runtime DLLs on your system.\r\n");
            printf("  However, please DO ADD QUOTE if there are spaces in the path.\r\n\r\n");
#endif
        }
        cuda_path_updated = true;
    }
    return ok(env);
}

static int on_load(ErlNifEnv *,void **,ERL_NIF_TERM) {
    return 0;
}

static int on_upgrade(ErlNifEnv*, void**, void**, ERL_NIF_TERM) {
    return 0;
}

static ErlNifFunc nif_functions[] = {
    {"run_once", 0, evision_windows_fix_run_once, 0},
};

ERL_NIF_INIT(evision_windows_fix, nif_functions, on_load, NULL, on_upgrade, NULL);
