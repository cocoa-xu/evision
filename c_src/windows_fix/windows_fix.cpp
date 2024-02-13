#include <erl_nif.h>
#include <string>
#include <cstring>
#include <stdlib.h>
#include <sstream>
#include <vector>
#include <stdio.h>
#include "../nif_utils.hpp"

#if defined(_WIN32) || defined(WIN32) || defined(__CYGWIN__) || defined(__MINGW32__) || defined(__BORLANDC__)
#define OS_WIN
#endif

#ifdef OS_WIN
#include <windows.h>
#include <libloaderapi.h>
#include <winbase.h>
#include <wchar.h>
#endif

#ifdef __cplusplus
extern "C"
{
#endif
    #ifdef OS_WIN
    int evision_windows_fix_dir_exists(LPCWSTR path) {
        DWORD dwAttrib = GetFileAttributesW(path);
        return (dwAttrib != INVALID_FILE_ATTRIBUTES && (dwAttrib & FILE_ATTRIBUTE_DIRECTORY));
    }

	static ERL_NIF_TERM evision_windows_fix_run_once(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        static bool opencv_path_updated = false;
        static bool cuda_path_updated = false;

        if (opencv_path_updated && cuda_path_updated) return evision::nif::ok(env);

        if (!opencv_path_updated) {
            std::wstring directory;
            wchar_t path[65536];
            HMODULE hm = NULL;
            if (GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT, (LPCWSTR)&evision_windows_fix_run_once, &hm) == 0) {
                int ret = GetLastError();
                fprintf(stderr, "GetModuleHandle failed, error = %d\r\n", ret);
            }

            if (GetModuleFileNameW(hm, (LPWSTR)path, sizeof(path)) == 0) {
                int ret = GetLastError();
                fprintf(stderr, "GetModuleFileName failed, error = %d\r\n", ret);
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
                    t << priv_dir + L"\\x64\\vc";
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
                ERL_NIF_TERM ret_term = evision::nif::error(env, "Cannot detect OpenCV lib directory");
                return ret_term;
            }

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
                DWORD error = GetLastError();
                LPTSTR error_text = nullptr;
                FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, HRESULT_FROM_WIN32(error), MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR)&error_text, 0, NULL);
                if (error_text != nullptr) {
                    ERL_NIF_TERM ret_term = evision::nif::error(env, error_text);
                    LocalFree(error_text);
                    return ret_term;
                } else {
                    ERL_NIF_TERM ret_term = evision::nif::error(env, "error happened when adding OpenCV runtime DLL path, but cannot get formatted error message");
                    return ret_term;
                }
            }
            opencv_path_updated = true;
            printf("added opencv lib dir: %ws\r\n", opencvLibDirectoryPCWSTR);
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
                    DWORD error = GetLastError();
                    LPTSTR error_text = nullptr;
                    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, HRESULT_FROM_WIN32(error), MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR)&error_text, 0, NULL);
                    if (error_text != nullptr) {
                        ERL_NIF_TERM ret_term = evision::nif::error(env, error_text);
                        LocalFree(error_text);
                        return ret_term;
                    } else {
                        ERL_NIF_TERM ret_term = evision::nif::error(env, "error happened when adding CUDA runtime DLL path, but cannot get formatted error message");
                        return ret_term;
                    }
                }
            }
            cuda_path_updated = true;
        }
        return evision::nif::ok(env);
	}
    #else
    static ERL_NIF_TERM evision_windows_fix_run_once(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        return evision::nif::ok(env);
    }
    #endif

	static int on_load(ErlNifEnv *,void **,ERL_NIF_TERM) {
		return 0;
	}

	static int on_reload(ErlNifEnv*, void**, ERL_NIF_TERM) {
		return 0;
	}

	static int on_upgrade(ErlNifEnv*, void**, void**, ERL_NIF_TERM) {
		return 0;
	}

	static ErlNifFunc nif_functions[] = {
		{"run_once", 0, evision_windows_fix_run_once, 0},
	};
	
	ERL_NIF_INIT(evision_windows_fix, nif_functions, on_load, on_reload, on_upgrade, NULL);
#ifdef __cplusplus
}
#endif
