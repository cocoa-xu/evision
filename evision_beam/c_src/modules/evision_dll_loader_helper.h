#include <string>
#include "../nif_utils.hpp"

#if defined(_WIN32) || defined(WINRT) || defined(_WIN32_WCE) || defined(__WIN32__) || defined(_MSC_VER)

#include <windows.h>
#include <libloaderapi.h>
#include <winbase.h>
#include <wchar.h>

#endif

// @evision c: dll_loader_helper,evision_dll_loader_helper_addDLLDirectory,1
// @evision nif: def _addDLLDirectory(_opts \\ []), do: :erlang.nif_error("addDLLDirectory not loaded")
static ERL_NIF_TERM evision_dll_loader_helper_addDLLDirectory(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
#if defined(_WIN32) || defined(WINRT) || defined(_WIN32_WCE) || defined(__WIN32__) || defined(_MSC_VER)
    if (argc != 1) return enif_make_badarg(env);

    std::string newDirectory;
    if (evision::nif::get(env, argv[0], newDirectory)) {
        int len;
        int slen = (int)newDirectory.length() + 1;
        len = MultiByteToWideChar(CP_ACP, 0, newDirectory.c_str(), slen, 0, 0);
        wchar_t* buf = new wchar_t[len];
        MultiByteToWideChar(CP_ACP, 0, newDirectory.c_str(), slen, buf, len);
        std::wstring newDirectoryW(buf);
        PCWSTR newDirectoryPCWSTR = newDirectoryW.c_str();
        WCHAR pathBuffer[65536];
        DWORD pathLen = GetEnvironmentVariableW(L"PATH", pathBuffer, 65536);
        WCHAR newPath[65536];
        newPath[0] = L'\0';
        wcscpy_s(newPath, _countof(newPath), (const wchar_t*)pathBuffer);
        wcscat_s(newPath, _countof(newPath), (const wchar_t*)L";");
        wcscat_s(newPath, _countof(newPath), (const wchar_t*)newDirectoryPCWSTR);
        SetEnvironmentVariableW(L"PATH", newPath);
        SetDefaultDllDirectories(LOAD_LIBRARY_SEARCH_DEFAULT_DIRS | LOAD_LIBRARY_SEARCH_USER_DIRS);
        DLL_DIRECTORY_COOKIE ret = AddDllDirectory(newDirectoryPCWSTR);
        delete[] buf;
        if (ret == 0) {
            DWORD error = GetLastError();
            LPTSTR error_text = nullptr;
            FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, HRESULT_FROM_WIN32(error), MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR)&error_text, 0, NULL);
            if (error_text != nullptr) {
                ERL_NIF_TERM ret_term = evision::nif::error(env, error_text);
                LocalFree(error_text);
                return ret_term;
            }
            else {
                ERL_NIF_TERM ret_term = evision::nif::error(env, "error happened, but cannot get formatted error message");
                return ret_term;
            }
        }
        else {
            return evision::nif::ok(env);
        }
    }
    else {
        return enif_make_badarg(env);
    }
#else
    return evision::nif::ok(env);
#endif
}
