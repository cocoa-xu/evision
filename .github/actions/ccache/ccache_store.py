#!/usr/bin/env python3
import hashlib
import io
import json
import os
import shutil
import subprocess
import sys
import tarfile
import time
import urllib.error
import urllib.request
import zipfile

CCACHE_VERSION = "4.14.3"
CCACHE_BINARIES = {
    ("Linux", "X64"): ("linux-x86_64-musl-static.tar.gz", "e4583fd653b794a480b9915358679a75cdd7a7010dd865eaf4cbdc61e665a3f0"),
    ("Linux", "ARM64"): ("linux-aarch64-musl-static.tar.gz", "4b0ecece7929cbaf548ff1ab9a24074b240132ab8176bc3865a27d88c457a0d8"),
    ("macOS", "X64"): ("darwin.tar.gz", "764dd2a8100ff83e6bbd7b9a1e2016cc401be34bee8a70ea99698186622c34d5"),
    ("macOS", "ARM64"): ("darwin.tar.gz", "764dd2a8100ff83e6bbd7b9a1e2016cc401be34bee8a70ea99698186622c34d5"),
    ("Windows", "X64"): ("windows-x86_64.zip", "c845c3d65be81954c75659145a522a793a27586bf940c5dcfe18af33fa1a67dd"),
}
RELEASE_TAG = "ccache"
API = os.environ.get("GITHUB_API_URL", "https://api.github.com")
WINDOWS = os.environ["RUNNER_OS"] == "Windows"


def ccache_conf():
    # Direct mode can miss a newly added header that shadows an included one, so
    # every lookup hashes the preprocessed source. macOS's /usr/bin/c++ is the same
    # shim for every Xcode, so `-v` identifies the compiler there; cl.exe has no `-v`.
    compiler_check = "content" if WINDOWS else "%compiler% -v"
    return (
        "direct_mode = false\n"
        f"compiler_check = {compiler_check}\n"
        "inode_cache = false\n"
        "max_size = 0\n"
    )


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


def github(method, url, data=None, headers=None, redirect=True):
    if not url.startswith("https://"):
        url = f"{API}/repos/{os.environ['GITHUB_REPOSITORY']}/{url}"
    request = urllib.request.Request(
        url,
        data=data,
        method=method,
        headers={
            "Authorization": f"Bearer {os.environ['GITHUB_TOKEN']}",
            "Accept": "application/vnd.github+json",
            "X-GitHub-Api-Version": "2022-11-28",
            **(headers or {}),
        },
    )
    handlers = [] if redirect else [NoRedirect]
    return urllib.request.build_opener(*handlers).open(request, timeout=900)


def paginate(url):
    page = 1
    while True:
        with github("GET", f"{url}?per_page=100&page={page}") as response:
            items = json.load(response)
        yield from items
        if len(items) < 100:
            return
        page += 1


def find_release():
    for release in paginate("releases"):
        if release["draft"] and release["tag_name"] == RELEASE_TAG:
            return release
    raise RuntimeError(f"no draft release tagged {RELEASE_TAG}")


def find_asset(release, name):
    for asset in paginate(f"releases/{release['id']}/assets"):
        if asset["name"] == name:
            return asset
    return None


def download_asset(asset, path):
    try:
        response = github(
            "GET",
            f"releases/assets/{asset['id']}",
            headers={"Accept": "application/octet-stream"},
            redirect=False,
        )
    except urllib.error.HTTPError as error:
        if error.code not in (301, 302, 303, 307, 308):
            raise
        response = urllib.request.urlopen(error.headers["Location"], timeout=900)
    with response, open(path, "wb") as out:
        shutil.copyfileobj(response, out, 1 << 20)


def upload_asset(release, path, name):
    old = find_asset(release, name)
    if old is not None:
        github("DELETE", f"releases/assets/{old['id']}").close()
    upload_url = release["upload_url"].split("{")[0]
    with open(path, "rb") as body:
        github(
            "POST",
            f"{upload_url}?name={name}",
            data=body,
            headers={
                "Content-Type": "application/x-tar",
                "Content-Length": str(os.path.getsize(path)),
            },
        ).close()


def install(bin_dir):
    exe = os.path.join(bin_dir, "ccache.exe" if WINDOWS else "ccache")
    if os.path.exists(exe):
        return exe
    suffix, sha256 = CCACHE_BINARIES[(os.environ["RUNNER_OS"], os.environ["RUNNER_ARCH"])]
    url = f"https://github.com/ccache/ccache/releases/download/v{CCACHE_VERSION}/ccache-{CCACHE_VERSION}-{suffix}"
    with urllib.request.urlopen(url, timeout=300) as response:
        data = response.read()
    if hashlib.sha256(data).hexdigest() != sha256:
        raise RuntimeError(f"checksum mismatch for {url}")
    member = f"ccache-{CCACHE_VERSION}-{suffix.split('.')[0]}/{os.path.basename(exe)}"
    if suffix.endswith(".zip"):
        binary = zipfile.ZipFile(io.BytesIO(data)).read(member)
    else:
        with tarfile.open(fileobj=io.BytesIO(data), mode="r:gz") as tar:
            binary = tar.extractfile(member).read()
    os.makedirs(bin_dir, exist_ok=True)
    with open(exe, "wb") as out:
        out.write(binary)
    os.chmod(exe, 0o755)
    return exe


def ccache(exe, cache_dir, *args, capture=False):
    return subprocess.run(
        [exe, *args],
        check=True,
        env={**os.environ, "CCACHE_DIR": cache_dir},
        stdout=subprocess.PIPE if capture else None,
        universal_newlines=True,
    ).stdout


def append(github_file, line):
    with open(os.environ[github_file], "a", encoding="utf-8") as out:
        out.write(line + "\n")


def extract(archive, cache_dir):
    with tarfile.open(archive) as tar:
        for member in tar.getmembers():
            if member.name.startswith("/") or ".." in member.name.split("/") or not (member.isfile() or member.isdir()):
                raise RuntimeError(f"unexpected archive entry {member.name}")
        if hasattr(tarfile, "data_filter"):
            tar.extractall(cache_dir, filter="data")
        else:
            tar.extractall(cache_dir)


def restore(name, cache_dir, bin_dir):
    exe = install(bin_dir)
    append("GITHUB_PATH", bin_dir)
    shutil.rmtree(cache_dir, ignore_errors=True)
    os.makedirs(cache_dir)
    archive = cache_dir + ".tar"
    try:
        asset = find_asset(find_release(), name + ".tar")
        if asset is None:
            print(f"No stored ccache named {name}; compiling everything.")
        else:
            started = time.time()
            download_asset(asset, archive)
            extract(archive, cache_dir)
            print(f"Restored ccache {name} ({os.path.getsize(archive)} bytes) in {time.time() - started:.0f}s.")
    except Exception as error:
        print(f"::warning::Could not restore ccache {name}, compiling everything: {error}")
        shutil.rmtree(cache_dir, ignore_errors=True)
        os.makedirs(cache_dir)
    finally:
        if os.path.exists(archive):
            os.remove(archive)
    with open(os.path.join(cache_dir, "ccache.conf"), "w", encoding="utf-8") as out:
        out.write(ccache_conf())
    append("GITHUB_ENV", f"CCACHE_DIR={cache_dir}")
    append("GITHUB_ENV", f"CCACHE_STORE_STARTED_AT={int(time.time())}")
    ccache(exe, cache_dir, "--zero-stats")


def save(name, cache_dir, bin_dir):
    archive = cache_dir + ".tar"
    try:
        exe = install(bin_dir)
        ccache(exe, cache_dir, "--show-stats")
        stats = dict(line.split("\t") for line in ccache(exe, cache_dir, "--print-stats", capture=True).splitlines())
        lookups = sum(int(stats[key]) for key in ("cache_miss", "direct_cache_hit", "preprocessed_cache_hit"))
        if lookups == 0:
            print(f"::warning::ccache {name} was not used in this job; keeping the stored cache.")
            return
        # Hits refresh an entry's mtime, so this drops only what this job didn't use.
        age = int(time.time()) - int(os.environ["CCACHE_STORE_STARTED_AT"]) + 600
        ccache(exe, cache_dir, "--evict-older-than", f"{age}s")
        with tarfile.open(archive, "w") as tar:
            for entry in sorted(os.listdir(cache_dir)):
                if entry != "tmp":
                    tar.add(os.path.join(cache_dir, entry), arcname=entry)
        started = time.time()
        upload_asset(find_release(), archive, name + ".tar")
        print(f"Saved ccache {name} ({os.path.getsize(archive)} bytes) in {time.time() - started:.0f}s.")
    except Exception as error:
        print(f"::warning::Could not save ccache {name}: {error}")
    finally:
        if os.path.exists(archive):
            os.remove(archive)


def main():
    mode, name, cache_dir = sys.argv[1:]
    cache_dir = os.path.abspath(cache_dir)
    bin_dir = cache_dir + "-bin"
    if mode == "restore":
        restore(name, cache_dir, bin_dir)
    elif mode == "save":
        save(name, cache_dir, bin_dir)
    else:
        raise SystemExit(f"unknown mode {mode}")


if __name__ == "__main__":
    main()
