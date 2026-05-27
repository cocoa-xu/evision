#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Parse OpenCV's ``platforms/js/opencv_js.config.py`` whitelist.

Used by the evision codegen to learn which generated bindings have an
opencv.js counterpart so a persistent ``@js`` module attribute can be
emitted per matching function. See
``projects/evision/context/js-correspondence-tag.md`` in the
``cocoa/ai-native`` notebook for the full design.

The config file is plain Python: each OpenCV module (``core``, ``imgproc``,
``dnn``, ...) is a ``dict[classname_or_'', list[fn_name]]`` later merged
into a single flat ``white_list`` via ``makeWhiteList``. The merge loses
the per-module bucketing, so this parser executes the file with a stub
``makeWhiteList`` and walks the resulting namespace itself to keep
``module_name`` on every entry.

embindgen treats the ``cv::dnn`` and ``cv::aruco`` namespaces as
prefix-stripped when the whitelist is a ``.py`` file
(``embindgen.py:1066-1069``). The same defaults are applied here so
consumers see the JS-side function name as opencv.js would expose it.
"""

from __future__ import annotations

import os
from pathlib import Path
from typing import Dict, Iterator, List, Optional, Set, Tuple


# Hardcoded in embindgen.py for the ``.py`` whitelist mode.
DEFAULT_NAMESPACE_PREFIX_OVERRIDE: Dict[str, str] = {
    "dnn": "",
    "aruco": "",
}

ModuleWhitelist = Dict[str, List[str]]
ParsedWhitelist = Dict[str, ModuleWhitelist]
Entry = Tuple[str, Optional[str], str, str]


class ConfigNotFoundError(FileNotFoundError):
    """Raised when no opencv_js.config.py can be located."""


def discover_config_path(
    repo_root: Optional[os.PathLike] = None,
    opencv_ver: Optional[str] = None,
) -> Path:
    """Locate ``opencv_js.config.py`` for the active OpenCV checkout.

    Resolution order:

    1. ``OPENCV_VER`` arg, then ``$OPENCV_VER`` env var, then any single
       ``3rd_party/opencv/opencv-*`` directory present on disk.
    2. ``repo_root`` arg, then ``$EVISION_REPO_ROOT`` env var, then the
       repo root inferred from this file's location.

    Raises ``ConfigNotFoundError`` if neither yields a readable file.
    """
    root = Path(repo_root) if repo_root else _default_repo_root()
    ver = opencv_ver or os.environ.get("OPENCV_VER")

    if ver:
        candidate = root / "3rd_party" / "opencv" / f"opencv-{ver}" / "platforms" / "js" / "opencv_js.config.py"
        if candidate.is_file():
            return candidate
        raise ConfigNotFoundError(
            f"opencv_js.config.py not found for OPENCV_VER={ver} at {candidate}"
        )

    opencv_root = root / "3rd_party" / "opencv"
    if not opencv_root.is_dir():
        raise ConfigNotFoundError(
            f"OpenCV root {opencv_root} does not exist; pass repo_root or run `make download_opencv` first"
        )

    candidates = sorted(
        p for p in opencv_root.glob("opencv-*")
        if p.is_dir() and not p.name.startswith("opencv_contrib")
    )
    if not candidates:
        raise ConfigNotFoundError(
            f"No opencv-* checkout under {opencv_root}; set OPENCV_VER or run `make download_opencv`"
        )

    config = candidates[-1] / "platforms" / "js" / "opencv_js.config.py"
    if not config.is_file():
        raise ConfigNotFoundError(
            f"Found {candidates[-1]} but its platforms/js/opencv_js.config.py is missing"
        )
    return config


def load_modules(config_path: Optional[os.PathLike] = None) -> ParsedWhitelist:
    """Execute the config file and return its per-module whitelist dicts.

    Keys are the module variable names from the file (``core``,
    ``imgproc``, ``dnn``, ...); values are the original
    ``{classname_or_'': [fn_name, ...]}`` dicts.

    Raises ``ConfigNotFoundError`` (a subclass of ``FileNotFoundError``)
    when the path does not resolve to an existing file.
    """
    path = Path(config_path) if config_path else discover_config_path()
    if not path.is_file():
        raise ConfigNotFoundError(f"opencv_js.config.py not found at {path}")

    namespace: Dict[str, object] = {
        "makeWhiteList": _stub_make_whitelist,
        "__file__": str(path),
    }
    source = path.read_text()
    exec(compile(source, str(path), "exec"), namespace)

    modules: ParsedWhitelist = {}
    for name, value in namespace.items():
        if name.startswith("_") or name in _RESERVED_NAMES:
            continue
        if not isinstance(value, dict):
            continue
        if not all(
            isinstance(k, str) and isinstance(v, list) and all(isinstance(item, str) for item in v)
            for k, v in value.items()
        ):
            continue
        modules[name] = {k: list(v) for k, v in value.items()}
    return modules


def namespace_prefix_override(
    config_path: Optional[os.PathLike] = None,
) -> Dict[str, str]:
    """Return the effective ``namespace_prefix_override`` mapping.

    Starts from the embindgen-side ``.py`` defaults
    (``{'dnn': '', 'aruco': ''}``) and merges any value assigned to
    ``namespace_prefix_override`` inside the config file itself.
    """
    path = Path(config_path) if config_path else discover_config_path()
    if not path.is_file():
        raise ConfigNotFoundError(f"opencv_js.config.py not found at {path}")

    namespace: Dict[str, object] = {
        "makeWhiteList": _stub_make_whitelist,
        "__file__": str(path),
    }
    exec(compile(path.read_text(), str(path), "exec"), namespace)

    overrides = dict(DEFAULT_NAMESPACE_PREFIX_OVERRIDE)
    extra = namespace.get("namespace_prefix_override")
    if isinstance(extra, dict):
        for k, v in extra.items():
            if isinstance(k, str) and isinstance(v, str):
                overrides[k] = v
    return overrides


def flat_set(
    config_path: Optional[os.PathLike] = None,
    modules: Optional[ParsedWhitelist] = None,
) -> Set[Tuple[str, Optional[str], str]]:
    """Return ``{(module, classname_or_None, fn_name)}`` for fast lookup.

    Pass an already-loaded ``modules`` dict to avoid re-executing the
    config file; otherwise the file at ``config_path`` (or the discovered
    default) is loaded.
    """
    if modules is None:
        modules = load_modules(config_path)
    out: Set[Tuple[str, Optional[str], str]] = set()
    for module_name, module_dict in modules.items():
        for class_key, fn_names in module_dict.items():
            classname = class_key or None
            for fn_name in fn_names:
                out.add((module_name, classname, fn_name))
    return out


def entries(
    config_path: Optional[os.PathLike] = None,
    modules: Optional[ParsedWhitelist] = None,
) -> Iterator[Entry]:
    """Yield ``(module, classname_or_None, fn_name, js_kind)`` for the emitter.

    ``js_kind`` is one of ``'function'``, ``'method'``, ``'constructor'``.
    A class entry is treated as the constructor when its function name
    equals the bare class name (the segment after the last ``_`` in the
    class key) — this matches OpenCV's convention where
    e.g. ``'aruco_Dictionary': ['Dictionary', ...]`` lists ``Dictionary``
    as the constructor.
    """
    if modules is None:
        modules = load_modules(config_path)
    for module_name in sorted(modules):
        module_dict = modules[module_name]
        for class_key in sorted(module_dict):
            fn_names = module_dict[class_key]
            classname = class_key or None
            bare = class_key.rsplit("_", 1)[-1] if class_key else None
            for fn_name in fn_names:
                if classname is None:
                    kind = "function"
                elif fn_name == bare:
                    kind = "constructor"
                else:
                    kind = "method"
                yield (module_name, classname, fn_name, kind)


def _stub_make_whitelist(module_list):
    merged: ModuleWhitelist = {}
    for module in module_list:
        for k, v in module.items():
            merged.setdefault(k, []).extend(v)
    return merged


def _default_repo_root() -> Path:
    env = os.environ.get("EVISION_REPO_ROOT")
    if env:
        return Path(env)
    return Path(__file__).resolve().parent.parent


_RESERVED_NAMES = frozenset({
    "makeWhiteList",
    "white_list",
    "namespace_prefix_override",
})
