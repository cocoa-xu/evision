// Prove an opencv.js build is CSP-clean.
//
// Run under Node's --disallow-code-generation-from-strings, which makes
// eval()/new Function() throw EvalError exactly as a Content-Security-Policy
// without 'unsafe-eval' does in a browser. WebAssembly instantiation is not
// blocked by that flag, mirroring a policy that still allows 'wasm-unsafe-eval'.
//
// If embind still needed string->code to build its invokers, initialization
// would throw. Reaching a working cv.Mat proves the runtime is clean.
//
// Usage:
//   node [--disallow-code-generation-from-strings] verify_opencv_js_csp.mjs <opencv.js>
//
// With REQUIRE_NO_EVAL=1, assert that code generation really is disabled, so a
// strict check fails loudly if the node flag is ever dropped.

import { createRequire } from "node:module";
import { resolve } from "node:path";

function die(err) {
  const detail = err?.stack ?? String(err);
  const evalRelated = /code generation from strings|EvalError|new Function/i.test(detail);
  console.error(`FAIL${evalRelated ? " (eval/CSP)" : ""}: ${detail}`);
  process.exit(1);
}

// opencv.js may abort on a later tick (embind/emscripten throw asynchronously);
// route those to the same failure path rather than a bare stack trace.
process.on("uncaughtException", die);
process.on("unhandledRejection", die);

const target = process.argv[2];
if (!target) {
  console.error("usage: node verify_opencv_js_csp.mjs <opencv.js>");
  process.exit(2);
}

// Whether this process forbids eval()/new Function() -- the CSP posture we assert.
const codegenDisabled = (() => {
  try {
    new Function("");
    return false;
  } catch {
    return true;
  }
})();

if (process.env.REQUIRE_NO_EVAL === "1" && !codegenDisabled) {
  die("REQUIRE_NO_EVAL=1 but eval is allowed; pass --disallow-code-generation-from-strings");
}

// opencv.js is a UMD/CommonJS bundle, so require() it. resolve() against cwd so a
// caller-relative path ("out/opencv.js") is found, not one relative to this file.
const require = createRequire(import.meta.url);

// OpenCV 5.0's modules/js/CMakeLists.txt sets MODULARIZE=1 / EXPORT_NAME='cv', so
// the export is a factory returning a promise; tolerate the older shapes too.
function initialize(exported) {
  const mod = typeof exported === "function" ? exported() : exported;
  if (mod && typeof mod.then === "function") return mod;
  if (mod && mod.Mat) return Promise.resolve(mod);
  if (mod) return new Promise((res) => (mod.onRuntimeInitialized = () => res(mod)));
  return Promise.reject(new Error("opencv.js export was empty"));
}

// A live timer keeps the event loop alive, so a module that never initializes
// fails here instead of letting the process exit 0 without verifying anything.
function withTimeout(promise, ms) {
  return new Promise((res, rej) => {
    const timer = setTimeout(() => rej(new Error(`timed out after ${ms}ms waiting for init`)), ms);
    promise.then(
      (value) => {
        clearTimeout(timer);
        res(value);
      },
      (err) => {
        clearTimeout(timer);
        rej(err);
      },
    );
  });
}

async function main() {
  const cv = await withTimeout(initialize(require(resolve(target))), 120_000);

  const mat = new cv.Mat(3, 3, cv.CV_8UC1); // exercises an embind constructor invoker
  try {
    if (mat.rows !== 3) throw new Error(`unexpected Mat.rows: ${mat.rows}`);
  } finally {
    mat.delete();
  }

  const posture = codegenDisabled ? "disabled" : "allowed";
  console.log(`PASS: opencv.js initialized and embind Mat works (code generation from strings ${posture})`);
}

main().catch(die);
