workspace(
    name = "servant_elm_bazel",
    managed_directories = {
        "@npm": ["node_modules"],
    },
)

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "rules_haskell",
    sha256 = "e258692e7e4266c71ad8ef77c8179a8dba179f0854121008827189764f571625",
    strip_prefix = "rules_haskell-57081449c34c0a3de429759837f61a0b70a8f3e2",
    urls = ["https://github.com/tweag/rules_haskell/archive/57081449c34c0a3de429759837f61a0b70a8f3e2.tar.gz"],
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load("@rules_haskell//haskell:toolchain.bzl", "rules_haskell_toolchains")

rules_haskell_toolchains()

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

http_archive(
    name = "zlib.hs",
    build_file_content = """\
load("@rules_cc//cc:defs.bzl", "cc_library")
cc_library(
    name = "zlib",
    srcs = [":z"],
    hdrs = glob(["*.h"]),
    includes = ["."],
    linkstatic = 1,
    visibility = ["//visibility:public"],
)
cc_library(
    name = "z",
    srcs = glob(["*.c"]),
    hdrs = glob(["*.h"]),
    copts = ["-w"],
    # Work around `ld -rpath` error on MacOS.
    linkstatic = 1,
)
""",
    sha256 = "c3e5e9fdd5004dcb542feda5ee4f0ff0744628baf8ed2dd5d66f8ca1197cb1a1",
    strip_prefix = "zlib-1.2.11",
    urls = ["http://zlib.net/zlib-1.2.11.tar.gz"],
)

stack_snapshot(
    name = "stackage",
    extra_deps = {"zlib": ["@zlib.hs//:zlib"]},
    packages = [
        "aeson",
        "aeson-casing",
        "base",
        "containers",
        "filepath",
        "hspec",
        "hspec-expectations",
        "http-client",
        "optparse-applicative",
        "servant-0.17",
        "servant-client-0.17",
        "servant-client-core-0.17",
        "servant-elm-0.7.1",
        "servant-foreign-0.15.1",
        "servant-server-0.17",
        "stm",
        "tar",
        "temporary",
        "text",
        "wai-app-static",
        "wai-logger",
        "warp",
    ],
    snapshot = "lts-14.27",
)

http_archive(
    name = "build_bazel_rules_nodejs",
    sha256 = "d0c4bb8b902c1658f42eb5563809c70a06e46015d64057d25560b0eb4bdc9007",
    urls = ["https://github.com/bazelbuild/rules_nodejs/releases/download/1.5.0/rules_nodejs-1.5.0.tar.gz"],
)

load("@build_bazel_rules_nodejs//:index.bzl", "node_repositories", "yarn_install")

node_repositories()

yarn_install(
    name = "npm",
    package_json = "//:package.json",
    yarn_lock = "//:yarn.lock",
)

http_archive(
    name = "com_github_edschouten_rules_elm",
    sha256 = "bd1f6b808c9b54d6190cf252ae1a5e0ddb88302eb5650a45b42389f684406bab",
    strip_prefix = "rules_elm-6548399afde6690dc72f7cabdd29e7381c9ec508",
    urls = ["https://github.com/EdSchouten/rules_elm/archive/6548399afde6690dc72f7cabdd29e7381c9ec508.tar.gz"],
)

load("@com_github_edschouten_rules_elm//elm:deps.bzl", "elm_register_toolchains")

elm_register_toolchains()

load("@com_github_edschouten_rules_elm//repository:def.bzl", "elm_repository")

elm_repository(
    name = "elm_package_bartavelle_json_helpers",
    sha256 = "d616cc7118b71ef17a2fade01a0612ceb0796743893aa8882432602b54c6264d",
    strip_prefix = "json-helpers-2.0.2",
    urls = ["https://github.com/bartavelle/json-helpers/archive/2.0.2.tar.gz"],
)

elm_repository(
    name = "elm_package_elm_browser",
    sha256 = "23f41491d325afc72649d512741fb8173725014c93e482d25bab3325555a4f59",
    strip_prefix = "browser-1.0.2",
    urls = ["https://github.com/elm/browser/archive/1.0.2.tar.gz"],
)

elm_repository(
    name = "elm_package_elm_bytes",
    sha256 = "922f3526e3b430e947d1d2eac5965e4caae625013649de2f657d4f258a5bdc0b",
    strip_prefix = "bytes-1.0.8",
    urls = ["https://github.com/elm/bytes/archive/1.0.8.tar.gz"],
)

elm_repository(
    name = "elm_package_elm_core",
    sha256 = "6e37b11c88c89a68d19d0c7625f1ef39ed70c59e443def95e4de98d6748c80a7",
    strip_prefix = "core-1.0.5",
    urls = ["https://github.com/elm/core/archive/1.0.5.tar.gz"],
)

elm_repository(
    name = "elm_package_elm_file",
    sha256 = "c85b4025e12c1bf2dee9e4d853459ead7d1fa917304adfa2af27d116c86292e6",
    strip_prefix = "file-1.0.5",
    urls = ["https://github.com/elm/file/archive/1.0.5.tar.gz"],
)

elm_repository(
    name = "elm_package_elm_html",
    sha256 = "73b885e0a3d2f9781b1c9bbcc1ee9ac032f503f5ef46a27da3ba617cebbf6fd8",
    strip_prefix = "html-1.0.0",
    urls = ["https://github.com/elm/html/archive/1.0.0.tar.gz"],
)

elm_repository(
    name = "elm_package_elm_http",
    sha256 = "619bc23d7753bc172016ea764233dd7dfded1d919263c41b59885c5bcdd10b01",
    strip_prefix = "http-2.0.0",
    urls = ["https://github.com/elm/http/archive/2.0.0.tar.gz"],
)

elm_repository(
    name = "elm_package_elm_json",
    sha256 = "d0635f33137e4ad3fc323f96ba280e45dc41afa51076c53d9f04fd92c2cf5c4e",
    strip_prefix = "json-1.1.3",
    urls = ["https://github.com/elm/json/archive/1.1.3.tar.gz"],
)

elm_repository(
    name = "elm_package_elm_svg",
    sha256 = "13d8c9a129421be95bf75d8cc3ddf05e5eafc6df79e3c7312f040773c7918cb3",
    strip_prefix = "svg-1.0.1",
    urls = ["https://github.com/elm/svg/archive/1.0.1.tar.gz"],
)

elm_repository(
    name = "elm_package_elm_time",
    sha256 = "e18bca487adec67bfe4043a33b975d81527a7732377050d0421dd86d503c906d",
    strip_prefix = "time-1.0.0",
    urls = ["https://github.com/elm/time/archive/1.0.0.tar.gz"],
)

elm_repository(
    name = "elm_package_elm_url",
    sha256 = "840e9d45d8a9bd64a7f76421a1de2518e02c7cbea7ed42efd380b4e875e9682b",
    strip_prefix = "url-1.0.0",
    urls = ["https://github.com/elm/url/archive/1.0.0.tar.gz"],
)

elm_repository(
    name = "elm_package_elm_virtual_dom",
    sha256 = "cf87286ed5d1b31aaf99c6a3368ccd340d1356b1973f1afe5f668c47e22b3b60",
    strip_prefix = "virtual-dom-1.0.2",
    urls = ["https://github.com/elm/virtual-dom/archive/1.0.2.tar.gz"],
)

elm_repository(
    name = "elm_package_lattyware_fontawesome",
    sha256 = "062a98be0efd4db3e6765fee8a9169258f3b318563829d972eb7211388998f92",
    strip_prefix = "elm-fontawesome-4.0.0",
    urls = ["https://github.com/lattyware/elm-fontawesome/archive/4.0.0.tar.gz"],
)

http_archive(
    name = "rules_pkg",
    sha256 = "352c090cc3d3f9a6b4e676cf42a6047c16824959b438895a76c2989c6d7c246a",
    urls = [
        "https://github.com/bazelbuild/rules_pkg/releases/download/0.2.5/rules_pkg-0.2.5.tar.gz",
        "https://mirror.bazel.build/github.com/bazelbuild/rules_pkg/releases/download/0.2.5/rules_pkg-0.2.5.tar.gz",
    ],
)

load("@rules_pkg//:deps.bzl", "rules_pkg_dependencies")

rules_pkg_dependencies()
