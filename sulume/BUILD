licenses(["notice"])

package(default_visibility = ["//visibility:public"])

cc_binary(
    name = "local",
    srcs = ["local.cc"],
    deps = [
        "//external:base",
        "//ninetan:ninestream_util",
        ":json11",
    ],
)

cc_library(
    name = "json11",
    srcs = ["json11.cpp"],
    hdrs = ["json11.hpp"],
)
