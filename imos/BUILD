licenses(["notice"])

package(default_visibility = ["//visibility:public"])

sh_test(
    name = "switch-test",
    srcs = ["switch-test.sh"],
    data = [
        ":switch.php",
        "//ninetan:ninestream",
        "//bin",
    ],
)
