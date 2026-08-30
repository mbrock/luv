const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const native_target = b.standardTargetOptions(.{});

    const planner = b.addModule("planner", .{
        .root_source_file = b.path("src/planner.zig"),
        .target = native_target,
        .optimize = optimize,
    });
    const tests = b.addTest(.{ .root_module = planner });
    const test_step = b.step("test", "Run rectangle planner tests");
    test_step.dependOn(&b.addRunArtifact(tests).step);

    const wasm_target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
    });
    const wasm_planner = b.addModule("planner-wasm", .{
        .root_source_file = b.path("src/planner.zig"),
        .target = wasm_target,
        .optimize = optimize,
    });
    const plugin_module = b.createModule(.{
        .root_source_file = b.path("src/plugin.zig"),
        .target = wasm_target,
        .optimize = optimize,
        .imports = &.{.{ .name = "planner", .module = wasm_planner }},
    });
    const plugin = b.addExecutable(.{
        .name = "typst-prty",
        .root_module = plugin_module,
        .use_llvm = true,
    });
    plugin.entry = .disabled;
    plugin.rdynamic = true;
    plugin.export_memory = true;
    b.installArtifact(plugin);
}
