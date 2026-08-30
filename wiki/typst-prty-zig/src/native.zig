const std = @import("std");
const planner = @import("planner");

pub fn main(init: std.process.Init) !void {
    var args = try std.process.Args.Iterator.initAllocator(init.minimal.args, init.gpa);
    defer args.deinit();
    _ = args.skip();
    const path = args.next() orelse return error.MissingRequestPath;
    const iterations = if (args.next()) |text|
        try std.fmt.parseInt(usize, text, 10)
    else
        1;
    if (args.next() != null) return error.TooManyArguments;

    const input = try std.Io.Dir.cwd().readFileAlloc(
        init.io,
        path,
        init.gpa,
        .limited(std.math.maxInt(u32)),
    );
    defer init.gpa.free(input);

    for (0..iterations) |_| {
        const output = try planner.layoutCbor(init.gpa, input);
        std.mem.doNotOptimizeAway(output);
        init.gpa.free(output);
    }
}
