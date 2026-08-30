const std = @import("std");
const planner = @import("planner");

const allocator = std.heap.wasm_allocator;

extern "typst_env" fn wasm_minimal_protocol_write_args_to_buffer(ptr: [*]u8) void;
extern "typst_env" fn wasm_minimal_protocol_send_result_to_host(ptr: [*]const u8, len: usize) void;

export fn layout(input_len: usize) u32 {
    const input = allocator.alloc(u8, input_len) catch return fail("out of memory");
    defer allocator.free(input);
    wasm_minimal_protocol_write_args_to_buffer(input.ptr);

    const output = planner.layout(allocator, input) catch |err| {
        return fail(@errorName(err));
    };
    defer allocator.free(output);
    wasm_minimal_protocol_send_result_to_host(output.ptr, output.len);
    return 0;
}

fn fail(message: []const u8) u32 {
    wasm_minimal_protocol_send_result_to_host(message.ptr, message.len);
    return 1;
}
