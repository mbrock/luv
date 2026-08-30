const std = @import("std");

// Typst supplies a semantic tree whose leaves are measured proportional text
// boxes.  This module owns everything combinatorial: analytic row, column, and
// frame bounds; bounded Pareto frontiers; and selection under a width limit.
// Plans use 32-bit arena indices while searching.  The winner becomes a flat
// CBOR display list of absolute [x, y, drawable-id] placements.

const Size = struct {
    width: f64,
    height: f64,
    id: u32 = 0,
};

const Kind = enum {
    leaf,
    row,
    column,
    row_column,
    prefer_row,
    call,
    frame,
};

const Node = struct {
    kind: Kind,
    variants: []const Size = &.{},
    children: []const Node = &.{},
    gap: f64 = 0,
    row_gap: f64 = 0,
    column_gap: f64 = 0,
    inset_x: f64 = 0,
    inset_y: f64 = 0,
};

const Request = struct {
    limit: f64,
    max_frontier: u8 = 8,
    first_frame: u32 = 0,
    root: Node,
};

const PlanTag = enum {
    empty,
    leaf,
    row,
    column,
    frame,
};

const Plan = struct {
    tag: PlanTag,
    left: u32 = 0,
    right: u32 = 0,
    drawable: u32 = 0,
    gap: f64 = 0,
    width: f64 = 0,
    height: f64 = 0,
};

const Frame = Size;

const Placement = struct {
    x: f64,
    y: f64,
    drawable: u32,
};

const Candidate = struct {
    width: f64,
    height: f64,
    cost: u32 = 0,
    plan: u32,
};

const Planner = struct {
    allocator: std.mem.Allocator,
    limit: f64,
    max_frontier: usize,
    plans: std.ArrayList(Plan) = .empty,

    fn addPlan(self: *Planner, plan: Plan) !u32 {
        const index: u32 = @intCast(self.plans.items.len);
        try self.plans.append(self.allocator, plan);
        return index;
    }

    fn one(self: *Planner, candidate: Candidate) ![]Candidate {
        const result = try self.allocator.alloc(Candidate, 1);
        result[0] = candidate;
        return result;
    }

    fn empty(self: *Planner) ![]Candidate {
        return self.one(.{
            .width = 0,
            .height = 0,
            .plan = try self.addPlan(.{ .tag = .empty, .width = 0, .height = 0 }),
        });
    }

    fn eval(self: *Planner, node: Node) anyerror![]Candidate {
        return switch (node.kind) {
            .leaf => self.evalLeaf(node),
            .row => self.composeChildren(node.children, .row, node.gap),
            .column => self.composeChildren(node.children, .column, node.gap),
            .row_column => self.evalRowColumn(node),
            .prefer_row => self.evalPreferRow(node),
            .call => self.evalCall(node),
            .frame => self.evalFrame(node),
        };
    }

    fn evalLeaf(self: *Planner, node: Node) ![]Candidate {
        if (node.variants.len == 0) return error.LeafHasNoVariants;
        var candidates: std.ArrayList(Candidate) = .empty;
        for (node.variants) |variant| {
            if (!std.math.isFinite(variant.width) or !std.math.isFinite(variant.height) or
                variant.width < 0 or variant.height < 0)
                return error.InvalidRectangle;
            try candidates.append(self.allocator, .{
                .width = variant.width,
                .height = variant.height,
                .plan = try self.addPlan(.{
                    .tag = .leaf,
                    .drawable = variant.id,
                    .width = variant.width,
                    .height = variant.height,
                }),
            });
        }
        return self.pareto(candidates.items);
    }

    fn evalChildren(self: *Planner, children: []const Node) ![][]Candidate {
        const frontiers = try self.allocator.alloc([]Candidate, children.len);
        for (children, 0..) |child, index|
            frontiers[index] = try self.eval(child);
        return frontiers;
    }

    fn composeChildren(self: *Planner, children: []const Node, tag: PlanTag, gap: f64) ![]Candidate {
        return self.compose(try self.evalChildren(children), tag, gap);
    }

    fn compose(self: *Planner, frontiers: []const []Candidate, tag: PlanTag, gap: f64) ![]Candidate {
        if (tag != .row and tag != .column) return error.InvalidComposition;
        if (frontiers.len == 0) return self.empty();

        var candidates = frontiers[0];
        for (frontiers[1..]) |right_frontier| {
            var next: std.ArrayList(Candidate) = .empty;
            for (candidates) |left| {
                for (right_frontier) |right| {
                    const width = if (tag == .row)
                        left.width + gap + right.width
                    else
                        @max(left.width, right.width);
                    const height = if (tag == .row)
                        @max(left.height, right.height)
                    else
                        left.height + gap + right.height;
                    try next.append(self.allocator, .{
                        .width = width,
                        .height = height,
                        .cost = left.cost + right.cost,
                        .plan = try self.addPlan(.{
                            .tag = tag,
                            .left = left.plan,
                            .right = right.plan,
                            .gap = gap,
                            .width = width,
                            .height = height,
                        }),
                    });
                }
            }
            candidates = try self.restrict(next.items);
        }
        return candidates;
    }

    fn evalRowColumn(self: *Planner, node: Node) ![]Candidate {
        const children = try self.evalChildren(node.children);
        const horizontal = try self.compose(children, .row, node.row_gap);
        const vertical = try self.compose(children, .column, node.column_gap);
        return self.choose(&.{ horizontal, vertical });
    }

    fn evalPreferRow(self: *Planner, node: Node) ![]Candidate {
        const children = try self.evalChildren(node.children);
        const horizontal = try self.compose(children, .row, node.row_gap);
        if (hasFitting(horizontal, self.limit)) return horizontal;
        return self.compose(children, .column, node.column_gap);
    }

    fn evalCall(self: *Planner, node: Node) ![]Candidate {
        const children = try self.evalChildren(node.children);
        if (children.len == 0) return self.empty();
        if (children.len == 1) return children[0];

        const arguments = children[1..];
        const arrangement = try self.choose(&.{
            try self.compose(arguments, .row, node.row_gap),
            try self.compose(arguments, .column, node.column_gap),
        });
        const beside = try self.compose(&.{ children[0], arrangement }, .row, node.row_gap);
        if (hasFitting(beside, self.limit)) return beside;
        return self.compose(children, .column, node.column_gap);
    }

    fn evalFrame(self: *Planner, node: Node) ![]Candidate {
        if (node.children.len != 1) return error.FrameNeedsOneChild;
        const children = try self.eval(node.children[0]);
        var framed: std.ArrayList(Candidate) = .empty;
        for (children) |child| {
            const width = child.width + 2 * node.inset_x;
            const height = child.height + 2 * node.inset_y;
            try framed.append(self.allocator, .{
                .width = width,
                .height = height,
                .cost = child.cost,
                .plan = try self.addPlan(.{
                    .tag = .frame,
                    .left = child.plan,
                    .width = width,
                    .height = height,
                }),
            });
        }
        return self.restrict(framed.items);
    }

    fn choose(self: *Planner, frontiers: []const []Candidate) ![]Candidate {
        var candidates: std.ArrayList(Candidate) = .empty;
        for (frontiers) |frontier|
            try candidates.appendSlice(self.allocator, frontier);
        return self.pareto(candidates.items);
    }

    fn restrict(self: *Planner, candidates: []const Candidate) ![]Candidate {
        if (hasFitting(candidates, self.limit)) {
            var fitting: std.ArrayList(Candidate) = .empty;
            for (candidates) |candidate|
                if (candidate.width <= self.limit)
                    try fitting.append(self.allocator, candidate);
            return self.pareto(fitting.items);
        }
        return self.pareto(candidates);
    }

    fn pareto(self: *Planner, candidates: []const Candidate) ![]Candidate {
        var kept: std.ArrayList(Candidate) = .empty;
        for (candidates) |candidate| {
            var rejected = false;
            for (kept.items) |other| {
                if (dominates(other, candidate)) {
                    rejected = true;
                    break;
                }
            }
            if (rejected) continue;

            var index: usize = 0;
            while (index < kept.items.len) {
                if (dominates(candidate, kept.items[index]))
                    _ = kept.swapRemove(index)
                else
                    index += 1;
            }
            try kept.append(self.allocator, candidate);
        }
        if (kept.items.len <= self.max_frontier)
            return kept.toOwnedSlice(self.allocator);

        std.mem.sort(Candidate, kept.items, {}, struct {
            fn lessThan(_: void, a: Candidate, b: Candidate) bool {
                return a.width < b.width;
            }
        }.lessThan);
        const bounded = try self.allocator.alloc(Candidate, self.max_frontier);
        if (self.max_frontier == 1) {
            bounded[0] = kept.items[0];
        } else {
            for (0..self.max_frontier) |sample| {
                const index = sample * (kept.items.len - 1) / (self.max_frontier - 1);
                bounded[sample] = kept.items[index];
            }
        }
        return bounded;
    }

    fn best(self: *Planner, candidates: []const Candidate) !Candidate {
        var winner: ?Candidate = null;
        for (candidates) |candidate| {
            if (candidate.width > self.limit) continue;
            if (winner == null or better(candidate, winner.?)) winner = candidate;
        }
        return winner orelse error.NoLayoutFits;
    }

    fn flattenPlan(
        self: *Planner,
        index: u32,
        x: f64,
        y: f64,
        first_frame: u32,
        frames: *std.ArrayList(Frame),
        placements: *std.ArrayList(Placement),
    ) anyerror!void {
        const plan = self.plans.items[index];
        switch (plan.tag) {
            .empty => {},
            .leaf => try placements.append(self.allocator, .{
                .x = x,
                .y = y,
                .drawable = plan.drawable,
            }),
            .row => {
                const left = self.plans.items[plan.left];
                try self.flattenPlan(plan.left, x, y, first_frame, frames, placements);
                try self.flattenPlan(
                    plan.right,
                    x + left.width + plan.gap,
                    y,
                    first_frame,
                    frames,
                    placements,
                );
            },
            .column => {
                const left = self.plans.items[plan.left];
                try self.flattenPlan(plan.left, x, y, first_frame, frames, placements);
                try self.flattenPlan(
                    plan.right,
                    x,
                    y + left.height + plan.gap,
                    first_frame,
                    frames,
                    placements,
                );
            },
            .frame => {
                const frame_index: u32 = @intCast(frames.items.len);
                const drawable = std.math.add(u32, first_frame, frame_index) catch
                    return error.TooManyDrawables;
                try frames.append(self.allocator, .{
                    .width = plan.width,
                    .height = plan.height,
                });
                try placements.append(self.allocator, .{
                    .x = x,
                    .y = y,
                    .drawable = drawable,
                });
                const child = self.plans.items[plan.left];
                try self.flattenPlan(
                    plan.left,
                    x + (plan.width - child.width) / 2,
                    y + (plan.height - child.height) / 2,
                    first_frame,
                    frames,
                    placements,
                );
            },
        }
    }
};

fn dominates(a: Candidate, b: Candidate) bool {
    return a.width <= b.width and a.height <= b.height and a.cost <= b.cost;
}

fn better(a: Candidate, b: Candidate) bool {
    if (a.height != b.height) return a.height < b.height;
    if (a.cost != b.cost) return a.cost < b.cost;
    return a.width < b.width;
}

fn hasFitting(candidates: []const Candidate, limit: f64) bool {
    for (candidates) |candidate|
        if (candidate.width <= limit) return true;
    return false;
}

const CborParser = struct {
    allocator: std.mem.Allocator,
    input: []const u8,
    pos: usize = 0,

    const Header = struct {
        major: u3,
        info: u5,
        argument: u64,
    };

    fn byte(self: *CborParser) !u8 {
        if (self.pos == self.input.len) return error.UnexpectedEnd;
        defer self.pos += 1;
        return self.input[self.pos];
    }

    fn readInt(self: *CborParser, comptime T: type) !T {
        const size = @sizeOf(T);
        if (self.input.len - self.pos < size) return error.UnexpectedEnd;
        const value = std.mem.readInt(T, self.input[self.pos..][0..size], .big);
        self.pos += size;
        return value;
    }

    fn header(self: *CborParser) !Header {
        const initial = try self.byte();
        const info: u5 = @truncate(initial);
        const argument: u64 = switch (info) {
            0...23 => info,
            24 => try self.byte(),
            25 => try self.readInt(u16),
            26 => try self.readInt(u32),
            27 => try self.readInt(u64),
            else => return error.UnsupportedCbor,
        };
        return .{ .major = @truncate(initial >> 5), .info = info, .argument = argument };
    }

    fn length(argument: u64) !usize {
        return std.math.cast(usize, argument) orelse error.InputTooLarge;
    }

    fn mapLength(self: *CborParser) !usize {
        const item = try self.header();
        if (item.major != 5) return error.ExpectedMap;
        return length(item.argument);
    }

    fn arrayLength(self: *CborParser) !usize {
        const item = try self.header();
        if (item.major != 4) return error.ExpectedArray;
        return length(item.argument);
    }

    fn text(self: *CborParser) ![]const u8 {
        const item = try self.header();
        if (item.major != 3) return error.ExpectedText;
        const len = try length(item.argument);
        if (self.input.len - self.pos < len) return error.UnexpectedEnd;
        defer self.pos += len;
        return self.input[self.pos..][0..len];
    }

    fn unsigned(self: *CborParser, comptime T: type) !T {
        const item = try self.header();
        if (item.major != 0) return error.ExpectedUnsigned;
        return std.math.cast(T, item.argument) orelse error.IntegerOverflow;
    }

    fn number(self: *CborParser) !f64 {
        const item = try self.header();
        return switch (item.major) {
            0 => @floatFromInt(item.argument),
            1 => -1 - @as(f64, @floatFromInt(item.argument)),
            7 => switch (item.info) {
                25 => @as(f64, @floatCast(@as(f16, @bitCast(@as(u16, @intCast(item.argument)))))),
                26 => @as(f64, @floatCast(@as(f32, @bitCast(@as(u32, @intCast(item.argument)))))),
                27 => @as(f64, @bitCast(item.argument)),
                else => error.ExpectedNumber,
            },
            else => error.ExpectedNumber,
        };
    }

    fn parseSize(self: *CborParser) !Size {
        var size: Size = .{ .width = 0, .height = 0 };
        for (0..try self.mapLength()) |_| {
            const key = try self.text();
            if (std.mem.eql(u8, key, "width"))
                size.width = try self.number()
            else if (std.mem.eql(u8, key, "height"))
                size.height = try self.number()
            else if (std.mem.eql(u8, key, "id"))
                size.id = try self.unsigned(u32)
            else
                try self.skip();
        }
        return size;
    }

    fn parseSizes(self: *CborParser) ![]const Size {
        const sizes = try self.allocator.alloc(Size, try self.arrayLength());
        for (sizes) |*size| size.* = try self.parseSize();
        return sizes;
    }

    fn parseNodes(self: *CborParser) ![]const Node {
        const nodes = try self.allocator.alloc(Node, try self.arrayLength());
        for (nodes) |*node| node.* = try self.parseNode();
        return nodes;
    }

    fn parseKind(text_value: []const u8) !Kind {
        inline for (std.meta.fields(Kind)) |field|
            if (std.mem.eql(u8, text_value, field.name))
                return @enumFromInt(field.value);
        return error.UnknownKind;
    }

    fn parseNode(self: *CborParser) anyerror!Node {
        var kind: ?Kind = null;
        var node: Node = .{ .kind = .leaf };
        for (0..try self.mapLength()) |_| {
            const key = try self.text();
            if (std.mem.eql(u8, key, "kind"))
                kind = try parseKind(try self.text())
            else if (std.mem.eql(u8, key, "variants"))
                node.variants = try self.parseSizes()
            else if (std.mem.eql(u8, key, "children"))
                node.children = try self.parseNodes()
            else if (std.mem.eql(u8, key, "gap"))
                node.gap = try self.number()
            else if (std.mem.eql(u8, key, "row_gap"))
                node.row_gap = try self.number()
            else if (std.mem.eql(u8, key, "column_gap"))
                node.column_gap = try self.number()
            else if (std.mem.eql(u8, key, "inset_x"))
                node.inset_x = try self.number()
            else if (std.mem.eql(u8, key, "inset_y"))
                node.inset_y = try self.number()
            else
                try self.skip();
        }
        node.kind = kind orelse return error.MissingKind;
        return node;
    }

    fn parseRequest(self: *CborParser) !Request {
        var limit: ?f64 = null;
        var max_frontier: u8 = 8;
        var first_frame: u32 = 0;
        var root: ?Node = null;
        for (0..try self.mapLength()) |_| {
            const key = try self.text();
            if (std.mem.eql(u8, key, "limit"))
                limit = try self.number()
            else if (std.mem.eql(u8, key, "max_frontier"))
                max_frontier = try self.unsigned(u8)
            else if (std.mem.eql(u8, key, "first_frame"))
                first_frame = try self.unsigned(u32)
            else if (std.mem.eql(u8, key, "root"))
                root = try self.parseNode()
            else
                try self.skip();
        }
        if (self.pos != self.input.len) return error.TrailingData;
        return .{
            .limit = limit orelse return error.MissingLimit,
            .max_frontier = max_frontier,
            .first_frame = first_frame,
            .root = root orelse return error.MissingRoot,
        };
    }

    fn skip(self: *CborParser) anyerror!void {
        const item = try self.header();
        switch (item.major) {
            0, 1, 7 => {},
            2, 3 => {
                const len = try length(item.argument);
                if (self.input.len - self.pos < len) return error.UnexpectedEnd;
                self.pos += len;
            },
            4 => for (0..try length(item.argument)) |_| try self.skip(),
            5 => for (0..try length(item.argument)) |_| {
                try self.skip();
                try self.skip();
            },
            else => return error.UnsupportedCbor,
        }
    }
};

fn writeCborHeader(writer: *std.Io.Writer, major: u3, value: usize) !void {
    const prefix: u8 = @as(u8, major) << 5;
    if (value < 24) {
        try writer.writeByte(prefix | @as(u8, @intCast(value)));
    } else if (value <= std.math.maxInt(u8)) {
        try writer.writeByte(prefix | 24);
        try writer.writeInt(u8, @intCast(value), .big);
    } else if (value <= std.math.maxInt(u16)) {
        try writer.writeByte(prefix | 25);
        try writer.writeInt(u16, @intCast(value), .big);
    } else if (value <= std.math.maxInt(u32)) {
        try writer.writeByte(prefix | 26);
        try writer.writeInt(u32, @intCast(value), .big);
    } else {
        try writer.writeByte(prefix | 27);
        try writer.writeInt(u64, @intCast(value), .big);
    }
}

fn writeCborArray(writer: *std.Io.Writer, len: usize) !void {
    try writeCborHeader(writer, 4, len);
}

fn writeCborUnsigned(writer: *std.Io.Writer, value: u32) !void {
    try writeCborHeader(writer, 0, value);
}

fn writeCborFloat(writer: *std.Io.Writer, value: f64) !void {
    try writer.writeByte(0xfb);
    try writer.writeInt(u64, @bitCast(value), .big);
}

fn encodeDisplayList(
    allocator: std.mem.Allocator,
    width: f64,
    height: f64,
    frames: []const Frame,
    placements: []const Placement,
) ![]u8 {
    var output: std.Io.Writer.Allocating = .init(allocator);
    errdefer output.deinit();
    try writeCborArray(&output.writer, 4);
    try writeCborFloat(&output.writer, width);
    try writeCborFloat(&output.writer, height);
    try writeCborArray(&output.writer, frames.len);
    for (frames) |frame| {
        try writeCborArray(&output.writer, 2);
        try writeCborFloat(&output.writer, frame.width);
        try writeCborFloat(&output.writer, frame.height);
    }
    try writeCborArray(&output.writer, placements.len);
    for (placements) |placement| {
        try writeCborArray(&output.writer, 3);
        try writeCborFloat(&output.writer, placement.x);
        try writeCborFloat(&output.writer, placement.y);
        try writeCborUnsigned(&output.writer, placement.drawable);
    }
    return output.toOwnedSlice();
}

fn layoutRequest(
    output_allocator: std.mem.Allocator,
    arena: std.mem.Allocator,
    request: Request,
) ![]u8 {
    if (!std.math.isFinite(request.limit) or request.limit < 0 or
        request.max_frontier == 0 or request.max_frontier > 64)
        return error.InvalidOptions;

    var planner: Planner = .{
        .allocator = arena,
        .limit = request.limit,
        .max_frontier = request.max_frontier,
    };
    const winner = try planner.best(try planner.eval(request.root));

    var frames: std.ArrayList(Frame) = .empty;
    var placements: std.ArrayList(Placement) = .empty;
    try planner.flattenPlan(
        winner.plan,
        0,
        0,
        request.first_frame,
        &frames,
        &placements,
    );
    return encodeDisplayList(
        output_allocator,
        winner.width,
        winner.height,
        frames.items,
        placements.items,
    );
}

pub fn layout(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var arena_state = std.heap.ArenaAllocator.init(allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const parsed = try std.json.parseFromSlice(Request, arena, input, .{});
    defer parsed.deinit();
    return layoutRequest(allocator, arena, parsed.value);
}

pub fn layoutCbor(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var arena_state = std.heap.ArenaAllocator.init(allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var parser: CborParser = .{ .allocator = arena, .input = input };
    return layoutRequest(allocator, arena, try parser.parseRequest());
}

test "row-column chooses the shortest fitting rectangle" {
    const wide =
        \\{"limit":25,"root":{"kind":"row_column","row_gap":2,"column_gap":3,"children":[
        \\  {"kind":"leaf","variants":[{"width":10,"height":5,"id":7}]},
        \\  {"kind":"leaf","variants":[{"width":10,"height":5,"id":8}]}
        \\]}}
    ;
    const horizontal = try layout(std.testing.allocator, wide);
    defer std.testing.allocator.free(horizontal);
    const expected_horizontal = [_]u8{
        0x84, 0xfb, 0x40, 0x36, 0, 0, 0, 0, 0, 0, 0xfb, 0x40, 0x14, 0, 0, 0, 0, 0, 0,
        0x80, 0x82,
        0x83, 0xfb, 0, 0, 0, 0, 0, 0, 0, 0, 0xfb, 0, 0, 0, 0, 0, 0, 0, 0, 7,
        0x83, 0xfb, 0x40, 0x28, 0, 0, 0, 0, 0, 0, 0xfb, 0, 0, 0, 0, 0, 0, 0, 0, 8,
    };
    try std.testing.expectEqualSlices(u8, &expected_horizontal, horizontal);

    const narrow =
        \\{"limit":20,"root":{"kind":"row_column","row_gap":2,"column_gap":3,"children":[
        \\  {"kind":"leaf","variants":[{"width":10,"height":5,"id":7}]},
        \\  {"kind":"leaf","variants":[{"width":10,"height":5,"id":8}]}
        \\]}}
    ;
    const vertical = try layout(std.testing.allocator, narrow);
    defer std.testing.allocator.free(vertical);
    const expected_vertical = [_]u8{
        0x84, 0xfb, 0x40, 0x24, 0, 0, 0, 0, 0, 0, 0xfb, 0x40, 0x2a, 0, 0, 0, 0, 0, 0,
        0x80, 0x82,
        0x83, 0xfb, 0, 0, 0, 0, 0, 0, 0, 0, 0xfb, 0, 0, 0, 0, 0, 0, 0, 0, 7,
        0x83, 0xfb, 0, 0, 0, 0, 0, 0, 0, 0, 0xfb, 0x40, 0x20, 0, 0, 0, 0, 0, 0, 8,
    };
    try std.testing.expectEqualSlices(u8, &expected_vertical, vertical);
}

test "leaf alternatives and frames use analytic dimensions" {
    const input =
        \\{"limit":20,"first_frame":12,"root":{"kind":"frame","inset_x":2,"inset_y":1,"children":[
        \\  {"kind":"leaf","variants":[
        \\    {"width":18,"height":4,"id":3},
        \\    {"width":12,"height":7,"id":4}
        \\  ]}
        \\]}}
    ;
    const output = try layout(std.testing.allocator, input);
    defer std.testing.allocator.free(output);
    const expected = [_]u8{
        0x84, 0xfb, 0x40, 0x30, 0, 0, 0, 0, 0, 0, 0xfb, 0x40, 0x22, 0, 0, 0, 0, 0, 0,
        0x81, 0x82,
        0xfb, 0x40, 0x30, 0, 0, 0, 0, 0, 0, 0xfb, 0x40, 0x22, 0, 0, 0, 0, 0, 0,
        0x82,
        0x83, 0xfb, 0, 0, 0, 0, 0, 0, 0, 0, 0xfb, 0, 0, 0, 0, 0, 0, 0, 0, 12,
        0x83, 0xfb, 0x40, 0, 0, 0, 0, 0, 0, 0, 0xfb, 0x3f, 0xf0, 0, 0, 0, 0, 0, 0, 4,
    };
    try std.testing.expectEqualSlices(u8, &expected, output);
}
