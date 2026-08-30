const std = @import("std");

// Typst supplies a semantic tree whose leaves are measured proportional text
// boxes.  This module owns everything combinatorial: analytic row, column, and
// frame bounds; bounded Pareto frontiers; and selection under a width limit.
// Plans use 32-bit arena indices while searching.  Only the winning plan is
// expanded to JSON for Typst to turn back into concrete content.

const Size = struct {
    width: f64,
    height: f64,
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
    leaf: u32 = 0,
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
    leaf: u32 = 0,
    variant: u32 = 0,
    gap: f64 = 0,
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
            .plan = try self.addPlan(.{ .tag = .empty }),
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
        for (node.variants, 0..) |variant, index| {
            if (!std.math.isFinite(variant.width) or !std.math.isFinite(variant.height) or
                variant.width < 0 or variant.height < 0)
                return error.InvalidRectangle;
            try candidates.append(self.allocator, .{
                .width = variant.width,
                .height = variant.height,
                .plan = try self.addPlan(.{
                    .tag = .leaf,
                    .leaf = node.leaf,
                    .variant = @intCast(index),
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
                    try next.append(self.allocator, .{
                        .width = if (tag == .row)
                            left.width + gap + right.width
                        else
                            @max(left.width, right.width),
                        .height = if (tag == .row)
                            @max(left.height, right.height)
                        else
                            left.height + gap + right.height,
                        .cost = left.cost + right.cost,
                        .plan = try self.addPlan(.{
                            .tag = tag,
                            .left = left.plan,
                            .right = right.plan,
                            .gap = gap,
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
            try framed.append(self.allocator, .{
                .width = child.width + 2 * node.inset_x,
                .height = child.height + 2 * node.inset_y,
                .cost = child.cost,
                .plan = try self.addPlan(.{ .tag = .frame, .left = child.plan }),
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

    fn writePlan(self: *Planner, writer: *std.Io.Writer, index: u32) anyerror!void {
        const plan = self.plans.items[index];
        switch (plan.tag) {
            .empty => try writer.writeAll("{\"kind\":\"empty\"}"),
            .leaf => try writer.print(
                "{{\"kind\":\"leaf\",\"leaf\":{d},\"variant\":{d}}}",
                .{ plan.leaf, plan.variant },
            ),
            .row, .column => {
                try writer.print(
                    "{{\"kind\":\"{s}\",\"gap\":{d},\"children\":[",
                    .{ @tagName(plan.tag), plan.gap },
                );
                var first = true;
                try self.writeCompositionChildren(writer, plan.left, plan.tag, plan.gap, &first);
                try self.writeCompositionChildren(writer, plan.right, plan.tag, plan.gap, &first);
                try writer.writeAll("]}");
            },
            .frame => {
                try writer.writeAll("{\"kind\":\"frame\",\"child\":");
                try self.writePlan(writer, plan.left);
                try writer.writeByte('}');
            },
        }
    }

    fn writeCompositionChildren(
        self: *Planner,
        writer: *std.Io.Writer,
        index: u32,
        tag: PlanTag,
        gap: f64,
        first: *bool,
    ) anyerror!void {
        const child = self.plans.items[index];
        if (child.tag == tag and child.gap == gap) {
            try self.writeCompositionChildren(writer, child.left, tag, gap, first);
            try self.writeCompositionChildren(writer, child.right, tag, gap, first);
            return;
        }
        if (!first.*) try writer.writeByte(',');
        first.* = false;
        try self.writePlan(writer, index);
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

pub fn layout(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var arena_state = std.heap.ArenaAllocator.init(allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const parsed = try std.json.parseFromSlice(Request, arena, input, .{});
    defer parsed.deinit();
    const request = parsed.value;
    if (!std.math.isFinite(request.limit) or request.limit < 0 or
        request.max_frontier == 0 or request.max_frontier > 64)
        return error.InvalidOptions;

    var planner: Planner = .{
        .allocator = arena,
        .limit = request.limit,
        .max_frontier = request.max_frontier,
    };
    const winner = try planner.best(try planner.eval(request.root));

    var output: std.Io.Writer.Allocating = .init(allocator);
    errdefer output.deinit();
    try output.writer.print(
        "{{\"width\":{d},\"height\":{d},\"layout\":",
        .{ winner.width, winner.height },
    );
    try planner.writePlan(&output.writer, winner.plan);
    try output.writer.writeByte('}');
    return output.toOwnedSlice();
}

test "row-column chooses the shortest fitting rectangle" {
    const wide =
        \\{"limit":25,"root":{"kind":"row_column","row_gap":2,"column_gap":3,"children":[
        \\  {"kind":"leaf","leaf":0,"variants":[{"width":10,"height":5}]},
        \\  {"kind":"leaf","leaf":1,"variants":[{"width":10,"height":5}]}
        \\]}}
    ;
    const horizontal = try layout(std.testing.allocator, wide);
    defer std.testing.allocator.free(horizontal);
    try std.testing.expect(std.mem.indexOf(u8, horizontal, "\"layout\":{\"kind\":\"row\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, horizontal, "\"width\":22") != null);
    try std.testing.expect(std.mem.indexOf(u8, horizontal, "\"leaf\":1") != null);

    const narrow =
        \\{"limit":20,"root":{"kind":"row_column","row_gap":2,"column_gap":3,"children":[
        \\  {"kind":"leaf","leaf":0,"variants":[{"width":10,"height":5}]},
        \\  {"kind":"leaf","leaf":1,"variants":[{"width":10,"height":5}]}
        \\]}}
    ;
    const vertical = try layout(std.testing.allocator, narrow);
    defer std.testing.allocator.free(vertical);
    try std.testing.expect(std.mem.indexOf(u8, vertical, "\"layout\":{\"kind\":\"column\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, vertical, "\"height\":13") != null);
}

test "leaf alternatives and frames use analytic dimensions" {
    const input =
        \\{"limit":20,"root":{"kind":"frame","inset_x":2,"inset_y":1,"children":[
        \\  {"kind":"leaf","leaf":0,"variants":[
        \\    {"width":18,"height":4},
        \\    {"width":12,"height":7}
        \\  ]}
        \\]}}
    ;
    const output = try layout(std.testing.allocator, input);
    defer std.testing.allocator.free(output);
    try std.testing.expect(std.mem.indexOf(u8, output, "\"width\":16") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "\"height\":9") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "\"variant\":1") != null);
}
