const std = @import("std");
const ast = @import("ast.zig");
const environment = @import("environment.zig");

const log = std.log.scoped(.object);

const Self = @This();

value: Value,

pub const trueVal: Self = .{ .value = .{ .boolean = true } };
pub const falseVal: Self = .{ .value = .{ .boolean = false } };
pub const nullVal: Self = .{ .value = .{ .null = 0 } };

pub fn initError(allocator: std.mem.Allocator, comptime fmt: []const u8, args: anytype) Self {
    const errorMsg = std.fmt.allocPrint(allocator, fmt, args) catch unreachable;

    return .{
        .value = .{
            .@"error" = errorMsg,
        },
    };
}

pub fn isError(self: *const Self) bool {
    return self.value == .@"error";
}

pub const Type = enum {
    integer,
    boolean,
    null,

    function,

    @"return",
    @"error",
};

pub const Value = union(Type) {
    integer: isize,
    boolean: bool,
    null: u1,
    function: Function,

    @"return": *Self,
    @"error": []const u8,

    pub fn string(self: Value, allocator: std.mem.Allocator) anyerror![]const u8 {
        var arr = std.ArrayList(u8).init(allocator);
        const writer = arr.writer();

        try self.inspect(writer);
        return arr.items;
    }

    pub fn inspect(self: Value, writer: anytype) anyerror!void {
        std.debug.print("", .{});
        switch (self) {
            .integer => |int| {
                try writer.print("{d}", .{int});
            },

            .boolean => |b| {
                try writer.print("{}", .{b});
            },

            .null => {
                try writer.writeAll("null");
            },

            .function => |func| {
                try writer.writeAll("fn(");

                for (func.parameters.items, 0..) |param, i| {
                    try writer.writeAll(param);

                    if (i < func.parameters.items.len - 1) {
                        try writer.writeAll(", ");
                    }
                }

                try writer.writeAll(") {\n");

                try func.body.print(writer);
                try writer.writeAll("\n}");
            },

            .@"return" => |val| {
                try val.value.inspect(writer);
            },

            .@"error" => |err| {
                try writer.print("ERROR: {s}", .{err});
            },
        }
    }

    const Function = struct {
        parameters: std.ArrayList([]const u8),
        body: *ast.Statement.Block,
        env: *environment.Environment,
    };

    // const FunctionCall = struct {
    //     arguments: std.ArrayList([] const u8),
    // };
};
