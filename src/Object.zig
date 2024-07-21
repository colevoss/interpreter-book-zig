const std = @import("std");

const log = std.log.scoped(.object);

const Self = @This();

value: Value,

pub const trueVal: Self = .{ .value = .{ .boolean = true } };
pub const falseVal: Self = .{ .value = .{ .boolean = false } };
pub const nullVal: Self = .{ .value = .{ .null = 0 } };

pub const Value = union(enum) {
    integer: isize,
    boolean: bool,
    null: u1,

    @"return": *Self,
    @"error": []const u8,

    pub fn inspect(self: Value, writer: anytype) anyerror!void {
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

            .@"return" => |val| {
                try val.value.inspect(writer);
            },

            .@"error" => |err| {
                try writer.print("ERROR: {s}", .{err});
            },
        }
    }
};
