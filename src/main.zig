const std = @import("std");
const token = @import("token.zig");
const Lexer = @import("lexer.zig").Lexer;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    const input = try allocator.alloc(u8, 2048);
    defer allocator.free(input);

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var line = std.ArrayList(u8).init(allocator);
    const writer = line.writer();

    try stdout.print(">> ", .{});

    while (stdin.streamUntilDelimiter(writer, '\n', null)) {
        var lexer = Lexer.init(line.items);

        var tok = lexer.nextToken();

        while (tok.type != .eof) : (tok = lexer.nextToken()) {
            std.debug.print("{}:{} {s}: {}\n", .{ tok.start, tok.end, tok.literal, tok.type });
        }

        try stdout.print("{s}\n", .{line.items});
        defer line.clearRetainingCapacity();
        try stdout.print(">> ", .{});
    } else |err| switch (err) {
        else => {
            std.debug.print("FUCK", .{});
            return;
        },
    }
}
