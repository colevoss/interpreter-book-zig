const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Evaluator = @import("evaluator.zig").Evaluator;
const environment = @import("environment.zig");

pub fn run(allocator: std.mem.Allocator) !void {
    const input = allocator.alloc(u8, 4096) catch unreachable;
    defer allocator.free(input);

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var line = std.ArrayList(u8).init(allocator);
    const writer = line.writer();

    var env = environment.Environment.init(allocator);
    defer env.deinit();

    try stdout.writeAll(">> ");
    while (stdin.streamUntilDelimiter(writer, '\n', null)) {
        defer line.clearRetainingCapacity();

        var lexer = Lexer.init(line.items);

        var parser = Parser.init(allocator, &lexer);
        defer parser.deinit();

        var program = try parser.parseProgram(allocator);
        defer program.deinit();

        var eval = Evaluator.init(allocator, &env);
        defer eval.deinit();

        if (parser.errorCount() > 0) {
            for (parser.errors.items) |e| {
                try stdout.print("Error: {s}\n", .{e});
            }

            try stdout.writeAll(">> ");
            continue;
        }

        const result = eval.evaluate(&program);

        try result.value.inspect(stdout);
        try stdout.writeAll("\n");

        try stdout.writeAll(">> ");
    } else |err| {
        std.debug.print("FUCK {}", .{err});
        return;
    }
}
