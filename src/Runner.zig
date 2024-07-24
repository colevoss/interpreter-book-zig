const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Evaluator = @import("evaluator.zig").Evaluator;
const environment = @import("environment.zig");

pub fn run(allocator: std.mem.Allocator, path: []const u8) !void {
    const stdout = std.io.getStdOut();
    const outWriter = stdout.writer();

    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const stat = try file.stat();
    const code = try file.readToEndAlloc(allocator, stat.size);
    defer allocator.free(code);

    var lexer = Lexer.init(code);

    var parser = Parser.init(allocator, &lexer);
    defer parser.deinit();

    if (parser.errorCount() > 0) {
        for (parser.errors.items) |e| {
            try outWriter.print("{s}\n", .{e});
        }

        return;
    }

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    var env = environment.Environment.init(allocator);
    defer env.deinit();

    var eval = Evaluator.init(allocator, &env);
    defer eval.deinit();

    const result = eval.evaluate(&program);

    try result.value.inspect(outWriter);
    try outWriter.print("\n", .{});
}
