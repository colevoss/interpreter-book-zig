const std = @import("std");
const token = @import("token.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Evaluator = @import("evaluator.zig").Evaluator;
const Repl = @import("Repl.zig");
const Runner = @import("Runner.zig");

const log = std.log.scoped(.main);

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    defer _ = gpa.deinit();

    var args = std.process.args();
    _ = args.skip();

    const filePathArg = args.next();

    if (filePathArg) |filePath| {
        try Runner.run(allocator, filePath);
        return;
    }

    try Repl.run(allocator);
}
