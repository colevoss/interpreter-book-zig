const std = @import("std");
const t = @import("token.zig");

pub const Program = struct {
    statements: []Statement,

    pub fn tokenLiteral(self: *Program) []const u8 {
        if (self.statements.len == 0) {
            return "";
        }

        return self.statements[0].tokenLiteral();
    }
};

pub const Statement = union(enum) {
    let: *const LetStatement,

    pub fn tokenLiteral(self: Statement) []const u8 {
        return switch (self) {
            Statement.let => |*let| let.*.tokenValue(),
        };
    }
};

pub const Expression = union(enum) {
    pub fn tokenLiteral(_: Expression) []const u8 {
        return "";
    }
};

pub const LetStatement = struct {
    fn tokenValue(_: *LetStatement) []const u8 {
        return "hi";
    }
};

test "t" {
    const statement = Statement{ .let = &LetStatement{} };

    try std.testing.expect(std.mem.eql(u8, statement.tokenLiteral(), "hi"));
}
