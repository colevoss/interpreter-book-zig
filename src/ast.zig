const std = @import("std");
const token = @import("token.zig");
const Token = token.Token;
const Allocator = std.mem.Allocator;

pub const Expression = union(enum) {
    identifier: Identifier,

    pub const Identifier = struct {
        token: Token,
        value: []const u8,
    };
};

pub const Statement = union(enum) {
    let: Let,

    ret: Return,

    /// Let statement:
    /// let myVar = 5;
    pub const Let = struct {
        // the "let" token
        token: Token,

        // the name of the variable
        name: Expression.Identifier,

        // the value of the variable which itself could be an expression
        value: Expression,
    };

    pub const Return = struct {
        token: Token,
        value: Expression,
    };
};

pub const Program = struct {
    statements: std.ArrayList(Statement),
    // allocator: Allocator,

    pub fn init(allocator: Allocator) Program {
        const statements = std.ArrayList(Statement).init(allocator);

        return Program{
            .statements = statements,
            // .allocator = allocator,
        };
    }

    pub fn deinit(self: *Program) void {
        self.statements.deinit();
    }

    pub fn addStatement(self: *Program, statement: Statement) !usize {
        try self.statements.append(statement);
        return self.statements.items.len;
    }
};
