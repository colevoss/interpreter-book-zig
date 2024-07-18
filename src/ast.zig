const std = @import("std");
const token = @import("token.zig");
const Token = token.Token;
const Allocator = std.mem.Allocator;

pub const Exp = struct {
    token: Token,
    type: ExpressionType,

    pub const ExpressionType = union(enum) {
        identifier: []const u8,

        integer: i64,

        prefix: Prefix,
    };

    pub const Prefix = struct {
        operator: []const u8,
        right: *Expression,
    };
};

pub const Expression = union(enum) {
    identifier: Identifier,

    integerLiteral: IntegerLiteral,

    // prefix: Prefix,

    pub const Identifier = struct {
        token: Token,
        value: []const u8,
    };

    pub const IntegerLiteral = struct {
        token: Token,
        value: i64,
    };
};

// pub const Prefix = struct {
//     token: Token,
//     operator: []const u8,
//     right: Expression,
// };

pub const Statement = union(enum) {
    let: Let,

    ret: Return,

    expression: ExpressionStatement,

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

    // Return statement
    pub const Return = struct {
        token: Token,
        value: Expression,
    };

    // This is when a line of code is just an expression without a let or return
    // example:
    // let x = 5; <-- let statement
    // x + 10;    <-- expression statement
    pub const ExpressionStatement = struct {
        token: Token, // first token of the expression
        expression: ?Expression,
    };
};

pub const Program = struct {
    statements: std.ArrayList(Statement),

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

    pub fn addStatement(self: *Program, statement: Statement) !void {
        try self.statements.append(statement);
    }
};
