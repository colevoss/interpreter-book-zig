const std = @import("std");
const token = @import("token.zig");
const Token = token.Token;
const Allocator = std.mem.Allocator;

pub const Expression = struct {
    token: Token,
    type: ExpressionType,

    pub fn init(tok: Token, expType: ExpressionType) Expression {
        return .{
            .token = tok,
            .type = expType,
        };
    }

    pub fn initAlloc(allocator: Allocator, tok: Token, expType: ExpressionType) std.mem.Allocator.Error!*Expression {
        const expression = try allocator.create(Expression);

        expression.* = .{
            .token = tok,
            .type = expType,
        };

        return expression;
    }

    pub fn invalidAlloc(allocator: Allocator, tok: Token) std.mem.Allocator.Error!*Expression {
        return Expression.initAlloc(allocator, tok, .{
            .invalid = tok.literal,
        });
    }

    pub const ExpressionType = union(enum) {
        identifier: []const u8,
        integer: i64,
        prefix: Prefix,
    };

    pub const Prefix = struct {
        operator: []const u8,
        // TODO: Should this be an optional?
        right: ?*Expression,
    };
};

pub const Statement = struct {
    token: Token,

    type: StatementType,

    pub fn init(tok: Token, stmtType: StatementType) Statement {
        return .{
            .token = tok,
            .type = stmtType,
        };
    }

    pub fn initAlloc(allocator: Allocator, tok: Token, stmtType: StatementType) !*Statement {
        const stmt = try allocator.create(Statement);
        stmt.* = .{
            .token = tok,
            .type = stmtType,
        };

        return stmt;
    }

    pub const StatementType = union(enum) {
        let: Let,
        @"return": ?*Expression,
        expression: ExpressionStatement,
    };

    pub const Let = struct {
        name: []const u8,
        value: *Expression,
    };

    pub const ExpressionStatement = struct {
        expression: *Expression,
    };
};

pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn init(allocator: Allocator) Program {
        const statements = std.ArrayList(Statement).init(allocator);

        return Program{
            .statements = statements,
        };
    }

    pub fn deinit(self: *Program) void {
        self.statements.deinit();
    }

    pub fn addStatement(self: *Program, statement: Statement) !void {
        try self.statements.append(statement);
    }
};
