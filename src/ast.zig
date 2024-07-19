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

    pub fn print(self: *Expression, writer: anytype) !void {
        try self.type.print(writer);
    }

    pub fn initAlloc(allocator: Allocator, tok: Token, expType: ExpressionType) std.mem.Allocator.Error!*Expression {
        const expression = try allocator.create(Expression);

        expression.* = .{
            .token = tok,
            .type = expType,
        };

        return expression;
    }

    pub const ExpressionType = union(enum) {
        identifier: []const u8,
        integer: i64,
        prefix: Prefix,
        infix: Infix,

        pub fn print(self: ExpressionType, writer: anytype) anyerror!void {
            switch (self) {
                .identifier => |i| {
                    try writer.writeAll(i);
                },
                .integer => |int| {
                    try writer.print("{d}", .{int});
                },
                .prefix => |prefix| {
                    try writer.writeAll("(");
                    try writer.writeAll(prefix.operator);

                    if (prefix.right) |right| {
                        try right.print(writer);
                    }
                    try writer.writeAll(")");
                },
                .infix => |infix| {
                    try writer.writeAll("(");
                    try infix.left.print(writer);
                    try writer.print(" {s} ", .{infix.operator});
                    try infix.right.print(writer);
                    try writer.writeAll(")");
                },
            }
        }
    };

    pub const Prefix = struct {
        operator: []const u8,
        // TODO: Should this be an optional?
        right: ?*Expression,
    };

    pub const Infix = struct {
        operator: []const u8,
        right: *Expression,
        left: *Expression,
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

    pub fn print(self: *const Statement, writer: anytype) !void {
        try self.type.print(writer);
    }

    pub const StatementType = union(enum) {
        let: Let,
        @"return": ?*Expression,
        expression: ExpressionStatement,

        pub fn print(self: StatementType, writer: anytype) !void {
            switch (self) {
                .let => |let| {
                    try writer.writeAll(let.name);
                    try writer.writeAll(" = ");
                    // try let.value.print(writer);

                    try writer.writeAll("//TODO: fix let value expressions");
                    try writer.writeAll(";");
                },
                .@"return" => {
                    try writer.writeAll("return");
                    try writer.writeAll(";");
                },
                .expression => |expression| {
                    try expression.expression.print(writer);
                },
            }
        }
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

    pub fn print(self: *Program, writer: anytype) !void {
        for (self.statements.items) |statement| {
            try statement.print(writer);
        }
    }
};
