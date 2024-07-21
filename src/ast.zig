const std = @import("std");
const Object = @import("Object.zig");
const token = @import("token.zig");
const eval = @import("evaluator.zig");
const Token = token.Token;
const Allocator = std.mem.Allocator;

fn padWriterLine(writer: anytype, padding: usize) !void {
    var i: usize = 0;
    while (i < padding) : (i += 1) {
        _ = try writer.write("  ");
        // _ = try writer.write("..");
    }
}

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

    pub fn debug(self: *Expression, writer: anytype, depth: usize) !void {
        try padWriterLine(writer, depth);
        // try writer.print("[{s}] ({s})\n", .{ @tagName(self.type), self.token.literal });
        try writer.print("[{s}]\n", .{@tagName(self.type)});
        try self.type.debug(writer, depth + 1);
    }

    pub fn initAlloc(allocator: Allocator, tok: Token, expType: ExpressionType) std.mem.Allocator.Error!*Expression {
        const expression = try allocator.create(Expression);

        expression.* = .{
            .token = tok,
            .type = expType,
        };

        return expression;
    }

    pub fn evaluate(self: *Expression, evaluator: *eval.Evaluator) Object {
        return evaluator.evaluateExpression(self);
    }

    pub const Prefix = struct {
        operator: []const u8,
        // TODO: Should this be an optional?
        right: *Expression,
    };

    pub const Infix = struct {
        operator: []const u8,
        right: *Expression,
        left: *Expression,
    };

    pub const If = struct {
        condition: *Expression,
        consequence: *Statement.Block,
        alternative: ?*Statement.Block,
    };

    pub const FunctionLiteral = struct {
        parameters: std.ArrayList([]const u8),
        body: *Statement.Block,
    };

    pub const FunctionCall = struct {
        function: *Expression, // this could be an Identifier or a FunctionLiteral
        arguments: std.ArrayList(*Expression),
    };

    pub const ExpressionType = union(enum) {
        identifier: []const u8,
        integer: isize,
        boolean: bool,

        prefix: Prefix,
        infix: Infix,

        @"if": If,

        function: FunctionLiteral,
        call: FunctionCall,

        pub fn debug(self: ExpressionType, writer: anytype, depth: usize) anyerror!void {
            try padWriterLine(writer, depth);

            switch (self) {
                .identifier => |i| {
                    try writer.print("{s}: {s}", .{ @tagName(self), i });
                },
                .integer => |int| {
                    try writer.print("{d}", .{int});
                },
                .infix => |infix| {
                    try writer.print("operator: {s}\n", .{infix.operator});

                    try padWriterLine(writer, depth);
                    try writer.writeAll("left:\n");
                    try infix.left.debug(writer, depth + 1);

                    try padWriterLine(writer, depth);
                    try writer.writeAll("right:\n");
                    try infix.right.debug(writer, depth + 1);
                },
                .prefix => |prefix| {
                    try writer.print("operator: {s}\n", .{prefix.operator});

                    if (prefix.right) |right| {
                        try padWriterLine(writer, depth);
                        try writer.writeAll("right:\n");
                        try right.debug(writer, depth + 1);
                    }
                },
                .boolean => |b| {
                    try writer.print("bool: {}", .{b});
                },
                .@"if" => |i| {
                    try writer.writeAll("condition:\n");
                    try i.condition.debug(writer, depth + 1);

                    try padWriterLine(writer, depth);
                    try writer.writeAll("consequence:\n");
                    try i.consequence.debug(writer, depth);

                    if (i.alternative) |alt| {
                        try padWriterLine(writer, depth);
                        try writer.writeAll("alternative:\n");
                        try alt.debug(writer, depth);
                    }
                },
                else => {
                    try writer.print("EXPRESSION TYPE {s} DEBUG NOT IMPLEMENTED\n", .{@tagName(self)});
                },
            }

            _ = try writer.write("\n");
        }

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

                    try prefix.right.print(writer);
                    try writer.writeAll(")");
                },
                .infix => |infix| {
                    try writer.writeAll("(");
                    try infix.left.print(writer);
                    try writer.print(" {s} ", .{infix.operator});
                    try infix.right.print(writer);
                    try writer.writeAll(")");
                },
                .boolean => |b| {
                    try writer.print("{}", .{b});
                },
                .@"if" => |ifst| {
                    try writer.writeAll("if ");
                    try ifst.condition.print(writer);
                    try writer.writeAll(" ");
                    try ifst.consequence.print(writer);

                    if (ifst.alternative) |alt| {
                        try writer.writeAll(" else ");
                        try alt.print(writer);
                    }
                },
                .function => |function| {
                    try writer.writeAll("fn(");

                    for (function.parameters.items, 0..) |p, i| {
                        try writer.writeAll(p);

                        if (i < function.parameters.items.len - 1) {
                            try writer.writeAll(", ");
                        }
                    }

                    try writer.writeAll(")");

                    if (function.body.statements.items.len > 0) {
                        try writer.writeAll(" ");
                        try function.body.print(writer);
                    }
                },
                .call => |call| {
                    try call.function.print(writer);

                    try writer.writeAll("(");

                    for (call.arguments.items, 0..) |p, i| {
                        try p.print(writer);

                        if (i < call.arguments.items.len - 1) {
                            try writer.writeAll(", ");
                        }
                    }

                    try writer.writeAll(")");
                },
                // else => {
                //     try writer.print("EXPRESSION TYPE {s} PRINT NOT IMPLEMENTED", .{@tagName(self)});
                // },
            }
        }
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

    pub fn evaluate(self: *Statement, evaluator: *eval.Evaluator) Object {
        return evaluator.evaluateStatement(self);
    }

    pub fn print(self: *const Statement, writer: anytype) !void {
        try self.type.print(writer);
    }

    pub fn debug(self: *const Statement, writer: anytype, depth: usize) !void {
        try self.type.debug(writer, depth);
    }

    pub const Let = struct {
        name: []const u8,
        value: *Expression,
    };

    pub const ExpressionStatement = struct {
        expression: *Expression,
    };

    pub const Block = struct {
        token: token.Token,
        // These might need to be a pointer to statement
        statements: std.ArrayList(Statement),

        pub fn init(allocator: Allocator, tok: token.Token) !*Block {
            const block = try allocator.create(Block);

            block.* = .{
                .token = tok,
                .statements = std.ArrayList(Statement).init(allocator),
            };

            return block;
        }

        pub fn addStatement(self: *Block, statement: Statement) !void {
            try self.statements.append(statement);
        }

        pub fn debug(self: *Block, writer: anytype, depth: usize) !void {
            for (self.statements.items) |statement| {
                // try padWriterLine(writer, depth);
                try statement.debug(writer, depth);
            }
        }

        pub fn print(self: *Block, writer: anytype) !void {
            for (self.statements.items) |statement| {
                try statement.print(writer);
            }
        }
    };

    pub const StatementType = union(enum) {
        let: Let,
        @"return": ?*Expression,
        expression: ExpressionStatement,
        block: Block,

        pub fn print(self: StatementType, writer: anytype) anyerror!void {
            switch (self) {
                .let => |let| {
                    try writer.writeAll(let.name);
                    try writer.writeAll(" = ");
                    try let.value.print(writer);

                    try writer.writeAll(";");
                },
                .@"return" => {
                    try writer.writeAll("return");
                    try writer.writeAll(";");
                },
                .expression => |expression| {
                    try expression.expression.print(writer);
                },
                .block => |block| {
                    for (block.statements.items) |statement| {
                        try statement.print(writer);
                    }
                },
            }
        }

        pub fn debug(self: StatementType, writer: anytype, depth: usize) !void {
            try padWriterLine(writer, depth);
            // try writer.print("{s}\n", .{@tagName(self)});
            switch (self) {
                .expression => |expression| {
                    // try expression.expression.debug(writer, depth + 1);
                    try expression.expression.debug(writer, depth);
                },
                else => {
                    try writer.print("{s}:\n", .{@tagName(self)});
                },
            }
        }
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

    pub fn statementCount(self: *const Program) usize {
        return self.statements.items.len;
    }

    pub fn addStatement(self: *Program, statement: Statement) !void {
        try self.statements.append(statement);
    }

    pub fn print(self: *Program, writer: anytype) !void {
        for (self.statements.items) |statement| {
            try statement.print(writer);
        }
    }

    pub fn debug(self: *Program, writer: anytype) !void {
        for (self.statements.items) |statement| {
            try statement.debug(writer, 0);
        }
    }

    pub fn evaluate(self: *const Program, evaluator: *eval.Evaluator) Object {
        return evaluator.evaluateProgram(self);
    }
};
