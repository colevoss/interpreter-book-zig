const std = @import("std");
const expect = std.testing.expect;
const Allocator = std.mem.Allocator;

const lexer = @import("lexer.zig");

const token = @import("token.zig");
const TokenType = token.TokenType;

const ast = @import("ast.zig");
const Statement = ast.Statement;
const Expression = ast.Expression;

pub const Parser = struct {
    lexer: *lexer.Lexer,

    currentToken: token.Token,
    peekToken: token.Token,

    errors: std.ArrayList([]const u8),

    allocator: Allocator,

    arena: std.heap.ArenaAllocator,

    pub const Precedence = enum(u8) {
        lowest,

        // ==
        equals,

        // > or <
        lessgreater,

        // +
        sum,

        // *
        product,

        // -X or !X
        prefix,

        // myFunction(X)
        call,

        pub fn forToken(tok: TokenType) Precedence {
            return switch (tok) {
                .eq, .neq => .equals,
                .lt, .gt => .lessgreater,
                .plus, .minus => .sum,
                .asterisk, .slash => .product,
                else => .lowest,
            };
        }

        pub fn int(self: Precedence) u8 {
            return @intFromEnum(self);
        }
    };

    pub fn init(allocator: Allocator, lex: *lexer.Lexer) Parser {
        var parser: Parser = undefined;
        parser.lexer = lex;

        parser.allocator = allocator;

        parser.arena = std.heap.ArenaAllocator.init(parser.allocator);
        parser.errors = std.ArrayList([]const u8).init(parser.allocator);

        parser.nextToken();
        parser.nextToken();

        return parser;
    }

    pub fn deinit(self: *Parser) void {
        // Free each error string
        // for (self.errors.items) |e| {
        //     self.allocator.free(e);
        // }

        // deinit errors arraylist
        self.errors.deinit();
        // deinit all arena allocations
        self.arena.deinit();
    }

    pub fn errorCount(self: *Parser) usize {
        return self.errors.items.len;
    }

    pub fn nextToken(self: *Parser) void {
        self.currentToken = self.peekToken;
        self.peekToken = self.lexer.nextToken();
    }

    pub fn parseProgram(self: *Parser, allocator: Allocator) !ast.Program {
        var program = ast.Program.init(allocator);

        while (self.currentToken.type != token.TokenType.eof) : (self.nextToken()) {
            if (try self.parseStatement()) |statement| {
                try program.addStatement(statement);
            }
        }

        return program;
    }

    fn parseStatement(self: *Parser) !?Statement {
        return switch (self.currentToken.type) {
            .let => self.parseLetStatement(),
            .ret => self.parseReturnStatement(),
            else => try self.parseExpressionStatement(),
        };
    }

    fn parseLetStatement(self: *Parser) ?Statement {
        const tok = self.currentToken;
        // let statements need to be followed by an identifier
        if (!self.expectPeek(.ident)) {
            return null;
        }

        // TODO: Set value expression
        var let: Statement.Let = undefined;
        let.name = self.currentToken.literal;

        // TODO: Skip value expression for now
        while (!self.currentTokenIs(.semicolon)) {
            self.nextToken();
        }

        return Statement.init(tok, .{
            .let = let,
        });
    }

    fn parseReturnStatement(self: *Parser) ?Statement {
        const statement = Statement.init(self.currentToken, .{
            // TODO: PLACEHODLER FOR ACTUAL EXPRESSION
            .@"return" = null,
        });

        while (!self.currentTokenIs(.semicolon)) {
            self.nextToken();
        }

        return statement;
    }

    // ===============================================
    // Expressions
    // ===============================================
    fn parseExpressionStatement(self: *Parser) !?Statement {
        const tok = self.currentToken;
        const expression = self.parseExpression(.lowest);

        if (expression == null) {
            return null;
        }

        const exprStatement = Statement.ExpressionStatement{
            .expression = expression.?,
        };

        if (self.peekTokenIs(.semicolon)) {
            self.nextToken();
        }

        return Statement.init(tok, .{ .expression = exprStatement });
    }

    fn parseExpression(self: *Parser, precedence: Parser.Precedence) ?*Expression {
        const prefix = self.parsePrefix(self.currentToken.type) catch |err| {
            std.log.err("Error parsing prefix {}\n", .{err});
            return null;
        };

        if (prefix == null) {
            return null;
        }

        var leftExp = prefix.?;

        while (!self.peekTokenIs(.semicolon) and precedence.int() < self.peekPrecedence().int()) {
            if (!shouldParseInfix(self.peekToken.type)) {
                return leftExp;
            }

            self.nextToken();
            leftExp = self.parseInfixExpression(leftExp) catch |err| {
                std.log.err("Error parsing infix: {}\n", .{err});
                return null;
            };
        }

        return leftExp;
    }

    fn parsePrefix(self: *Parser, tokenType: TokenType) !?*Expression {
        return switch (tokenType) {
            .ident => try self.parseIdentifier(),
            .int => self.parseIntLiteral(),
            .bang, .minus => try self.parsePrefixExpression(),
            else => null,
        };
    }

    // fn parseInfix(self: *Parser, tokenType: TokenType, left: *Expression) !?*Expression {
    fn shouldParseInfix(tokenType: TokenType) bool {
        return switch (tokenType) {
            // .eq, .neq, .plus, .minus, .asterisk, .slash, .lt, .gt => self.parseInfixExpression(left),
            .eq, .neq, .plus, .minus, .asterisk, .slash, .lt, .gt => true,
            else => false,
        };
    }

    fn parseInfixExpression(self: *Parser, left: *Expression) !*Expression {
        const tok = self.currentToken;
        const precedence = self.currentPrecedence();
        self.nextToken();

        const right = self.parseExpression(precedence) orelse unreachable;

        return try Expression.initAlloc(self.arena.allocator(), tok, .{
            .infix = .{
                .operator = tok.literal,
                .left = left,
                .right = right,
            },
        });
    }

    fn parsePrefixExpression(self: *Parser) !*Expression {
        const tok = self.currentToken;
        self.nextToken();

        const right = self.parseExpression(.prefix);

        const prefix = Expression.Prefix{
            .right = right,
            .operator = tok.literal,
        };
        return try Expression.initAlloc(self.arena.allocator(), tok, .{
            .prefix = prefix,
        });
    }

    fn parseIdentifier(self: *Parser) !*Expression {
        return try Expression.initAlloc(
            self.arena.allocator(),
            self.currentToken,
            .{
                .identifier = self.currentToken.literal,
            },
        );
    }

    fn parseIntLiteral(self: *Parser) !?*Expression {
        const integer = std.fmt.parseInt(i64, self.currentToken.literal, 10) catch |parseIntErr| {
            self.addError(
                "could not parse {s} as integer at {d}:{d}: {s}",
                .{
                    self.currentToken.literal,
                    self.currentToken.line,
                    self.currentToken.start,
                    @errorName(parseIntErr),
                },
            );

            return null;
        };

        return try Expression.initAlloc(
            self.arena.allocator(),
            self.currentToken,
            .{
                .integer = integer,
            },
        );
    }

    fn expectPeek(self: *Parser, tokenType: TokenType) bool {
        if (self.peekTokenIs(tokenType)) {
            self.nextToken();
            return true;
        }

        self.peekError(tokenType);

        return false;
    }

    fn currentTokenIs(self: *Parser, tokenType: TokenType) bool {
        return self.currentToken.type == tokenType;
    }

    fn peekTokenIs(self: *Parser, tokenType: TokenType) bool {
        return self.peekToken.type == tokenType;
    }

    fn peekError(self: *Parser, tokenType: TokenType) void {
        self.addError("Expected {s} token at {d}:{d} but received \"{s}\"", .{
            @tagName(tokenType),
            self.peekToken.line,
            self.peekToken.start,
            self.peekToken.literal,
        });
    }

    fn peekPrecedence(self: *Parser) Precedence {
        return Precedence.forToken(self.peekToken.type);
    }

    fn currentPrecedence(self: *Parser) Precedence {
        return Precedence.forToken(self.currentToken.type);
    }

    fn addError(self: *Parser, comptime err: []const u8, args: anytype) void {
        const msg = std.fmt.allocPrint(self.arena.allocator(), err, args) catch |fmtErr| {
            std.log.err("Could not format error: {}", .{fmtErr});
            return;
        };

        self.errors.append(msg) catch |caught| {
            std.log.err("Could not add error: \"{s}\" ({})", .{ err, caught });
        };
    }
};

test "parsing let statement" {
    const code =
        \\let a = 10;
    ;

    var lex = lexer.Lexer.init(code);
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, &lex);
    defer parser.deinit();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    try expect(program.statements.items.len == 1);

    const statement = program.statements.items[0];

    try expect(statement.type == .let);
    try expect(statement.token.type == .let);
    try expect(std.mem.eql(u8, statement.type.let.name, "a"));
}

test "parsing return statement" {
    const code =
        \\return a;
    ;

    var lex = lexer.Lexer.init(code);

    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, &lex);
    defer parser.deinit();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    try expect(program.statements.items.len == 1);

    const statement = program.statements.items[0];

    try expect(statement.type == .@"return");
}

test "parse let statement error" {
    const code =
        \\let -
    ;

    var lex = lexer.Lexer.init(code);
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, &lex);
    defer parser.deinit();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    try expect(parser.errorCount() == 1);
    try expect(std.mem.eql(u8, parser.errors.items[0], "Expected ident token at 1:5 but received \"-\""));
}

test "parse identifer statement expression" {
    const code =
        \\hello;
        \\goodbye;
    ;

    var lex = lexer.Lexer.init(code);
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, &lex);
    defer parser.deinit();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    try expect(program.statements.items[0].type == .expression);
    try expect(program.statements.items[1].type == .expression);
}

test "parse integer statement expression" {
    const code =
        \\5;
        \\10;
    ;

    var lex = lexer.Lexer.init(code);
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, &lex);
    defer parser.deinit();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    try expect(program.statements.items[0].type == .expression);
    try expect(program.statements.items[0].type.expression.expression.type.integer == 5);
    try expect(program.statements.items[1].type == .expression);
    try expect(program.statements.items[1].type.expression.expression.type.integer == 10);
}

test "parse integer failure" {
    const code =
        \\100000000000000000000000000000000000000000000000000000000;
    ;

    var lex = lexer.Lexer.init(code);
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, &lex);
    defer parser.deinit();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    try expect(parser.errorCount() == 1);
    try expect(std.mem.eql(u8, parser.errors.items[0], "could not parse 100000000000000000000000000000000000000000000000000000000 as integer at 1:1: Overflow"));
}

test "parse prefix expression" {
    const code =
        \\-10;
        \\!not;
    ;

    var lex = lexer.Lexer.init(code);
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, &lex);
    defer parser.deinit();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    const minusExpressStatement = program.statements.items[0];
    const bangExpressStatement = program.statements.items[1];

    try expect(minusExpressStatement.type.expression.expression.type == .prefix);
    try expect(minusExpressStatement.type.expression.expression.token.type == .minus);
    try expect(minusExpressStatement.type.expression.expression.type.prefix.right.?.type == .integer);
    try expect(std.mem.eql(u8, minusExpressStatement.type.expression.expression.type.prefix.operator, "-"));

    try expect(bangExpressStatement.type.expression.expression.type == .prefix);
    try expect(bangExpressStatement.type.expression.expression.token.type == .bang);
    try expect(bangExpressStatement.type.expression.expression.type.prefix.right.?.type == .identifier);
    try expect(std.mem.eql(u8, bangExpressStatement.type.expression.expression.type.prefix.operator, "!"));
}

test "parse infix expressions" {
    const code =
        \\10 + 10;
        \\5 + 10;
        \\10000000 + 10000000;
    ;

    var lex = lexer.Lexer.init(code);
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, &lex);
    defer parser.deinit();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    const cases = [_]struct { i64, []const u8, i64 }{
        .{ 10, "+", 10 },
        .{ 5, "+", 10 },
        .{ 10000000, "+", 10000000 },
    };

    for (cases, 0..) |case, i| {
        const expr = program.statements.items[i];
        try expect(expr.type.expression.expression.type == .infix);
        try expect(expr.type.expression.expression.type.infix.left.type.integer == case[0]);
        try expect(std.mem.eql(u8, expr.type.expression.expression.type.infix.operator, case[1]));
        try expect(expr.type.expression.expression.type.infix.right.type.integer == case[2]);
    }
}
