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
    };

    pub fn init(allocator: Allocator, lex: *lexer.Lexer) Parser {
        var parser: Parser = undefined;
        parser.lexer = lex;

        parser.allocator = allocator;
        parser.errors = std.ArrayList([]const u8).init(parser.allocator);

        parser.nextToken();
        parser.nextToken();

        return parser;
    }

    pub fn deini(self: *Parser) void {
        for (self.errors.items) |e| {
            self.allocator.free(e);
        }

        self.errors.deinit();
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
            if (self.parseStatement()) |statement| {
                try program.addStatement(statement);
            }
        }

        return program;
    }

    fn addError(self: *Parser, comptime err: []const u8, args: anytype) void {
        const msg = std.fmt.allocPrint(self.allocator, err, args) catch |fmtErr| {
            std.log.err("Could not format error: {}", .{fmtErr});
            return;
        };

        self.errors.append(msg) catch |caught| {
            std.log.err("Could not add error: \"{s}\" ({})", .{ err, caught });
        };
    }

    fn parseStatement(self: *Parser) ?Statement {
        return switch (self.currentToken.type) {
            .let => self.parseLetStatement(),
            .ret => self.parseReturnStatement(),
            else => self.parseExpressionStatement(),
        };
    }

    fn parseLetStatement(self: *Parser) ?Statement {
        var statement: Statement.Let = undefined;
        statement.token = self.currentToken;

        // variable name
        if (!self.expectPeek(.ident)) {
            return null;
        }

        statement.name = Expression.Identifier{
            .token = self.currentToken,
            .value = self.currentToken.literal,
        };

        // equals sign
        if (!self.expectPeek(.assign)) {
            return null;
        }

        // TODO: Skip until semi colon
        while (!self.currentTokenIs(.semicolon)) {
            self.nextToken();
        }

        return Statement{ .let = statement };
    }

    pub fn parseReturnStatement(self: *Parser) ?Statement {
        var ret: Statement.Return = undefined;
        ret.token = self.currentToken;

        // TODO: parse identifier
        while (!self.currentTokenIs(.semicolon)) {
            self.nextToken();
        }

        return Statement{ .ret = ret };
    }

    // ===============================================
    // Expressions
    // ===============================================
    fn parseExpressionStatement(self: *Parser) ?Statement {
        const tok = self.currentToken;
        const expression = self.parseExpression(.lowest);

        if (expression == null) {
            return null;
        }

        if (self.peekTokenIs(.semicolon)) {
            self.nextToken();
        }

        return Statement{
            .expression = .{
                .expression = expression,
                .token = tok,
            },
        };
    }

    fn parseExpression(self: *Parser, _: Parser.Precedence) ?Expression {
        const prefix = self.parsePrefix(self.currentToken.type);

        if (prefix == null) {
            return null;
        }

        const leftExp = prefix;

        return leftExp;
    }

    fn parsePrefix(self: *Parser, tokenType: TokenType) ?Expression {
        return switch (tokenType) {
            .ident => self.parseIdentifier(),
            .int => self.parseIntLiteral(),
            // .bang, .minus => self.parsePrefixExpression(),
            else => null,
        };
    }

    // fn parsePrefixExpression(self: *Parser) ?Expression {
    //     const tok = self.currentToken;
    //     const operator = self.currentToken.literal;
    //
    //     self.nextToken();
    //
    //     const right = self.parseExpression(.prefix);
    //
    //     return Expression{
    //         .prefix = .{
    //             .token = tok,
    //             .operator = operator,
    //             .right = right,
    //         },
    //     };
    // }

    fn parseIdentifier(self: *Parser) Expression {
        const identifier = Expression.Identifier{
            .token = self.currentToken,
            .value = self.currentToken.literal,
        };

        return Expression{ .identifier = identifier };
    }

    fn parseIntLiteral(self: *Parser) ?Expression {
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

        return Expression{
            .integerLiteral = .{
                .value = integer,
                .token = self.currentToken,
            },
        };
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
        const message = std.fmt.allocPrint(self.allocator, "Expected {s} token at {d}:{d} but received \"{s}\"", .{
            @tagName(tokenType),
            self.peekToken.line,
            self.peekToken.start,
            self.peekToken.literal,
        }) catch |err| {
            std.log.err("Error creating peekError message {}", .{err});
            return;
        };

        self.errors.append(message) catch |err| {
            std.log.err("Error creating peekError message {}", .{err});
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
    defer parser.deini();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    try expect(program.statements.items.len == 1);

    const statement = program.statements.items[0];

    try expect(statement == .let);
    try expect(std.mem.eql(u8, statement.let.name.value, "a"));
}

test "parsing return statement" {
    const code =
        \\return a;
    ;

    var lex = lexer.Lexer.init(code);

    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, &lex);
    defer parser.deini();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    try expect(program.statements.items.len == 1);

    const statement = program.statements.items[0];

    try expect(statement == .ret);
}

test "parse let statement error" {
    const code =
        \\let -
    ;

    var lex = lexer.Lexer.init(code);
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, &lex);
    defer parser.deini();

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
    defer parser.deini();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    try expect(program.statements.items[0] == .expression);
    try expect(program.statements.items[1] == .expression);
}

test "parse integer statement expression" {
    const code =
        \\5;
        \\10;
    ;

    var lex = lexer.Lexer.init(code);
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, &lex);
    defer parser.deini();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    try expect(program.statements.items[0] == .expression);
    try expect(program.statements.items[0].expression.expression.?.integerLiteral.value == 5);
    try expect(program.statements.items[1] == .expression);
    try expect(program.statements.items[1].expression.expression.?.integerLiteral.value == 10);
}

test "parse integer failure" {
    const code =
        \\100000000000000000000000000000000000000000000000000000000;
    ;

    var lex = lexer.Lexer.init(code);
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, &lex);
    defer parser.deini();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    try expect(parser.errorCount() == 1);
    try expect(std.mem.eql(u8, parser.errors.items[0], "could not parse 100000000000000000000000000000000000000000000000000000000 as integer at 1:1: Overflow"));
}
