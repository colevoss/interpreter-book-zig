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
                .lparen => .call,
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
        self.errors.deinit();
        // deinit all arena allocations
        self.arena.deinit();
    }

    pub fn errorCount(self: *const Parser) usize {
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

        if (!self.expectPeek(.assign)) {
            return null;
        }

        // Consume the assign token
        self.nextToken();

        if (self.parseExpression(.lowest)) |value| {
            let.value = value;
        } else {
            return null;
        }

        if (self.peekTokenIs(.semicolon)) {
            self.nextToken();
        }

        return Statement.init(tok, .{
            .let = let,
        });
    }

    fn parseReturnStatement(self: *Parser) ?Statement {
        var statement = Statement.init(self.currentToken, .{
            // TODO: PLACEHODLER FOR ACTUAL EXPRESSION
            .@"return" = null,
        });

        // while (!self.currentTokenIs(.semicolon)) {
        //     self.nextToken();
        // }
        if (self.peekTokenIs(.semicolon)) {
            self.nextToken();
            return statement;
        }

        self.nextToken();

        if (self.parseExpression(.lowest)) |value| {
            statement.type.@"return" = value;
        }

        if (self.peekTokenIs(.semicolon)) {
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
        // std.debug.print("NO PREFIX{}\n", .{self.currentToken});
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

            leftExp = (self.parseInfixExpression(leftExp) catch |err| {
                std.log.err("Error parsing infix: {}\n", .{err});
                return null;
            }).?;
        }

        return leftExp;
    }

    fn parsePrefix(self: *Parser, tokenType: TokenType) !?*Expression {
        return switch (tokenType) {
            .ident => try self.parseIdentifier(),
            .int => try self.parseIntLiteral(),
            .bang, .minus => try self.parsePrefixExpression(),
            .true, .false => try self.parseBoolean(),
            .lparen => self.parseGroupedExpression(),
            .ifst => try self.parseIfExpression(),
            .function => try self.parseFunctionLiteral(),
            else => null,
        };
    }

    fn parsePrefixExpression(self: *Parser) !?*Expression {
        const tok = self.currentToken;
        self.nextToken();

        const right = self.parseExpression(.prefix);

        if (right == null) {
            return null;
        }

        const prefix = Expression.Prefix{
            .right = right orelse unreachable,
            .operator = tok.literal,
        };

        return try Expression.initAlloc(self.arena.allocator(), tok, .{
            .prefix = prefix,
        });
    }

    // =========================================================================
    // INFIX
    // =========================================================================

    fn parseInfixExpression(self: *Parser, left: *Expression) !?*Expression {
        const tok = self.currentToken;
        const precedence = self.currentPrecedence();

        if (self.currentTokenIs(.lparen)) {
            return try self.parseCallExpression(left);
        }

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

    fn shouldParseInfix(tokenType: TokenType) bool {
        return switch (tokenType) {
            .eq, .neq, .plus, .minus, .asterisk, .slash, .lt, .gt => true,
            // for function calls
            .lparen => true,
            else => false,
        };
    }

    fn parseCallExpression(self: *Parser, expr: *Expression) !?*Expression {
        const tok = self.currentToken;
        const args = std.ArrayList(*Expression).init(self.arena.allocator());

        var callExpression = Expression.FunctionCall{
            .function = expr,
            .arguments = args,
        };

        if (self.peekTokenIs(.rparen)) {
            self.nextToken();

            return try Expression.initAlloc(self.arena.allocator(), tok, .{
                .call = callExpression,
            });
        }

        // move past lparen
        self.nextToken();

        if (self.parseExpression(.lowest)) |firstArg| {
            try callExpression.arguments.append(firstArg);
        }

        while (self.peekTokenIs(.comma)) {
            // consume comma
            self.nextToken();
            // consume next expression to prepare for parseExpression
            self.nextToken();

            if (self.parseExpression(.lowest)) |arg| {
                try callExpression.arguments.append(arg);
            }
        }

        if (!self.expectPeek(.rparen)) {
            return null;
        }

        return try Expression.initAlloc(self.arena.allocator(), tok, .{
            .call = callExpression,
        });
    }

    // fn parseCallArguments(self: *Parser) !std.ArrayList(*Expression) {
    //     const args = std.ArrayList(*Expression).init(self.arena.allocator());
    //
    //     if (self.peekTokenIs(.rparen)) {
    //         self.nextToken();
    //         return args;
    //     }
    //
    //     self.nextToken();
    // }

    fn parseGroupedExpression(self: *Parser) ?*Expression {
        self.nextToken();

        const exp = self.parseExpression(.lowest);

        if (!self.expectPeek(.rparen)) {
            return null;
        }

        return exp;
    }

    fn parseIfExpression(self: *Parser) !?*Expression {
        const tok = self.currentToken;

        if (!self.expectPeek(.lparen)) {
            return null;
        }

        // currently the lparen is currentToken so we need to advance once more
        // to get to the condition expression
        self.nextToken();
        const condition = self.parseExpression(.lowest);

        if (condition == null) {
            return null;
        }

        if (!self.expectPeek(.rparen)) {
            return null;
        }

        if (!self.expectPeek(.lbrace)) {
            return null;
        }

        const consequence = try self.parseBlockStatement();

        var ifExpression = Expression.If{
            .condition = condition.?,
            .consequence = consequence,
            .alternative = null,
        };

        // else is optional so we don't need to assert with expectPeek
        if (self.peekTokenIs(.elsest)) {
            // consume the else token because peekTokenIs doesn't
            self.nextToken();

            if (!self.expectPeek(.lbrace)) {
                return null;
            }

            ifExpression.alternative = try self.parseBlockStatement();
        }

        return try Expression.initAlloc(self.arena.allocator(), tok, .{
            .@"if" = ifExpression,
        });
    }

    fn parseFunctionLiteral(self: *Parser) !?*Expression {
        const tok = self.currentToken;

        if (!self.expectPeek(.lparen)) {
            return null;
        }

        var params = std.ArrayList([]const u8).init(self.arena.allocator());

        // set current token to either first param, or closing paren if no params
        self.nextToken();

        while (!self.currentTokenIs(.rparen)) : (self.nextToken()) {
            if (!self.currentTokenIs(.ident)) {
                continue;
            }

            try params.append(self.currentToken.literal);
        }

        if (!self.expectPeek(.lbrace)) {
            params.deinit();
            return null;
        }

        const block = try self.parseBlockStatement();

        return Expression.initAlloc(self.arena.allocator(), tok, .{
            .function = .{
                .parameters = params,
                .body = block,
            },
        });
    }

    fn parseBlockStatement(self: *Parser) !*Statement.Block {
        const tok = self.currentToken;
        var block = try Statement.Block.init(self.arena.allocator(), tok);

        self.nextToken();

        while (!self.currentTokenIs(.rbrace) and !self.currentTokenIs(.eof)) : (self.nextToken()) {
            const statement = try self.parseStatement();

            if (statement) |s| {
                try block.addStatement(s);
            }
        }

        return block;
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

    fn parseBoolean(self: *Parser) !*Expression {
        return try Expression.initAlloc(
            self.arena.allocator(),
            self.currentToken,
            .{
                .boolean = self.currentTokenIs(.true),
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
        \\let b = true;
    ;

    var lex = lexer.Lexer.init(code);
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, &lex);
    defer parser.deinit();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    try expect(program.statementCount() == 2);

    const statementA = program.statements.items[0];
    const statementB = program.statements.items[1];

    try expect(statementA.type == .let);
    try expect(statementA.token.type == .let);
    try expect(std.mem.eql(u8, statementA.type.let.name, "a"));
    try expect(statementA.type.let.value.type == .integer);
    try expect(statementA.type.let.value.type.integer == 10);

    try expect(statementB.type == .let);
    try expect(statementB.token.type == .let);
    try expect(std.mem.eql(u8, statementB.type.let.name, "b"));
    try expect(statementB.type.let.value.type.boolean == true);
}

test "parsing return statement" {
    const code =
        \\return;
        \\return a;
        \\return 10;
    ;

    var lex = lexer.Lexer.init(code);

    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, &lex);
    defer parser.deinit();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    try expect(program.statementCount() == 3);

    const emtpyReturn = program.statements.items[0];
    const returnIdent = program.statements.items[1];
    const returnInt = program.statements.items[2];

    try expect(emtpyReturn.type == .@"return");
    try expect(emtpyReturn.type.@"return" == null);

    try expect(returnIdent.type == .@"return");
    try expect(returnIdent.type.@"return".?.type == .identifier);

    try expect(returnInt.type == .@"return");
    try expect(returnInt.type.@"return".?.type == .integer);
    try expect(returnInt.type.@"return".?.type.integer == 10);
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
    try expect(minusExpressStatement.type.expression.expression.type.prefix.right.type == .integer);
    try expect(std.mem.eql(u8, minusExpressStatement.type.expression.expression.type.prefix.operator, "-"));

    try expect(bangExpressStatement.type.expression.expression.type == .prefix);
    try expect(bangExpressStatement.type.expression.expression.token.type == .bang);
    try expect(bangExpressStatement.type.expression.expression.type.prefix.right.type == .identifier);
    try expect(std.mem.eql(u8, bangExpressStatement.type.expression.expression.type.prefix.operator, "!"));
}

test "parse integer infix expressions" {
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

test "parse boolean expression" {
    const code =
        \\true;
        \\false;
        \\true == true;
        \\!true;
    ;

    var lex = lexer.Lexer.init(code);
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, &lex);
    defer parser.deinit();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    // const writer = std.io.getStdOut().writer();
    // try program.debug(writer);

    const trueExpr = program.statements.items[0];
    try expect(trueExpr.type.expression.expression.type == .boolean);
    try expect(trueExpr.type.expression.expression.type.boolean == true);

    const falseExpr = program.statements.items[1];
    try expect(falseExpr.type.expression.expression.type == .boolean);
    try expect(falseExpr.type.expression.expression.type.boolean == false);

    const eqExpr = program.statements.items[2];
    try expect(eqExpr.type.expression.expression.type == .infix);
    try expect(eqExpr.type.expression.expression.type.infix.left.type == .boolean);
    try expect(eqExpr.type.expression.expression.type.infix.left.type.boolean == true);

    try expect(eqExpr.type.expression.expression.type.infix.right.type == .boolean);
    try expect(eqExpr.type.expression.expression.type.infix.right.type.boolean == true);

    const bangExpr = program.statements.items[3];
    try expect(bangExpr.type.expression.expression.type == .prefix);
    try expect(bangExpr.type.expression.expression.type.prefix.right.type == .boolean);
    try expect(bangExpr.type.expression.expression.type.prefix.right.type.boolean == true);
}

test "parse grouped expressions" {
    const cases = [_]struct { []const u8, []const u8 }{
        .{ "(5 + 5) * 2", "((5 + 5) * 2)" },
        .{ "1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)" },
        .{ "2 / (5 + 5)", "(2 / (5 + 5))" },
        .{ "-(5 + 5)", "(-(5 + 5))" },
        .{ "!(true == true)", "(!(true == true))" },
    };

    const allocator = std.testing.allocator;

    for (cases) |c| {
        var lex = lexer.Lexer.init(c[0]);

        var parser = Parser.init(allocator, &lex);
        defer parser.deinit();

        var program = try parser.parseProgram(allocator);
        defer program.deinit();

        var arr = std.ArrayList(u8).init(allocator);
        defer arr.deinit();

        const writer = arr.writer();

        try program.print(writer);

        try expect(std.mem.eql(u8, arr.items, c[1]));
    }
}

test "parse if statement" {
    const code =
        \\if (a + b == c) {
        \\  true;
        \\} else {
        \\  false;
        \\}
    ;

    var lex = lexer.Lexer.init(code);
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, &lex);
    defer parser.deinit();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    // const writer = std.io.getStdOut().writer();
    // try program.print(writer);
    try expect(program.statements.items.len == 1);
    const ifst = program.statements.items[0];

    try expect(ifst.type.expression.expression.type == .@"if");
    try expect(ifst.type.expression.expression.type.@"if".condition.type == .infix);
    try expect(ifst.type.expression.expression.type.@"if".consequence.statements.items.len == 1);
    try expect(ifst.type.expression.expression.type.@"if".alternative.?.statements.items.len == 1);

    var arr = std.ArrayList(u8).init(allocator);
    defer arr.deinit();

    const writer = arr.writer();

    try program.print(writer);
    try expect(std.mem.eql(u8, arr.items, "if ((a + b) == c) true else false"));
}

test "parse fn literal" {
    const code =
        \\fn(a, b) {
        \\  a + b;
        \\}
    ;

    var lex = lexer.Lexer.init(code);
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, &lex);
    defer parser.deinit();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    try expect(program.statementCount() == 1);

    const fnExpression = program.statements.items[0];

    try expect(fnExpression.type.expression.expression.type == .function);

    const body = fnExpression.type.expression.expression.type.function.body;
    const params = fnExpression.type.expression.expression.type.function.parameters;

    try expect(params.items.len == 2);
    try expect(std.mem.eql(u8, params.items[0], "a"));
    try expect(std.mem.eql(u8, params.items[1], "b"));

    try expect(body.statements.items.len == 1);
}

test "fn literal printing" {
    const cases = [_]struct { []const u8, []const u8 }{
        .{ "fn() {};", "fn()" },
        .{ "fn(a, b) {};", "fn(a, b)" },
        .{ "fn(a, b, c) {};", "fn(a, b, c)" },
        .{ "fn(a, b, c) { true };", "fn(a, b, c) true" },
        .{ "fn(a, b, c) { true; false; };", "fn(a, b, c) truefalse" },
    };

    const allocator = std.testing.allocator;

    for (cases) |c| {
        var lex = lexer.Lexer.init(c[0]);

        var parser = Parser.init(allocator, &lex);
        defer parser.deinit();

        var program = try parser.parseProgram(allocator);
        defer program.deinit();

        var arr = std.ArrayList(u8).init(allocator);
        defer arr.deinit();

        const writer = arr.writer();

        try program.print(writer);

        try expect(std.mem.eql(u8, arr.items, c[1]));
    }
}

test "parse function call" {
    const code =
        \\add(1, 2)
    ;

    var lex = lexer.Lexer.init(code);
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, &lex);
    defer parser.deinit();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    try expect(program.statementCount() == 1);

    const call = program.statements.items[0];

    try expect(call.type.expression.expression.type == .call);
    try expect(call.type.expression.expression.type.call.function.type == .identifier);
    try expect(call.type.expression.expression.type.call.arguments.items.len == 2);

    const argument1 = call.type.expression.expression.type.call.arguments.items[0];
    const argument2 = call.type.expression.expression.type.call.arguments.items[1];

    try expect(std.mem.eql(u8, argument1.token.literal, "1"));
    try expect(std.mem.eql(u8, argument2.token.literal, "2"));
}

test "function calling printing" {
    const cases = [_]struct { []const u8, []const u8 }{
        .{ "test()", "test()" },
        .{ "test(1, 2)", "test(1, 2)" },
        .{ "fn(){}()", "fn()()" },
        .{ "fn(a, b){}(1, 2)", "fn(a, b)(1, 2)" },
        .{ "test(1 + 1, 2, !true, fn(a, b){}(1, 2))", "test((1 + 1), 2, (!true), fn(a, b)(1, 2))" },
    };

    const allocator = std.testing.allocator;

    for (cases) |c| {
        var lex = lexer.Lexer.init(c[0]);

        var parser = Parser.init(allocator, &lex);
        defer parser.deinit();

        var program = try parser.parseProgram(allocator);
        defer program.deinit();

        var arr = std.ArrayList(u8).init(allocator);
        const writer = arr.writer();
        defer arr.deinit();
        // const writer = std.io.getStdOut().writer();

        try program.print(writer);

        try expect(std.mem.eql(u8, arr.items, c[1]));
    }
}
