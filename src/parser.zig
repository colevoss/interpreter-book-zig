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

    pub fn init(lex: *lexer.Lexer) Parser {
        var parser: Parser = undefined;
        parser.lexer = lex;

        parser.nextToken();

        return parser;
    }

    pub fn nextToken(self: *Parser) void {
        self.currentToken = self.peekToken;
        self.peekToken = self.lexer.nextToken();
    }

    pub fn parseProgram(self: *Parser, allocator: Allocator) !ast.Program {
        var program = ast.Program.init(allocator);

        while (self.currentToken.type != token.TokenType.eof) : (self.nextToken()) {
            if (self.parseStatement()) |statement| {
                _ = try program.addStatement(statement);
            }

            // const stmt = self.parseStatement();
            // if (stmt) |s| {
            //     _ = try program.addStatement(s);
            // }
        }

        return program;
    }

    pub fn parseStatement(self: *Parser) ?Statement {
        return switch (self.currentToken.type) {
            .let => self.parseLetStatement(),
            .ret => self.parseReturnStatement(),
            else => return null,
        };
    }

    pub fn parseLetStatement(self: *Parser) ?Statement {
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

    fn expectPeek(self: *Parser, tokenType: TokenType) bool {
        if (self.peekTokenIs(tokenType)) {
            self.nextToken();
            return true;
        }

        return false;
    }

    fn currentTokenIs(self: *Parser, tokenType: TokenType) bool {
        return self.currentToken.type == tokenType;
    }

    fn peekTokenIs(self: *Parser, tokenType: TokenType) bool {
        return self.peekToken.type == tokenType;
    }
};

test "parsing let statement" {
    const code =
        \\let a = 10;
    ;

    var lex = lexer.Lexer.init(code);
    var parser = Parser.init(&lex);

    const allocator = std.testing.allocator;
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
    var parser = Parser.init(&lex);

    const allocator = std.testing.allocator;
    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    try expect(program.statements.items.len == 1);

    const statement = program.statements.items[0];

    try expect(statement == .ret);
}
