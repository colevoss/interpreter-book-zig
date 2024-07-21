const std = @import("std");

pub const TokenType = enum(u8) {
    illegal,
    eof,

    // identifiers and literals
    ident,
    int,

    // operators
    assign,
    plus,
    minus,
    bang,
    asterisk,
    slash,

    lt,
    gt,

    // delimiters
    comma,
    semicolon,

    lparen,
    rparen,

    lbrace,
    rbrace,

    // keywords
    function,
    let,
    true,
    false,

    // if statement
    ifst,

    // else statment
    elsest,

    // return
    ret,

    // equality
    eq,
    neq,

    pub fn string(self: TokenType) []const u8 {
        return switch (self) {
            .assign => "=",
            .plus => "+",
            .minus => "-",
            .asterisk => "*",
            .slash => "/",
            .lt => "<",
            .gt => ">",
            .eq => "==",
            .neq => "!=",
            else => "",
        };
    }
};

pub const Token = struct {
    type: TokenType,
    literal: []const u8,
    line: usize,
    start: usize,
    end: usize,
};

test "token" {
    const token = Token{
        .type = .assign,
        // .Literal = "hello",
        .literal = &[_]u8{ 'h', 'e', 'l', 'l', 'o' },
        .line = 1,
        .start = 1,
        .end = 1,
    };

    // std.debug.print("LITERAL {s}", .{token.Literal});

    try std.testing.expect(token.type == .assign);
    try std.testing.expect(std.mem.eql(u8, token.literal, "hello"));
}
