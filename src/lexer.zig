const std = @import("std");
const token = @import("token.zig");
const ascii = std.ascii;

pub const Lexer = struct {
    input: []const u8,

    // current position in input (points to current char)
    position: usize = 0,

    // current reading position in input (after current char)
    readPosition: usize = 0,

    col: usize = 0,
    line: usize = 1,

    // Current char under examination
    ch: u8 = 0,

    pub fn init(input: []const u8) Lexer {
        var lexer = Lexer{
            .input = input,
        };

        lexer.readChar();

        return lexer;
    }

    pub fn nextToken(self: *Lexer) token.Token {
        self.skipWhitespace();

        var tok: token.Token = undefined;

        tok.line = self.line;
        tok.start = self.col;
        tok.end = self.col;

        switch (self.ch) {
            '=' => eq: {
                if (self.peekChar() == '=') {
                    // consume the next char
                    self.readChar();

                    tok.type = .eq;
                    tok.literal = "==";
                    tok.end = self.col;

                    break :eq;
                }

                tok.type = .assign;
                tok.literal = "=";
            },

            ';' => {
                tok.type = .semicolon;
                tok.literal = ";";
            },

            '(' => {
                tok.type = .lparen;
                tok.literal = "(";
            },

            ')' => {
                tok.type = .rparen;
                tok.literal = ")";
            },

            '{' => {
                tok.type = .lbrace;
                tok.literal = "{";
            },

            '}' => {
                tok.type = .rbrace;
                tok.literal = "}";
            },

            ',' => {
                tok.type = .comma;
                tok.literal = ",";
            },

            '+' => {
                tok.type = .plus;
                tok.literal = "+";
            },

            '-' => {
                tok.type = .minus;
                tok.literal = "-";
            },

            '*' => {
                tok.type = .asterisk;
                tok.literal = "*";
            },

            '/' => {
                tok.type = .slash;
                tok.literal = "/";
            },

            '!' => bang: {
                if (self.peekChar() == '=') {
                    self.readChar();

                    tok.type = .neq;
                    tok.literal = "!=";
                    tok.end = self.col;

                    break :bang;
                }

                tok.type = .bang;
                tok.literal = "!";
            },

            '>' => {
                tok.type = .gt;
                tok.literal = ">";
            },

            '<' => {
                tok.type = .lt;
                tok.literal = "<";
            },

            0 => {
                tok.type = .eof;
                tok.literal = "";
                tok.start = 0;
                tok.end = 0;
            },

            else => {
                if (ascii.isAlphabetic(self.ch)) {
                    // self.readIdentifierToken(&tok);
                    // tok.type = lookupIdent(tok.literal);

                    tok.literal = self.readIdentifier();
                    tok.type = lookupIdent(tok.literal);
                    // col is incremented to the next character after the identifier
                    // so we need to decremented it here
                    tok.end = self.col - 1;

                    return tok;
                }

                if (ascii.isDigit(self.ch)) {
                    tok.type = .int;
                    tok.literal = self.readDigit();
                    // col is incremented to the next character after the identifier
                    // so we need to decremented it here
                    tok.end = self.col - 1;

                    return tok;
                }

                tok.type = .illegal;
                tok.literal = "";
            },
        }

        self.readChar();

        return tok;
    }

    fn readChar(self: *Lexer) void {
        // If we are past the end of input, set to 0
        if (self.readPosition >= self.input.len) {
            self.ch = 0;
        } else {
            // set Ch to next char
            self.ch = self.input[self.readPosition];
        }

        // set current position to next position
        self.position = self.readPosition;

        // increment next position
        self.readPosition = self.readPosition + 1;

        self.col += 1;
    }

    fn nextLine(self: *Lexer) void {
        self.line += 1;
        self.col = 0;
    }

    fn peekChar(self: *Lexer) u8 {
        if (self.readPosition >= self.input.len) {
            return 0;
        }

        return self.input[self.readPosition];
    }

    fn readIdentifierToken(self: *Lexer, tok: *token.Token) void {
        tok.start = self.position;
        tok.end = self.position;

        while (ascii.isAlphabetic(self.ch)) {
            self.readChar();
            tok.end += 1;
        }

        tok.literal = self.input[tok.start..tok.end];
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        const position = self.position;

        while (ascii.isAlphabetic(self.ch)) {
            self.readChar();
        }

        return self.input[position..self.position];
    }

    fn readDigit(self: *Lexer) []const u8 {
        const position = self.position;

        while (ascii.isDigit(self.ch)) {
            self.readChar();
        }

        return self.input[position..self.position];
    }

    fn skipWhitespace(self: *Lexer) void {
        while (ascii.isWhitespace(self.ch)) {
            if (self.ch == ascii.control_code.lf or self.ch == ascii.control_code.cr) {
                self.nextLine();
            }

            self.readChar();
        }
    }

    fn lookupIdent(ident: []const u8) token.TokenType {
        if (std.mem.eql(u8, ident, "let")) {
            return .let;
        }

        if (std.mem.eql(u8, ident, "fn")) {
            return .function;
        }

        if (std.mem.eql(u8, ident, "true")) {
            return .true;
        }

        if (std.mem.eql(u8, ident, "false")) {
            return .false;
        }

        if (std.mem.eql(u8, ident, "if")) {
            return .ifst;
        }

        if (std.mem.eql(u8, ident, "else")) {
            return .elsest;
        }

        if (std.mem.eql(u8, ident, "return")) {
            return .ret;
        }

        return .ident;
    }
};

test "init" {
    const lexer = Lexer.init("abc");
    try std.testing.expect(lexer.ch == 'a');
}

test "readChar" {
    var lexer = Lexer.init("abc");
    try std.testing.expect(lexer.ch == 'a');

    lexer.readChar();
    try std.testing.expect(lexer.ch == 'b');

    lexer.readChar();
    try std.testing.expect(lexer.ch == 'c');

    lexer.readChar();
    try std.testing.expect(lexer.ch == 0);
}

test "nextToken basic tokens" {
    // var lexer = Lexer.init("=+(){},;-!*/><");
    var lexer = Lexer.init("=+(){},;-!*/><");

    const cases = [_]token.Token{
        .{ .type = .assign, .literal = "=", .line = 1, .start = 1, .end = 1 },
        .{ .type = .plus, .literal = "+", .line = 1, .start = 2, .end = 2 },
        .{ .type = .lparen, .literal = "(", .line = 1, .start = 3, .end = 3 },
        .{ .type = .rparen, .literal = ")", .line = 1, .start = 4, .end = 4 },
        .{ .type = .lbrace, .literal = "{", .line = 1, .start = 5, .end = 5 },
        .{ .type = .rbrace, .literal = "}", .line = 1, .start = 6, .end = 6 },
        .{ .type = .comma, .literal = ",", .line = 1, .start = 7, .end = 7 },
        .{ .type = .semicolon, .literal = ";", .line = 1, .start = 8, .end = 8 },
        .{ .type = .minus, .literal = "-", .line = 1, .start = 9, .end = 9 },
        .{ .type = .bang, .literal = "!", .line = 1, .start = 10, .end = 10 },
        .{ .type = .asterisk, .literal = "*", .line = 1, .start = 11, .end = 11 },
        .{ .type = .slash, .literal = "/", .line = 1, .start = 12, .end = 12 },
        .{ .type = .gt, .literal = ">", .line = 1, .start = 13, .end = 13 },
        .{ .type = .lt, .literal = "<", .line = 1, .start = 14, .end = 14 },
        .{ .type = .eof, .literal = "", .line = 1, .start = 0, .end = 0 },
    };

    for (cases) |case| {
        const t = lexer.nextToken();

        // std.debug.print("Type: {}, Literal: {s}, Start: {}\n", .{ t.type, t.literal, t.start });

        try std.testing.expect(t.type == case.type);
        try std.testing.expect(t.line == case.line);
        try std.testing.expect(t.start == case.start);
        try std.testing.expect(t.start == case.end);
        try std.testing.expect(std.mem.eql(u8, t.literal, case.literal));
    }
}

test "nextToken complex tokens" {
    const code =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\  x + y;
        \\};
        \\let result = add(five, ten);
        \\==
        \\!=
    ;

    var lexer = Lexer.init(code);

    const cases = [_]token.Token{
        .{ .type = token.TokenType.let, .literal = "let", .line = 1, .start = 1, .end = 3 },
        .{ .type = token.TokenType.ident, .literal = "five", .line = 1, .start = 5, .end = 8 },
        .{ .type = token.TokenType.assign, .literal = "=", .line = 1, .start = 10, .end = 10 },
        .{ .type = token.TokenType.int, .literal = "5", .line = 1, .start = 12, .end = 12 },
        .{ .type = token.TokenType.semicolon, .literal = ";", .line = 1, .start = 13, .end = 13 },
        .{ .type = token.TokenType.let, .literal = "let", .line = 2, .start = 1, .end = 3 },
        .{ .type = token.TokenType.ident, .literal = "ten", .line = 2, .start = 5, .end = 7 },
        .{ .type = token.TokenType.assign, .literal = "=", .line = 2, .start = 9, .end = 9 },
        .{ .type = token.TokenType.int, .literal = "10", .line = 2, .start = 11, .end = 12 },
        .{ .type = token.TokenType.semicolon, .literal = ";", .line = 2, .start = 13, .end = 13 },
        .{ .type = token.TokenType.let, .literal = "let", .line = 3, .start = 1, .end = 3 },
        .{ .type = token.TokenType.ident, .literal = "add", .line = 3, .start = 5, .end = 7 },
        .{ .type = token.TokenType.assign, .literal = "=", .line = 3, .start = 9, .end = 9 },
        .{ .type = token.TokenType.function, .literal = "fn", .line = 3, .start = 11, .end = 12 },
        .{ .type = token.TokenType.lparen, .literal = "(", .line = 3, .start = 13, .end = 13 },
        .{ .type = token.TokenType.ident, .literal = "x", .line = 3, .start = 14, .end = 14 },
        .{ .type = token.TokenType.comma, .literal = ",", .line = 3, .start = 15, .end = 15 },
        .{ .type = token.TokenType.ident, .literal = "y", .line = 3, .start = 17, .end = 17 },
        .{ .type = token.TokenType.rparen, .literal = ")", .line = 3, .start = 18, .end = 18 },
        .{ .type = token.TokenType.lbrace, .literal = "{", .line = 3, .start = 20, .end = 20 },
        .{ .type = token.TokenType.ident, .literal = "x", .line = 4, .start = 3, .end = 3 },
        .{ .type = token.TokenType.plus, .literal = "+", .line = 4, .start = 5, .end = 5 },
        .{ .type = token.TokenType.ident, .literal = "y", .line = 4, .start = 7, .end = 7 },
        .{ .type = token.TokenType.semicolon, .literal = ";", .line = 4, .start = 8, .end = 8 },
        .{ .type = token.TokenType.rbrace, .literal = "}", .line = 5, .start = 1, .end = 1 },
        .{ .type = token.TokenType.semicolon, .literal = ";", .line = 5, .start = 2, .end = 2 },
        .{ .type = token.TokenType.let, .literal = "let", .line = 6, .start = 1, .end = 3 },
        .{ .type = token.TokenType.ident, .literal = "result", .line = 6, .start = 5, .end = 10 },
        .{ .type = token.TokenType.assign, .literal = "=", .line = 6, .start = 12, .end = 12 },
        .{ .type = token.TokenType.ident, .literal = "add", .line = 6, .start = 14, .end = 16 },
        .{ .type = token.TokenType.lparen, .literal = "(", .line = 6, .start = 17, .end = 17 },
        .{ .type = token.TokenType.ident, .literal = "five", .line = 6, .start = 18, .end = 21 },
        .{ .type = token.TokenType.comma, .literal = ",", .line = 6, .start = 22, .end = 22 },
        .{ .type = token.TokenType.ident, .literal = "ten", .line = 6, .start = 24, .end = 26 },
        .{ .type = token.TokenType.rparen, .literal = ")", .line = 6, .start = 27, .end = 27 },
        .{ .type = token.TokenType.semicolon, .literal = ";", .line = 6, .start = 28, .end = 28 },
        .{ .type = token.TokenType.eq, .literal = "==", .line = 7, .start = 1, .end = 2 },
        .{ .type = token.TokenType.neq, .literal = "!=", .line = 8, .start = 1, .end = 2 },
        .{ .type = token.TokenType.eof, .literal = "", .line = 8, .start = 0, .end = 0 },
    };

    for (cases) |case| {
        const t = lexer.nextToken();

        std.debug.print("Type: {}, Literal: {s}, Line: {}, Start: {}, End: {}\n", .{ t.type, t.literal, t.line, t.start, t.end });
        try std.testing.expect(t.type == case.type);
        try std.testing.expect(t.line == case.line);
        try std.testing.expect(t.start == case.start);
        try std.testing.expect(t.end == case.end);
        try std.testing.expect(std.mem.eql(u8, t.literal, case.literal));
    }
}
