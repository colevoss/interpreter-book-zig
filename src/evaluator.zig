const std = @import("std");
const expect = std.testing.expect;
const ast = @import("ast.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Object = @import("Object.zig");
const TokenType = @import("token.zig").TokenType;
const Environment = @import("environment.zig").Environment;
const math = std.math;
const testing = std.testing;

const Statement = ast.Statement;
const Expression = ast.Expression;
const Program = ast.Program;

const log = std.log.scoped(.evaluator);

pub const Evaluator = struct {
    arena: std.heap.ArenaAllocator,
    environment: *Environment,

    pub fn init(allocoator: std.mem.Allocator, environment: *Environment) Evaluator {
        const arena = std.heap.ArenaAllocator.init(allocoator);

        return .{
            .arena = arena,
            .environment = environment,
        };
    }

    pub fn deinit(self: *Evaluator) void {
        self.arena.deinit();
    }

    pub fn evaluate(self: *Evaluator, node: anytype) Object {
        return node.evaluate(self);
    }

    pub fn evaluateProgram(self: *Evaluator, program: *const Program) Object {
        var object: Object = undefined;

        for (program.statements.items) |s| {
            object = self.evaluateStatement(&s);

            if (object.value == .@"return") {
                // Unwrap return value here to return it from the program
                return object.value.@"return".*;
            }

            if (object.value == .@"error") {
                return object;
            }
        }

        return object;
    }

    pub fn evaluateStatement(self: *Evaluator, statement: *const Statement) Object {
        return switch (statement.type) {
            .expression => |expression| self.evaluateExpression(expression.expression),

            .let => |let| let: {
                const letValueObj = self.evaluateExpression(let.value);

                if (letValueObj.isError()) {
                    return letValueObj;
                }

                self.environment.set(let.name, letValueObj) catch unreachable;

                break :let letValueObj;
            },

            .@"return" => |expression| ret: {
                const returnExpression = self.arena.allocator().create(Object) catch unreachable;

                returnExpression.* = if (expression) |e|
                    self.evaluateExpression(e)
                else
                    Object.nullVal;

                break :ret .{
                    .value = .{
                        // TODO: I think we are going to need to allocate this :(
                        .@"return" = returnExpression,
                    },
                };
            },
            // .block => |block| self.evaluateBlockStatement(block),
            else => Object.nullVal,
        };
    }

    pub fn evaluateBlockStatement(self: *Evaluator, block: *const Statement.Block) Object {
        var object: Object = undefined;

        for (block.statements.items) |s| {
            object = self.evaluateStatement(&s);

            // Do not unwrap the return value here so that outer block statements
            // can work with it
            if (object.value == .@"return" or object.value == .@"error") {
                return object;
            }
        }

        return object;
    }

    pub fn evaluateExpression(self: *Evaluator, expression: *const Expression) Object {
        return switch (expression.type) {
            .integer => |i| .{
                .value = .{
                    .integer = i,
                },
            },

            .boolean => |b| {
                if (b) {
                    return Object.trueVal;
                } else {
                    return Object.falseVal;
                }
            },

            .prefix => self.evaluatePrefixExpression(expression),

            .infix => self.evaluateInfixExpression(expression),

            .@"if" => self.evaluateIfExpression(expression),

            .identifier => self.evaluateIdnetifierExpression(expression),

            .function => self.evaluateFunctionLiteral(expression),
            .call => self.evaluateFunctionCallExpression(expression),

            // else => Object.nullVal,
        };
    }

    fn evaluatePrefixExpression(self: *Evaluator, expression: *const Expression) Object {
        std.debug.assert(expression.type == .prefix);

        const right = self.evaluateExpression(expression.type.prefix.right);

        if (right.isError()) {
            return right;
        }

        return switch (expression.token.type) {
            .bang => self.evaluateBangExpression(right),
            .minus => self.evaluateMinusExpression(right),
            else => return Object.initError(self.arena.allocator(), "uknown operator: {s}", .{@tagName(expression.token.type)}),
        };
    }

    fn evaluateMinusExpression(self: *Evaluator, object: Object) Object {
        if (object.value != .integer) {
            return Object.initError(self.arena.allocator(), "unknown operator -{s}", .{
                object.value.string(self.arena.allocator()) catch unreachable,
            });
        }

        return .{
            .value = .{
                .integer = -object.value.integer,
            },
        };
    }

    fn evaluateBangExpression(_: *const Evaluator, object: Object) Object {
        if (object.value == .null) {
            return Object.trueVal;
        }

        if (object.value != .boolean) {
            return Object.falseVal;
        }

        if (object.value.boolean == true) {
            return Object.falseVal;
        }

        // should be boolean = false here
        return Object.trueVal;
    }

    fn evaluateInfixExpression(self: *Evaluator, expression: *const Expression) Object {
        std.debug.assert(expression.type == .infix);

        const left = self.evaluateExpression(expression.type.infix.left);

        if (left.isError()) {
            return left;
        }

        const right = self.evaluateExpression(expression.type.infix.right);

        if (right.isError()) {
            return right;
        }

        if (self.areTypesMismatched(left, right)) |err| {
            return err;
        }

        if (left.value == .integer and right.value == .integer) {
            return self.evalIntegerInfixExpression(expression.token.type, left, right);
        }

        if (left.value == .boolean and right.value == .boolean) {
            return self.evalBoolInfixExpression(expression.token.type, left, right);
        }

        return Object.nullVal;
    }

    fn areTypesMismatched(self: *Evaluator, left: Object, right: Object) ?Object {
        if (@as(Object.Type, left.value) == @as(Object.Type, right.value)) {
            return null;
        }

        return Object.initError(self.arena.allocator(), "type mismatch: {s} ({s}) and {s} ({s})", .{
            @tagName(@as(Object.Type, left.value)),
            left.value.string(self.arena.allocator()) catch unreachable,
            @tagName(@as(Object.Type, right.value)),
            right.value.string(self.arena.allocator()) catch unreachable,
        });
    }

    fn evalBoolInfixExpression(self: *Evaluator, operator: TokenType, left: Object, right: Object) Object {
        std.debug.assert(left.value == .boolean);
        std.debug.assert(right.value == .boolean);

        const result = switch (operator) {
            .eq => left.value.boolean == right.value.boolean,
            .neq => left.value.boolean != right.value.boolean,

            else => {
                return Object.initError(self.arena.allocator(), "unknown operator {s} {s} {s}", .{
                    left.value.string(self.arena.allocator()) catch unreachable,
                    // @tagName(operator),
                    operator.string(),
                    right.value.string(self.arena.allocator()) catch unreachable,
                });
            },
        };

        return .{ .value = .{ .boolean = result } };
    }

    fn evalIntegerInfixExpression(_: *Evaluator, operator: TokenType, left: Object, right: Object) Object {
        std.debug.assert(left.value == .integer);
        std.debug.assert(right.value == .integer);

        switch (operator) {
            .plus => {
                const result = math.add(isize, left.value.integer, right.value.integer) catch |e| {
                    log.info("Addition resulted in error {s}", .{@errorName(e)});
                    return Object.nullVal;
                };

                return .{ .value = .{ .integer = result } };
            },

            .minus => {
                const result = math.sub(isize, left.value.integer, right.value.integer) catch |e| {
                    log.info("Subtraction resulted in {s} error", .{@errorName(e)});
                    return Object.nullVal;
                };

                return .{ .value = .{ .integer = result } };
            },

            .asterisk => {
                const result = math.mul(isize, left.value.integer, right.value.integer) catch |e| {
                    log.info("Multiplication resulted in {s} error", .{@errorName(e)});
                    return Object.nullVal;
                };

                return .{ .value = .{ .integer = result } };
            },

            .slash => {
                const result = math.divExact(isize, left.value.integer, right.value.integer) catch |e| {
                    log.info("Division resulted in {s} error", .{@errorName(e)});
                    return Object.nullVal;
                };

                return .{ .value = .{ .integer = result } };
            },

            .lt => return .{ .value = .{ .boolean = left.value.integer < right.value.integer } },
            .gt => return .{ .value = .{ .boolean = left.value.integer > right.value.integer } },

            .eq => return .{ .value = .{ .boolean = left.value.integer == right.value.integer } },
            .neq => return .{ .value = .{ .boolean = left.value.integer != right.value.integer } },
            // we could add error handling here
            else => return Object.nullVal,
        }
    }

    fn evaluateIfExpression(self: *Evaluator, expression: *const Expression) Object {
        std.debug.assert(expression.type == .@"if");

        const conditionObject = self.evaluateExpression(expression.type.@"if".condition);

        if (conditionObject.isError()) {
            return conditionObject;
        }

        if (isTruthyObj(conditionObject)) {
            return self.evaluateBlockStatement(expression.type.@"if".consequence);
        }

        if (expression.type.@"if".alternative) |alt| {
            return self.evaluateBlockStatement(alt);
        }

        return Object.nullVal;
    }

    fn evaluateIdnetifierExpression(self: *Evaluator, expression: *const Expression) Object {
        std.debug.assert(expression.type == .identifier);

        const environmentObj = self.environment.get(expression.type.identifier);

        if (environmentObj) |obj| {
            return obj;
        }

        log.debug("NO IDENTIFIER", .{});

        return Object.initError(self.arena.allocator(), "identifier not found: {s}", .{expression.type.identifier});
    }

    fn evaluateFunctionLiteral(self: *Evaluator, expression: *const Expression) Object {
        std.debug.assert(expression.type == .function);

        return Object{
            .value = .{
                .function = .{
                    .env = self.environment,
                    .parameters = expression.type.function.parameters,
                    .body = expression.type.function.body,
                },
            },
        };
    }

    fn evaluateFunctionCallExpression(self: *Evaluator, expression: *const Expression) Object {
        std.debug.assert(expression.type == .call);
        log.debug("Evaluating function expression", .{});

        const function = self.evaluateExpression(expression.type.call.function);

        if (function.isError()) {
            return function;
        }

        const argCount = expression.type.call.arguments.items.len;
        var args = std.ArrayList(Object).initCapacity(self.arena.allocator(), argCount) catch unreachable;
        defer args.deinit();

        for (expression.type.call.arguments.items) |arg| {
            const argExpression = self.evaluateExpression(arg);

            if (argExpression.isError()) {
                return argExpression;
            }

            args.append(argExpression) catch |e| {
                std.debug.print("{}\n", .{e});
            };
        }

        return self.applyFunction(function, args.items) catch unreachable;
    }

    fn applyFunction(self: *Evaluator, function: Object, args: []Object) !Object {
        if (function.value != .function) {
            return Object.initError(
                self.arena.allocator(),
                "not a function: {s}",
                .{function.value.string(self.arena.allocator()) catch unreachable},
            );
        }

        var enclosed = Environment.initEnclosed(self.arena.allocator(), function.value.function.env);

        // map function literal parameters to the arguments being passed to the function call
        for (function.value.function.parameters.items, 0..) |param, argIdx| {
            try enclosed.set(param, args[argIdx]);
        }

        self.environment = &enclosed;

        defer {
            self.environment = enclosed.outer.?;
            enclosed.deinit();
        }

        const result = self.evaluateBlockStatement(function.value.function.body);

        if (result.value == .@"return") {
            return result.value.@"return".*;
        }

        return result;
    }

    fn isTruthyObj(object: Object) bool {
        switch (object.value) {
            .boolean => |b| return b,
            .null => return false,
            else => return true,
        }
    }
};

fn testEvaluateCode(allocator: std.mem.Allocator, evaluator: *Evaluator, code: []const u8) !Object {
    var lex = Lexer.init(code);
    var parser = Parser.init(allocator, &lex);
    defer parser.deinit();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    return evaluator.evaluate(&program);
}

fn TestCase(comptime expected: type) type {
    return struct { []const u8, expected };
}

test "evaluate integer expresionos" {
    var env = Environment.init(testing.allocator);
    defer env.deinit();

    var e = Evaluator.init(std.testing.allocator, &env);
    defer e.deinit();

    var expression: Expression = undefined;
    expression.type = .{ .integer = 1 };

    const o = e.evaluate(&expression);

    try expect(o.value.integer == 1);
}

test "evaluate boolean expresionos" {
    var env = Environment.init(testing.allocator);
    defer env.deinit();

    var e = Evaluator.init(std.testing.allocator, &env);
    defer e.deinit();

    var expression: Expression = undefined;
    expression.type = .{ .boolean = true };

    const o = e.evaluate(&expression);

    try expect(o.value.boolean == true);
}

test "evaluate expression statements" {
    var env = Environment.init(testing.allocator);
    defer env.deinit();

    var e = Evaluator.init(std.testing.allocator, &env);
    defer e.deinit();

    var expression: Expression = undefined;
    expression.type = .{ .boolean = true };

    var statement: Statement = undefined;
    statement.type = .{
        .expression = .{ .expression = &expression },
    };

    const object = e.evaluate(&statement);

    try expect(object.value == .boolean);
}

test "evaluate bang prefix" {
    const cases = [_]TestCase(bool){
        .{ "!true", false },
        .{ "!false", true },
        .{ "!1", false },
    };

    var env = Environment.init(testing.allocator);
    defer env.deinit();

    var evaluator = Evaluator.init(testing.allocator, &env);
    defer evaluator.deinit();

    for (cases) |case| {
        const object = try testEvaluateCode(std.testing.allocator, &evaluator, case[0]);
        try expect(object.value.boolean == case[1]);
    }
}

test "evaluate minus prefix" {
    const cases = [_]TestCase(isize){
        .{ "-10", -10 },
        .{ "--10", 10 },
    };

    var env = Environment.init(testing.allocator);
    defer env.deinit();

    var evaluator = Evaluator.init(testing.allocator, &env);
    defer evaluator.deinit();

    for (cases) |case| {
        const object = try testEvaluateCode(std.testing.allocator, &evaluator, case[0]);
        try expect(object.value.integer == case[1]);
    }
}

test "evaluate infix integer expression" {
    const cases = [_]TestCase(isize){
        .{ "5", 5 },
        .{ "10", 10 },
        .{ "-5", -5 },
        .{ "-10", -10 },
        .{ "5 + 5 + 5 + 5 - 10", 10 },
        .{ "2 * 2 * 2 * 2 * 2", 32 },
        .{ "-50 + 100 + -50", 0 },
        .{ "5 * 2 + 10", 20 },
        .{ "5 + 2 * 10", 25 },
        .{ "20 + 2 * -10", 0 },
        .{ "50 / 2 * 2 + 10", 60 },
        .{ "2 * (5 + 10)", 30 },
        .{ "3 * 3 * 3 + 10", 37 },
        .{ "3 * (3 * 3) + 10", 37 },
        .{ "(5 + 10 * 2 + 15 / 3) * 2 + -10", 50 },
    };

    var env = Environment.init(testing.allocator);
    defer env.deinit();

    var evaluator = Evaluator.init(testing.allocator, &env);
    defer evaluator.deinit();

    for (cases) |case| {
        const object = try testEvaluateCode(std.testing.allocator, &evaluator, case[0]);
        try expect(object.value.integer == case[1]);
    }
}

test "evaluate infix nullifies on math error" {
    const allocator = std.testing.allocator;

    var env = Environment.init(testing.allocator);
    defer env.deinit();

    var evaluator = Evaluator.init(testing.allocator, &env);
    defer evaluator.deinit();

    const max = math.maxInt(isize);
    const min = math.minInt(isize);

    // Addition
    const addCode = try std.fmt.allocPrint(allocator, "{d} + {d}", .{ max, max });
    defer allocator.free(addCode);

    const addObj = try testEvaluateCode(allocator, &evaluator, addCode);
    try expect(addObj.value == .null);

    // Subtraction
    const subCode = try std.fmt.allocPrint(allocator, "{d} - {d}", .{ min, min });
    defer allocator.free(subCode);

    const subObj = try testEvaluateCode(allocator, &evaluator, subCode);
    try expect(subObj.value == .null);

    // Multiplication
    const multCode = try std.fmt.allocPrint(allocator, "{d} * {d}", .{ max, max });
    defer allocator.free(multCode);

    const multObj = try testEvaluateCode(allocator, &evaluator, multCode);
    try expect(multObj.value == .null);

    // Division
    const divCode = "1 / 2";

    const divObj = try testEvaluateCode(allocator, &evaluator, divCode);
    try expect(divObj.value == .null);
}

test "evaluate infix boolean expressions" {
    const cases = [_]TestCase(bool){
        .{ "true", true },
        .{ "false", false },
        .{ "1 < 2", true },
        .{ "1 > 2", false },
        .{ "1 < 1", false },
        .{ "1 > 1", false },
        .{ "1 == 1", true },
        .{ "1 != 1", false },
        .{ "1 == 2", false },
        .{ "1 != 2", true },
        .{ "true == true", true },
        .{ "false == false", true },
        .{ "true == false", false },
        .{ "true != false", true },
        .{ "false != true", true },
        .{ "(1 < 2) == true", true },
        .{ "(1 < 2) == false", false },
        .{ "(1 > 2) == true", false },
        .{ "(1 > 2) == false", true },
    };

    var env = Environment.init(testing.allocator);
    defer env.deinit();

    var evaluator = Evaluator.init(testing.allocator, &env);
    defer evaluator.deinit();

    for (cases) |case| {
        const object = try testEvaluateCode(std.testing.allocator, &evaluator, case[0]);
        try expect(object.value.boolean == case[1]);
    }
}

test "isTruthyObj" {
    const cases = [_]struct { Object, bool }{
        .{ Object{ .value = .{ .boolean = true } }, true },
        .{ Object{ .value = .{ .boolean = false } }, false },
        .{ Object{ .value = .{ .null = 0 } }, false },
        .{ Object{ .value = .{ .integer = 10 } }, true },
        .{ Object{ .value = .{ .integer = 0 } }, true },
    };

    for (cases) |case| {
        try expect(Evaluator.isTruthyObj(case[0]) == case[1]);
    }
}

test "evaluate if statements" {
    const cases = [_]TestCase(isize){
        // Consequence
        .{ "if (true) { 10 }", 10 },
        .{ "if (2 > 1) { 10 }", 10 },
        .{ "if (1 < 2) { 10 }", 10 },
        .{ "if (1 < 2) { 10 }", 10 },
        .{ "if (2 == 2) { 10 }", 10 },
        .{ "if (1 != 2) { 10 }", 10 },
        // Alternative
        .{ "if (false) { 10 } else { 20 }", 20 },
        .{ "if (2 < 1) { 10 } else { 20 }", 20 },
        .{ "if (1 > 2) { 10 } else { 20 }", 20 },
        .{ "if (1 > 2) { 10 } else { 20 }", 20 },
        .{ "if (2 != 2) { 10 } else { 20 }", 20 },
        .{ "if (1 == 2) { 10 } else { 20 }", 20 },
    };

    var env = Environment.init(testing.allocator);
    defer env.deinit();

    var evaluator = Evaluator.init(testing.allocator, &env);
    defer evaluator.deinit();

    for (cases) |case| {
        const object = try testEvaluateCode(std.testing.allocator, &evaluator, case[0]);
        try expect(object.value.integer == case[1]);
    }
}

test "evaluate return expressions" {
    const cases = [_]TestCase(isize){
        .{ "return 10;", 10 },
        .{ "if (true) { return 10 }", 10 },
        .{ "if (true) { return 10; 20; }", 10 },
        .{
            \\if (true) {
            \\  if (true) {
            \\    if (true) {
            \\      return 5
            \\    }
            \\    return 10;
            \\  }
            \\  return 20;
            \\}
            ,
            5,
        },
    };

    var env = Environment.init(testing.allocator);
    defer env.deinit();

    var evaluator = Evaluator.init(testing.allocator, &env);
    defer evaluator.deinit();

    for (cases) |case| {
        const object = try testEvaluateCode(std.testing.allocator, &evaluator, case[0]);
        try expect(object.value.integer == case[1]);
    }

    const nullObj = try testEvaluateCode(std.testing.allocator, &evaluator, "return;");
    try expect(nullObj.value == .null);
}

test "error handling" {
    const cases = [_]TestCase([]const u8){
        .{ "-true", "unknown operator -true" },
        .{ "-(1 != 1)", "unknown operator -false" },
        .{ "-false", "unknown operator -false" },
        .{ "(1 == 1) * 1", "type mismatch: boolean (true) and integer (1)" },
        .{ "1 - (2 < 1)", "type mismatch: integer (1) and boolean (false)" },
        .{ "1 + true", "type mismatch: integer (1) and boolean (true)" },
        .{ "true + 10", "type mismatch: boolean (true) and integer (10)" },
        .{ "(1 != 1) + (1 * 2)", "type mismatch: boolean (false) and integer (2)" },
        .{ "true + false", "unknown operator true + false" },
        .{ "false * false", "unknown operator false * false" },
        .{ "if (true) { false * false }", "unknown operator false * false" },
        .{ "if (true * 1) { false * false }", "type mismatch: boolean (true) and integer (1)" },
    };

    var env = Environment.init(testing.allocator);
    defer env.deinit();

    var evaluator = Evaluator.init(testing.allocator, &env);
    defer evaluator.deinit();

    for (cases) |case| {
        const object = try testEvaluateCode(std.testing.allocator, &evaluator, case[0]);
        // std.debug.print("{s} results in:\n\t\"{s}\"\n", .{ case[0], object.value.@"error" });
        try expect(std.mem.eql(u8, object.value.@"error", case[1]));
    }
}

test "evaluate let statements" {
    const cases = [_]TestCase(isize){
        .{ "let a = 10; a;", 10 },
        .{ "let a = 10 + 10; a;", 20 },
        .{
            \\let a = 10;
            \\if (true) {
            \\  a;
            \\}
            ,
            10,
        },
        .{
            \\let a = 10;
            \\let b = 20;
            \\if (a > b) {
            \\  a;
            \\} else {
            \\  b;
            \\}
            ,
            20,
        },
        .{
            \\let a = 10;
            \\let b = 20;
            \\if (a < b) {
            \\  a;
            \\} else {
            \\  b;
            \\}
            ,
            10,
        },
        .{
            \\let a = 10;
            \\let b = 20;
            \\let c = 30
            \\if (a + b == c) {
            \\  a;
            \\} else {
            \\  b;
            \\}
            ,
            10,
        },
    };

    var env = Environment.init(testing.allocator);
    defer env.deinit();

    var evaluator = Evaluator.init(testing.allocator, &env);
    defer evaluator.deinit();

    for (cases) |case| {
        const object = try testEvaluateCode(std.testing.allocator, &evaluator, case[0]);
        // std.debug.print("{s} results in:\n{d}\n", .{ case[0], object.value.integer });
        try expect(object.value.integer == case[1]);
        env.reset();
    }
}

test "testing memory leak" {
    const cases = [_]TestCase(isize){.{
        \\let a = 10;
        \\let b = 20;
        \\a + b;
        ,
        30,
    }};

    var env = Environment.init(testing.allocator);
    defer env.deinit();

    var evaluator = Evaluator.init(testing.allocator, &env);
    defer evaluator.deinit();

    for (cases) |case| {
        const object = try testEvaluateCode(std.testing.allocator, &evaluator, case[0]);
        // std.debug.print("{s} results in:\n{d}\n", .{ case[0], object.value.integer });
        try expect(object.value.integer == case[1]);
    }
}

test "evaluate function literal" {
    const code =
        \\let x = fn(x) {
        \\  x + 2;
        \\}
        \\
    ;

    var env = Environment.init(testing.allocator);
    defer env.deinit();

    var evaluator = Evaluator.init(testing.allocator, &env);
    defer evaluator.deinit();

    var lex = Lexer.init(code);
    var parser = Parser.init(std.testing.allocator, &lex);
    defer parser.deinit();

    var program = try parser.parseProgram(std.testing.allocator);
    defer program.deinit();

    const obj = evaluator.evaluate(&program);

    try expect(obj.value == .function);
    try expect(obj.value.function.parameters.items.len == 1);
    try expect(std.mem.eql(u8, obj.value.function.parameters.items[0], "x"));
}

test "evaluate function call" {
    const cases = [_]TestCase(isize){
        .{
            \\let addOne = fn(x) {
            \\  return x + 1;
            \\};
            \\
            \\let addTwo = fn(x) {
            \\  let addedOne = addOne(x);
            \\  return addOne(addedOne);
            \\};
            \\
            \\return 4 + addTwo(1);
            ,
            7,
        },
        // THIS CRASHES
        // .{
        //     \\let newAdder = fn(x) {
        //     \\  return fn(y) { return x + y; };
        //     \\}
        //     \\
        //     \\let addTwo = newAdder(2);
        //     \\return addTwo(2);
        //     ,
        //     4,
        // },
        .{
            "fn(x) { x + 10; }(5)",
            15,
        },
    };

    var env = Environment.init(testing.allocator);
    defer env.deinit();

    var evaluator = Evaluator.init(testing.allocator, &env);
    defer evaluator.deinit();

    for (cases) |case| {
        const object = try testEvaluateCode(std.testing.allocator, &evaluator, case[0]);
        try expect(object.value.integer == case[1]);
    }
}
