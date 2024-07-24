const std = @import("std");
const Object = @import("Object.zig");

const expect = std.testing.expect;

pub const Environment = struct {
    store: std.StringHashMap(Object),
    allocator: std.mem.Allocator,
    outer: ?*Environment = null,

    pub fn init(allocator: std.mem.Allocator) Environment {
        const store = std.StringHashMap(Object).init(allocator);

        return .{
            .store = store,
            .allocator = allocator,
        };
    }

    pub fn initEnclosed(allocator: std.mem.Allocator, outer: *Environment) Environment {
        var env = Environment.init(allocator);
        env.outer = outer;

        return env;
    }

    pub fn deinit(self: *Environment) void {
        self.freeKeys();
        self.store.deinit();
    }

    pub fn reset(self: *Environment) void {
        self.freeKeys();
        self.store.clearAndFree();
    }

    fn freeKeys(self: *Environment) void {
        var it = self.store.iterator();

        while (it.next()) |v| {
            self.allocator.free(v.key_ptr.*);
        }
    }

    pub fn get(self: *const Environment, name: []const u8) ?Object {
        if (self.store.get(name)) |obj| {
            return obj;
        }

        if (self.outer) |outer| {
            return outer.get(name);
        }

        return null;
    }

    pub fn set(self: *Environment, name: []const u8, object: Object) !void {
        // need to create a copy of the string so that Environment has ownership
        // of it. Its freed in the deinit
        const key = try self.allocator.dupe(u8, name);
        return self.store.put(key, object);
    }
};

test "get returns null" {
    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    const obj = env.get("should-be-null");

    try expect(obj == null);
}

test "get without outer" {
    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    const obj = Object{ .value = .{ .integer = 10 } };

    try env.set("test", obj);

    const testGetObj = env.get("test").?;

    try expect(obj.value.integer == testGetObj.value.integer);
}

test "get returns enclosing env object when it has outer" {
    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    const outerObj = Object{ .value = .{ .integer = 10 } };
    try env.set("test", outerObj);

    var enclosing = Environment.initEnclosed(std.testing.allocator, &env);
    defer enclosing.deinit();

    const innerObj = Object{ .value = .{ .integer = 20 } };
    try enclosing.set("test", innerObj);

    const gotten = enclosing.get("test").?;

    try expect(gotten.value.integer == innerObj.value.integer);
}

test "get returns outer env object when it has outer" {
    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    const outerObj = Object{ .value = .{ .integer = 10 } };
    try env.set("test", outerObj);

    var enclosing = Environment.initEnclosed(std.testing.allocator, &env);
    defer enclosing.deinit();

    const gotten = enclosing.get("test").?;

    try expect(gotten.value.integer == outerObj.value.integer);
}
