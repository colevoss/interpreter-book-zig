const std = @import("std");

const testing = std.testing;

test {
    std.testing.refAllDecls(@This());
}
