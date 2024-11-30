const std = @import("std");
const testing = std.testing;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

fn aa() !void {
    std.json.stringify;
}

pub fn WriteStream(comptime OutStream: type) type {
    return struct {
        const Self = @This();
        pub const Stream = OutStream;
        pub const Error = Stream.Error;

        stream: Stream,

        pub fn init(stream: OutStream) Self {
            return .{
                .stream = stream,
            };
        }

        pub fn deinit(self: *Self) void {
            self.* = undefined;
        }

        pub fn write(self: *Self, value: anytype) Error!void {
            // most of the code here is stolen from std.json stringify or at least
            // the code related to types
            _ = std.builtin.Type;

            const T = @TypeOf(value);
            switch (@typeInfo(T)) {
                .Int => {
                    try self.stream.print("i{}e", .{value});
                    return;
                },
                .ComptimeInt => {
                    return self.write(@as(std.math.IntFittingRange(value, value), value));
                },
                .Struct => |S| {
                    if (std.meta.hasFn(T, "bencodeStringify")) {
                        return value.jsonStringify(self);
                    }

                    if (S.is_tuple) {
                        try self.stream.print("l", .{});
                    } else {
                        try self.stream.print("d", .{});
                    }

                    inline for (S.fields) |Field| {
                        if (Field.type == void) continue;

                        if (!S.is_tuple) {
                            try self.stream.print("{}:{s}", .{ Field.name.len, Field.name });
                        }
                        try self.write(@field(value, Field.name));
                    }

                    try self.stream.print("e", .{});

                    return;
                },
                .Pointer => |ptr_info| switch (ptr_info.size) {
                    .One => switch (@typeInfo(ptr_info.child)) {
                        .Array => {
                            // Coerce `*[N]T` to `[]const T`
                            const Slice = []const std.meta.Elem(ptr_info.child);
                            return self.write(@as(Slice, value));
                        },
                        else => {
                            return self.write(value.*);
                        },
                    },
                    .Many, .Slice => {
                        if (ptr_info.size == .Many and ptr_info.sentinel == null) {
                            @compileError("unable to stringify type '" ++ @typeName(T) ++ "' without sentinel");
                        }
                        const slice = if (ptr_info.size == .Many) std.mem.span(value) else value;

                        if (ptr_info.child == u8) {
                            // this is []const u8, or some similar Zig string.
                            try self.stream.print("{}:{s}", .{ slice.len, slice });
                            return;
                        }
                        try self.stream.print("l", .{});

                        for (slice) |x| {
                            try self.write(x);
                        }

                        try self.stream.print("e", .{});
                        return;
                    },
                    else => @compileError("Unable to stringify type '" ++ @typeName(T) ++ "'"),
                },
                .Array => {
                    return self.write(&value);
                },
                else => @compileError("unable to stringify type '" ++ @typeName(T) ++ "'"),
            }
            unreachable;
        }
    };
}

pub fn stringify(value: anytype, out_stream: anytype) @TypeOf(out_stream).Error!void {
    var jw = WriteStream(@TypeOf(out_stream)).init(out_stream);
    defer jw.deinit();
    try jw.write(value);
}

test "encode number" {
    var out = ArrayList(u8).init(testing.allocator);
    defer out.deinit();

    try stringify(123, out.writer());
    try testing.expectEqualSlices(u8, "i123e", out.items);
}

test "encode negative number" {
    var out = ArrayList(u8).init(testing.allocator);
    defer out.deinit();

    try stringify(-123, out.writer());
    try testing.expectEqualSlices(u8, "i-123e", out.items);
}

test "encode string" {
    var out = ArrayList(u8).init(testing.allocator);
    defer out.deinit();

    try stringify("encode", out.writer());
    try testing.expectEqualSlices(u8, "6:encode", out.items);
}

test "encode list" {
    var out = ArrayList(u8).init(testing.allocator);
    defer out.deinit();

    try stringify([_][]const u8{ "ala", "kot" }, out.writer());
    try testing.expectEqualSlices(u8, "l3:ala3:kote", out.items);
}

test "encode struct" {
    var out = ArrayList(u8).init(testing.allocator);
    defer out.deinit();

    try stringify(.{ .ala = 123 }, out.writer());
    try testing.expectEqualSlices(u8, "d3:alai123ee", out.items);
}
