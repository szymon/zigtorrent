const std = @import("std");
const testing = std.testing;
const assert = std.debug.assert;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const BitStack = std.BitStack;

const default_max_len = 4 * 1024 * 1024;

const OBJECT_MODE = 0;
const LIST_MODE = 1;

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

        pub fn write(self: *Self, value: anytype) Self.Error!void {
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

pub const Error = error{ SyntaxError, UnexpectedEndOfInput };

pub const Token = union(enum) {
    object_begin,
    object_end,
    array_begin,
    array_end,

    number: []const u8,
    string: []const u8,

    end_of_document,
};

///
/// i123e: value -> int -> value
/// 3:ala: value -> string_len -> string -> value
/// l{}e: value -> value (state==[list]) -> end_nested -> value
/// d{}e: value -> value (state==[object]) -> end_nested -> value
pub const TokenType = enum {
    number,
    string,
    object_begin,
    object_end,
    array_begin,
    array_end,
    end_of_document,
};

pub const AllocWhen = enum { alloc_if_needed, alloc_always };

pub const Scanner = struct {
    const Self = @This();

    stack: BitStack,
    input: []const u8 = "",
    cursor: usize = 0,
    state: enum {
        value,
        number,
        string,
        end_nested,
    } = .value,

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
        self.* = undefined;
    }

    pub fn initCompleteInput(allocator: Allocator, complete_input: []const u8) Self {
        return .{
            .input = complete_input,
            .stack = BitStack.init(allocator),
        };
    }

    fn peek(self: Self) ParseFromValueError!u8 {
        if (self.cursor < self.input.len) {
            return self.input[self.cursor];
        } else {
            return ParseFromValueError.UnexpectedEndOfInput;
        }
    }

    pub fn peekNextTokenType(self: Self) ParseFromValueError!TokenType {
        switch (try self.peek()) {
            'i' => {
                return .number;
            },
            '0'...'9' => {
                return .string;
            },
            'l' => {
                return .array_begin;
            },
            'd' => {
                return .object_begin;
            },
            'e' => {
                if (self.stack.bit_len == 0) {
                    return ParseFromValueError.UnexpectedEndOfInput;
                }
                switch (self.stack.peek()) {
                    OBJECT_MODE => {
                        return .object_end;
                    },
                    LIST_MODE => {
                        return .array_end;
                    },
                }
            },
            else => {
                @panic("guwno");
            },
        }
    }

    pub fn next(self: *Self) ParseFromValueError!Token {
        state_loop: while (true) {
            std.debug.print("state={}, cursor={} input={s}\n", .{ self.state, self.cursor, self.input[self.cursor..] });
            if (self.cursor == self.input.len) {
                if (self.state == .value) {
                    return .end_of_document;
                } else {
                    return ParseFromValueError.UnexpectedEndOfInput;
                }
            }
            switch (self.state) {
                .value => {
                    switch (try self.peek()) {
                        'i' => {
                            self.cursor += 1;
                            self.state = .number;
                            continue :state_loop;
                        },
                        '0'...'9' => {
                            self.state = .string;
                            continue :state_loop;
                        },
                        'd' => {
                            self.cursor += 1;
                            try self.stack.push(OBJECT_MODE);
                            return .object_begin;
                        },
                        'l' => {
                            self.cursor += 1;
                            try self.stack.push(LIST_MODE);
                            return .array_begin;
                        },
                        'e' => {
                            self.cursor += 1;
                            if (self.stack.bit_len == 0) {
                                return ParseFromValueError.UnexpectedToken;
                            }
                            switch (self.stack.pop()) {
                                LIST_MODE => {
                                    return .array_end;
                                },
                                OBJECT_MODE => {
                                    return .object_end;
                                },
                            }
                        },
                        else => {
                            @panic("xd");
                        },
                    }
                },
                .number => {
                    const value_start = self.cursor;
                    while (true) {
                        const c = try self.peek();
                        if (c != 'e') {
                            self.cursor += 1;
                        } else {
                            break;
                        }
                    }

                    self.cursor += 1;
                    self.state = .value;

                    return Token{ .number = self.input[value_start .. self.cursor - 1] };
                },
                .string => {
                    const value_start = self.cursor;
                    while (true) {
                        const c = try self.peek();
                        if (c != ':') {
                            self.cursor += 1;
                        } else {
                            break;
                        }
                    }

                    const len = try std.fmt.parseInt(usize, self.input[value_start..self.cursor], 10);
                    self.cursor += 1;

                    if (self.cursor + len > self.input.len) {
                        return ParseFromValueError.UnexpectedEndOfInput;
                    }

                    const string_start = self.cursor;
                    self.cursor += len;
                    self.state = .value;

                    return Token{ .string = self.input[string_start..self.cursor] };
                },
                else => {
                    @panic("unreachable");
                },
            }
        }
    }
};

pub const ParseFromValueError = std.fmt.ParseIntError || std.fmt.ParseFloatError || Allocator.Error || error{
    UnexpectedToken,
    UnexpectedEndOfInput,
};

pub fn parseFromTokenSourceLeaky(comptime T: type, allocator: Allocator, scanner_or_reader: *Scanner) ParseFromValueError!T {
    const value = try innerParse(T, allocator, scanner_or_reader);

    assert(.end_of_document == try scanner_or_reader.next());

    return value;
}

fn innerParse(comptime T: type, allocator: Allocator, source: *Scanner) ParseFromValueError!T {
    std.debug.print("trying to parse: {s}\n", .{@typeName(T)});
    switch (@typeInfo(T)) {
        .Int, .ComptimeInt => {
            const token = try source.next();
            const slice = switch (token) {
                .number => |slice| slice,
                else => return error.UnexpectedToken,
            };
            return std.fmt.parseInt(T, slice, 10);
        },
        .Pointer => |ptr_info| {
            switch (ptr_info.size) {
                .Slice => {
                    const t = try source.next();
                    switch (t) {
                        .string => |slice| {
                            if (ptr_info.child != u8) return error.UnexpectedToken;

                            if (ptr_info.sentinel) |_| {
                                // handle sentinel pointer
                                @panic("unimplemented");
                            }
                            return slice;
                        },

                        .array_begin => {
                            var arraylist = ArrayList(ptr_info.child).init(allocator);
                            while (true) {
                                const tt = try source.peekNextTokenType();
                                std.debug.print("getting next token for array: {}\n", .{tt});
                                switch (tt) {
                                    .array_end => {
                                        _ = try source.next();
                                        std.debug.print("got end of array\n", .{});
                                        break;
                                    },
                                    else => {},
                                }

                                const val = try innerParse(ptr_info.child, allocator, source);
                                std.debug.print("appending value: {}\n", .{val});
                                try arraylist.append(val);
                            }

                            if (ptr_info.sentinel) |_| {
                                @panic("unimplemented");
                            }

                            return arraylist.toOwnedSlice();
                        },
                        else => @panic("unimplemented"),
                    }
                },
                else => @compileError("unable to aprse into type '" ++ @typeName(T) ++ "'"),
            }
        },
        else => @compileError("unable to parse into type '" ++ @typeName(T) ++ "'"),
    }
    unreachable;
}

pub fn parseFromSliceLeaky(comptime T: type, allocator: Allocator, s: []const u8) ParseFromValueError!T {
    var scanner = Scanner.initCompleteInput(allocator, s);
    defer scanner.deinit();

    return parseFromTokenSourceLeaky(T, allocator, &scanner);
}

fn appendSlice(list: *ArrayList(u8), buf: []const u8, max_value_len: usize) !void {
    const new_len = std.math.add(usize, list.items.len, buf.len) catch return error.ValueToLarge;
    if (new_len > max_value_len) return error.ValueToLarge;
    try list.appendSlice(list, buf);
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
test "decode trivial" {
    try testing.expectEqual(123, try parseFromSliceLeaky(i32, testing.allocator, "i123e"));
    try testing.expectEqual(-123, try parseFromSliceLeaky(i32, testing.allocator, "i-123e"));
    try testing.expectEqualSlices(u8, "ala", try parseFromSliceLeaky([]const u8, testing.allocator, "3:ala"));
    const list_result = try parseFromSliceLeaky([]i32, testing.allocator, "li1ei2ei3ee");
    try testing.expectEqualSlices(i32, &[3]i32{ 1, 2, 3 }, list_result);
    defer testing.allocator.free(list_result);
}
