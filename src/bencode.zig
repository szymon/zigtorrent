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
const debug_window_len = 120;

pub fn encodeLeaky(allocator: Allocator, data: []const u8) Allocator.Error![]const u8 {
    var result = try ArrayList(u8).initCapacity(allocator, data.len);

    for (data) |ch| {
        if (std.ascii.isPrint(ch)) {
            result.appendAssumeCapacity(ch);
        } else {
            result.appendAssumeCapacity('.');
        }
    }

    return try result.toOwnedSlice();
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

    allocator: Allocator,
    debug: bool = false,
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
            .allocator = allocator,
            .input = complete_input,
            .stack = BitStack.init(allocator),
        };
    }

    fn peek(self: Self) ParseFromValueError!u8 {
        if (self.cursor < self.input.len) {
            return self.input[self.cursor];
        } else {
            return error.UnexpectedEndOfInput;
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
                    return error.UnexpectedEndOfInput;
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
                @panic("stepped in some shit");
            },
        }
    }

    pub fn nextRaw(self: *Self) ParseFromValueError![]const u8 {
        const value_start = self.cursor;
        if (self.debug) {
            std.debug.print("doing some raw shit cursor={}\n", .{self.cursor});
        }

        switch (try self.next()) {
            .string, .number => {},
            .array_begin => {
                while (true) {
                    const n = try self.peekNextTokenType();
                    if (n == .array_end) {
                        _ = try self.next();
                        break;
                    }

                    _ = try self.nextRaw();
                }
            },
            .object_begin => {
                while (true) {
                    const n = try self.peekNextTokenType();
                    if (n == .object_end) {
                        _ = try self.next();
                        break;
                    }

                    // skip object key
                    _ = try self.next();
                    _ = try self.nextRaw();
                }
            },
            else => unreachable,
        }

        if (self.debug) {
            std.debug.print("end of real shit: {}\n", .{self.cursor});
        }

        return self.input[value_start..self.cursor];
    }

    pub fn next(self: *Self) ParseFromValueError!Token {
        state_loop: while (true) {
            if (self.debug) {
                const end = if (self.cursor + debug_window_len > self.input.len) self.input.len else self.cursor + debug_window_len;
                const encoded = try encodeLeaky(self.allocator, self.input[self.cursor..end]);
                std.debug.print("state={}, cursor={} input={s}\n", .{ self.state, self.cursor, encoded });
                self.allocator.free(encoded);
            }
            if (self.cursor == self.input.len) {
                if (self.state == .value) {
                    return .end_of_document;
                } else {
                    return error.UnexpectedEndOfInput;
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
                            if (self.debug) std.debug.print("found start of an object\n", .{});
                            return .object_begin;
                        },
                        'l' => {
                            self.cursor += 1;
                            try self.stack.push(LIST_MODE);
                            if (self.debug) std.debug.print("found start of a list\n", .{});
                            return .array_begin;
                        },
                        'e' => {
                            self.cursor += 1;
                            if (self.stack.bit_len == 0) {
                                return error.UnexpectedToken;
                            }
                            switch (self.stack.pop()) {
                                LIST_MODE => {
                                    if (self.debug) std.debug.print("found end of a list\n", .{});
                                    return .array_end;
                                },
                                OBJECT_MODE => {
                                    if (self.debug) std.debug.print("found end of an object\n", .{});
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

                    const res = Token{ .number = self.input[value_start .. self.cursor - 1] };
                    if (self.debug) std.debug.print("parsed number: {any}\n", .{res});
                    return res;
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
                        return error.UnexpectedEndOfInput;
                    }

                    const string_start = self.cursor;
                    self.cursor += len;
                    self.state = .value;

                    const res = Token{ .string = self.input[string_start..self.cursor] };
                    if (self.debug) {
                        const end = if (res.string.len > debug_window_len) debug_window_len else res.string.len;
                        const suffix: []const u8 = if (res.string.len == end) "" else "...";
                        const encoded = try encodeLeaky(self.allocator, res.string[0..end]);
                        std.debug.print("parsed string: '{s}'{s}\n", .{ encoded, suffix });
                        self.allocator.free(encoded);
                    }
                    return res;
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
    UnknownField,
    OtherError,
};

pub fn parseFromTokenSourceLeaky(comptime T: type, allocator: Allocator, scanner_or_reader: *Scanner) ParseFromValueError!T {
    const value = try innerParse(T, allocator, scanner_or_reader);

    assert(.end_of_document == try scanner_or_reader.next());

    return value;
}

fn skip(source: *Scanner) ParseFromValueError!void {
    if (source.debug) std.debug.print("skipping parsing\n", .{});

    switch (try source.next()) {
        .string, .number => {},
        .array_begin => {
            while (true) {
                const next = try source.peekNextTokenType();
                if (next == .array_end) {
                    _ = try source.next();
                    break;
                }

                try skip(source);
            }
        },
        .object_begin => {
            while (true) {
                const n = try source.peekNextTokenType();
                if (n == .object_end) {
                    _ = try source.next();
                    break;
                }

                // skip object key
                _ = try source.next();
                try skip(source);
            }
        },
        else => @panic("todo"),
    }
}

pub fn innerParse(comptime T: type, allocator: Allocator, source: *Scanner) ParseFromValueError!T {
    if (source.debug) std.debug.print("trying to parse: {s}\n", .{@typeName(T)});
    switch (@typeInfo(T)) {
        .Int, .ComptimeInt => {
            const token = try source.next();
            const slice = switch (token) {
                .number => |slice| slice,
                else => return error.UnexpectedToken,
            };
            return std.fmt.parseInt(T, slice, 10);
        },
        .Array => |arrayinfo| {
            switch (try source.peekNextTokenType()) {
                .array_begin => {
                    var r: T = undefined;
                    var i: usize = 0;
                    assert(try source.next() == .array_begin);
                    while (i < arrayinfo.len) : (i += 1) {
                        r[i] = try innerParse(arrayinfo.child, allocator, source);
                    }

                    if (try source.next() != .array_end) return error.UnexpectedToken;
                    return r;
                },
                .string => {
                    @panic("TODO");
                },
                else => {
                    @panic("unreachable");
                },
            }
        },
        .Struct => |structinfo| {
            if (structinfo.is_tuple) {
                @panic("unimplemented");
            }

            if (source.debug) {
                const end = if (source.cursor + debug_window_len > source.input.len) source.input.len else source.cursor + debug_window_len;
                const encoded = try encodeLeaky(allocator, source.input[source.cursor..end]);
                std.debug.print("state={} input={s}\n", .{ source.state, encoded });
                allocator.free(encoded);
            }

            if (std.meta.hasFn(T, "bencodeParse")) {
                return T.bencodeParse(allocator, source);
            }

            assert(try source.next() == .object_begin);

            var r: T = undefined;
            var fields_seen = [_]bool{false} ** structinfo.fields.len;

            while (true) {
                const name = try source.next();
                const field_name = switch (name) {
                    inline .string => |slice| slice,
                    .object_end => {
                        break;
                    },
                    else => {
                        return error.UnexpectedToken;
                    },
                };

                inline for (structinfo.fields, 0..) |field, i| {
                    if (field.is_comptime) {
                        @compileError("comptime fields are not supported: " ++ @typeName(T) ++ "." ++ field.name);
                    }

                    if (std.mem.eql(u8, field.name, field_name)) {
                        if (source.debug) std.debug.print("trying to parse value for field: {s}\n", .{field.name});
                        @field(r, field.name) = try innerParse(field.type, allocator, source);

                        fields_seen[i] = true;
                        break;
                    }
                } else {
                    if (source.debug) std.debug.print("no target found for field: {s}\n", .{field_name});
                    try skip(source);
                }
            }

            inline for (structinfo.fields, 0..) |field, i| {
                if (!fields_seen[i]) {
                    if (@typeInfo(field.type) == .Optional) {
                        @field(r, field.name) = null;
                        break;
                    } else {
                        @panic("field '" ++ field.name ++ "' is not optional and missing");
                    }
                }
            }

            return r;
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
                                if (source.debug) std.debug.print("getting next token for array: {any}\n", .{tt});
                                switch (tt) {
                                    .array_end => {
                                        _ = try source.next();
                                        if (source.debug) std.debug.print("got end of array\n", .{});
                                        break;
                                    },
                                    else => {},
                                }

                                const val = try innerParse(ptr_info.child, allocator, source);
                                if (source.debug) std.debug.print("appending value: {any}\n", .{val});
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
        .Optional => |info| {
            return try innerParse(info.child, allocator, source);
        },
        else => @compileError("unable to parse into type '" ++ @typeName(T) ++ "'"),
    }
    unreachable;
}

const ParseFromSliceLeakyOptions = struct {
    debug: bool = false,
};

pub fn parseFromSliceLeaky(comptime T: type, allocator: Allocator, s: []const u8, options: ParseFromSliceLeakyOptions) ParseFromValueError!T {
    var scanner = Scanner.initCompleteInput(allocator, s);
    defer scanner.deinit();

    scanner.debug = options.debug;

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
    try testing.expectEqual(123, try parseFromSliceLeaky(i32, testing.allocator, "i123e", .{}));
    try testing.expectEqual(-123, try parseFromSliceLeaky(i32, testing.allocator, "i-123e", .{}));
    try testing.expectEqualSlices(u8, "ala", try parseFromSliceLeaky([]const u8, testing.allocator, "3:ala", .{}));
    const list_result = try parseFromSliceLeaky([]i32, testing.allocator, "li1ei2ei3ee", .{});
    try testing.expectEqualSlices(i32, &[3]i32{ 1, 2, 3 }, list_result);
    defer testing.allocator.free(list_result);

    const t_d = struct {
        x: i32,
    };
    const t = struct {
        a: i32,
        s: []const u8,
        l: [2]i32,
        d: t_d,
    };
    const t_result = try parseFromSliceLeaky(t, testing.allocator, "d1:ai123e1:s3:ala1:lli1ei2ee1:dd1:xi1eee", .{});
    try testing.expectEqualDeep(t{
        .a = 123,
        .s = "ala",
        .l = [2]i32{ 1, 2 },
        .d = t_d{ .x = 1 },
    }, t_result);

    const result = try parseFromSliceLeaky(struct { @"ala ma kota": i32 }, testing.allocator, "d11:ala ma kotai1ee", .{});
    try testing.expectEqual(result.@"ala ma kota", 1);
}

test "decode struct with missing fields" {
    const t = struct { a: i32 };

    const t_result = try parseFromSliceLeaky(t, testing.allocator, "d7:missingli1ee1:ai123ee", .{});
    try testing.expectEqualDeep(t{ .a = 123 }, t_result);
}

test "decode double list" {
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const list_result = try parseFromSliceLeaky([][]const []const u8, alloc, "ll11:ala ma kotael10:kot ma aleee", .{});
    _ = list_result;
}

test "get raw data" {
    const t = struct {
        raw: []const u8,

        pub fn bencodeParse(allocator: Allocator, source: anytype) ParseFromValueError!@This() {
            var r: @This() = undefined;

            switch (@typeInfo(@This())) {
                .Struct => |structinfo| {
                    if (try source.next() != .object_begin) @panic("xd");
                    while (true) {
                        const name = try source.next();
                        const field_name = switch (name) {
                            inline .string => |slice| slice,
                            .object_end => {
                                break;
                            },
                            else => unreachable,
                        };

                        if (std.mem.eql(u8, field_name, "raw")) {
                            @field(r, "raw") = try source.nextRaw();
                            continue;
                        }
                        inline for (structinfo.fields) |field| {
                            if (std.mem.eql(u8, field.name, field_name)) {
                                @field(r, field.name) = try innerParse(field.type, allocator, source);
                            }
                        }
                    }
                },
                else => unreachable,
            }

            return r;
        }
    };

    const result = try parseFromSliceLeaky(t, testing.allocator, "d3:rawd1:ai123eee", .{ .debug = true });
    try testing.expectEqualStrings("d1:ai123ee", result.raw);
}
