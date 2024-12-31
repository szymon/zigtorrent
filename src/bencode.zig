const std = @import("std");
const testing = std.testing;
const assert = std.debug.assert;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const default_max_len = 4 * 1024 * 1024;

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
/// i123e: 
pub const TokenType = enum {
    object_begin,
    object_end,
    array_begin,
    array_end,
    value_end,
    true,
    false,
    null,
    number,
    string,
    end_of_document,
};

pub const AllocWhen = enum { alloc_if_needed, alloc_always };

pub const Scanner = struct {
    const Self = @This();
    pub const NextError = Error || Allocator.Error || std.fmt.ParseIntError || error{BufferUnderrun};
    pub const AllocError = Error || Allocator.Error || error{ValueToLarge};
    pub const PeekError = Error || error{BufferUnderrun};
    pub const SkipError = Error || Allocator.Error;
    pub const AllocIntoArrayListError = AllocError || error{BufferUnderrun};

    input: []const u8 = "",
    cursor: usize = 0,
    value_start: usize = 0,
    is_end_of_input: bool = false,
    string_len: usize = 0,
    expect_closing_tag: bool = false,
    state: enum {
        value,
        post_value,

        number_start,
        string_start,
        string_value,
        value_end,
    } = .value,

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn initCompleteInput(allocator: Allocator, complete_input: []const u8) Self {
        _ = allocator;
        return .{
            .input = complete_input,
        };
    }

    pub fn peekNextTokenType(self: *Self) PeekError!TokenType {
        std.debug.print("peek {}: {} {s}\n", .{ self.state, self.cursor, self.input[self.cursor..] });
        state_loop: while (true) {
            switch (self.state) {
                .value => {
                    switch (try self.peek()) {
                        'i' => return .number,
                        '0'...'9' => return .string,
                        'l' => return .array_begin,
                        'd' => return .object_begin,
                        'e' => return .value_end,
                        else => return error.SyntaxError,
                    }
                },
                .post_value => {
                    if (self.expect_closing_tag) {
                        self.expect_closing_tag = false;
                        const c = try self.peek();
                        if (c == 'e') {
                            self.cursor += 1;
                        } else {
                            return error.SyntaxError;
                        }
                    }

                    if (self.cursor < self.input.len) {
                        self.state = .value;
                        continue :state_loop;
                    } else {
                        return .end_of_document;
                    }
                },
                else => @panic("unimplemented"),
            }
            unreachable;
        }
    }

    pub fn next(self: *Self) NextError!Token {
        state_loop: while (true) {
            std.debug.print("loop {}: {} {s}\n", .{ self.state, self.cursor, self.input[self.cursor..] });
            switch (self.state) {
                .value => {
                    switch (try self.peek()) {
                        'i' => {
                            self.cursor += 1;
                            self.value_start = self.cursor;
                            self.state = .number_start;
                            self.expect_closing_tag = true;
                            continue :state_loop;
                        },
                        '0'...'9' => {
                            self.value_start = self.cursor;
                            self.state = .string_start;
                            continue :state_loop;
                        },
                        'l' => {
                            self.cursor += 1;
                            self.state = .value;
                            return .array_begin;
                        },
                        'e' => {
                            self.state = .value_end;
                            continue :state_loop;
                        },
                        else => @panic("unimplemented"),
                    }
                },
                .number_start => {
                    while (self.cursor < self.input.len) : (self.cursor += 1) {
                        switch (self.input[self.cursor]) {
                            '-' => continue,
                            '0'...'9' => continue,
                            else => {
                                self.state = .post_value;
                                return Token{ .number = self.takeValueSlice() };
                            },
                        }
                    }
                },
                .string_start => {
                    while (self.cursor < self.input.len) : (self.cursor += 1) {
                        switch (self.input[self.cursor]) {
                            '0'...'9' => continue,
                            ':' => {
                                const slice = self.takeValueSlice();
                                std.debug.print("parsing len from slice: {s}\n", .{slice});
                                const len = try std.fmt.parseInt(usize, slice, 10);
                                self.state = .string_value;
                                self.value_start = self.cursor + 1;
                                self.cursor += 1 + len;
                                continue :state_loop;
                            },
                            else => return error.SyntaxError,
                        }
                    }
                },
                .string_value => {
                    if (self.cursor < self.input.len) return error.BufferUnderrun;
                    const slice = self.input[self.value_start..self.cursor];
                    self.state = .post_value;
                    self.expect_closing_tag = false;
                    return Token{ .string = slice };
                },
                .post_value => {
                    if (self.expect_closing_tag) {
                        self.expect_closing_tag = false;
                        const c = try self.peek();
                        if (c == 'e') {
                            self.cursor += 1;
                        } else {
                            return error.SyntaxError;
                        }
                    }

                    if (self.cursor < self.input.len) {
                        self.state = .value;
                        continue :state_loop;
                    } else {
                        return .end_of_document;
                    }
                },
                .value_end => {
                    if (self.expect_closing_tag) {
                        self.expect_closing_tag = false;
                        const c = try self.peek();
                        if (c == 'e') {
                            self.cursor += 1;
                        } else {
                            return error.SyntaxError;
                        }
                    }
                },
            }
        }

        return .end_of_document;
    }

    fn takeValueSlice(self: *Self) []const u8 {
        const slice = self.input[self.value_start..self.cursor];
        self.value_start = self.cursor;
        return slice;
    }

    fn peek(self: *const Self) !u8 {
        if (self.cursor < self.input.len) {
            return self.input[self.cursor];
        }

        if (self.is_end_of_input) return error.UnexpectedEndOfInput;
        return error.BufferUnderrun;
    }
};

pub const ParseFromValueError = std.fmt.ParseIntError || std.fmt.ParseFloatError || Allocator.Error || error{
    UnexpectedToken,
    InvalidNumber,
    Overflow,
    InvalidEnumTag,
    DuplicateField,
    UnknownField,
    MissingField,
    LengthMismatch,
};

pub fn ParseError(comptime Source: type) type {
    return ParseFromValueError || Source.NextError || Source.PeekError || Source.AllocError;
}

pub fn parseFromTokenSourceLeaky(comptime T: type, allocator: Allocator, scanner_or_reader: *Scanner) ParseError(Scanner)!T {
    const value = try innerParse(T, allocator, scanner_or_reader);

    assert(.end_of_document == try scanner_or_reader.next());

    return value;
}

fn innerParse(comptime T: type, allocator: Allocator, source: *Scanner) ParseError(Scanner)!T {
    switch (@typeInfo(T)) {
        .Int, .ComptimeInt => {
            const token = try source.next();
            const slice = switch (token) {
                .number => |slice| slice,
                else => return error.UnexpectedToken,
            };
            return std.fmt.parseInt(T, slice, 10);
        },
        .Array => |array_info| {
            switch (try source.next()) {
                .string => |slice| {
                    if (array_info.child != u8) return error.UnexpectedToken;

                    const r: T = undefined;

                    if (slice.len != r.len) return error.LengthMismatch;
                    @memcpy(r, slice);
                    return r;
                },
                .array_begin => {
                    var r: T = undefined;
                    var i: usize = 0;
                    while (i < array_info.len) : (i += 1) {
                        r[i] = try innerParse(array_info.child, allocator, source);
                    }

                    if (.array_end != try source.next()) return error.UnexpectedToken;
                    return r;
                },
                else => @panic("unreachable"),
            }
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
                                switch (try source.peekNextTokenType()) {
                                    .array_end => {
                                        _ = try source.next();
                                        break;
                                    },
                                    else => {},
                                }

                                try arraylist.ensureUnusedCapacity(1);
                                arraylist.appendAssumeCapacity(try innerParse(ptr_info.child, allocator, source));
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

pub fn parseFromSliceLeaky(comptime T: type, allocator: Allocator, s: []const u8) ParseError(Scanner)!T {
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
    try testing.expectEqualSlices(i32, &[3]i32{ 1, 2, 3 }, try parseFromSliceLeaky([]i32, testing.allocator, "li1ei2ei3ee"));

    // const t = struct {
    //     a: i32,
    // };

    // const result = try parseFromSliceLeaky(@TypeOf(t), testing.allocator, "d1:ai123ee");
    // try testing.expectEqual(123, result.a);
}
