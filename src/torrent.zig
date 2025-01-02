const std = @import("std");
const testing = std.testing;
const assert = std.debug.assert;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const BitStack = std.BitStack;
const zeit = @import("zeit");

const bencode = @import("bencode.zig");
const utils = @import("utils.zig");

pub const Torrent = struct {
    announce: ?[]const u8 = null,
    @"announce-list": ?[][]const []const u8 = null,
    @"url-list": ?[][]const u8 = null,
    // how to fix this???
    info: TorrentInfo,
    comment: []const u8,
    @"created by": []const u8,
    @"creation date": i64,

    const TorrentInfo = struct {
        length: i64,
        name: []const u8,
        @"piece length": i64,
        pieces: []const u8,
        info_hash: []const u8,

        pub fn bencodeParse(allocator: Allocator, source: anytype) bencode.ParseFromValueError!@This() {
            _ = try source.nextRaw();

            var f1 = std.fs.cwd().openFile("torrent.info", .{}) catch return error.OtherError;
            var br = std.io.bufferedReader(f1.reader());
            const data = br.reader().readAllAlloc(allocator, 4 * 1024 * 1024) catch return error.OtherError;
            f1.close();

            const data_copy = try allocator.alloc(u8, data.len);
            @memcpy(data_copy, data);

            // std.debug.print("before sha1: {s}\n", .{std.fmt.fmtSliceHexLower(data)});

            var sha1 = std.crypto.hash.Sha1.init(.{});
            sha1.update(data_copy);
            const info_hash = sha1.finalResult();
            // std.debug.print("after sha1: {s}\n", .{std.fmt.fmtSliceHexLower(data)});

            // FOR SOME REASON THIS SHA1 IS ONLY CORRECT IF THIS CODE IS ALSO HERE
            // FOR SOME REASON THIS SHA1 IS ONLY CORRECT IF THIS CODE IS ALSO HERE
            // FOR SOME REASON THIS SHA1 IS ONLY CORRECT IF THIS CODE IS ALSO HERE
            // FOR SOME REASON THIS SHA1 IS ONLY CORRECT IF THIS CODE IS ALSO HERE
            {
                var file = std.fs.cwd().createFile("torrent.info", .{}) catch return error.OtherError;
                defer file.close();

                var bw = std.io.bufferedWriter(file.writer());
                _ = bw.writer().write(data) catch return error.OtherError;
                bw.flush() catch return error.OtherError;
            }
            const torrentInfo = struct {
                length: i64,
                name: []const u8,
                @"piece length": i64,
                pieces: []const u8,
            };

            const info = try bencode.parseFromSliceLeaky(torrentInfo, allocator, data_copy, .{ .debug = false });

            return .{
                .name = info.name,
                .pieces = info.pieces,
                .@"piece length" = info.@"piece length",
                .length = info.length,
                .info_hash = &info_hash,
            };
        }
    };

    fn creationDate(self: @This()) !zeit.Time {
        const instant = try zeit.instant(.{
            .source = .{ .unix_timestamp = self.@"creation date" },
        });

        return instant.time();
    }

    pub fn infoHash(self: @This()) []const u8 {
        return self.info.info_hash;
    }

    pub fn displayInfo(self: @This(), allocator: Allocator, writer: anytype) anyerror!void {
        const t = try self.creationDate();

        const info_hash = self.infoHash();

        try writer.print(
            \\Torrent:
            \\  name: {s}
            \\  hash: {s} ({s})
            \\  created by: {s}
            \\  creation date:
        , .{
            self.info.name,
            std.fmt.fmtSliceHexLower(info_hash),
            try utils.urlencode(allocator, info_hash),
            self.@"created by",
        });

        try t.strftime(writer, "%Y-%m-%d %H:%M:%S %Z");

        try writer.print(
            \\
            \\  comment: {s}
            \\  primary tracker: {?s}
            \\  additional trackers:
            \\
        , .{
            self.comment,
            self.announce,
        });

        if (self.@"announce-list") |list| {
            for (list) |item| {
                for (item) |it| {
                    try writer.print("    {s}\n", .{it});
                }
            }
        }

        if (self.@"url-list") |list| {
            for (list) |it| {
                try writer.print("    {s}\n", .{it});
            }
        }

        _ = try writer.write("\n");
    }
};

test "bencode torrent" {
    var file = try std.fs.cwd().openFile("ubuntu-24.04.1-live-server-amd64.iso.torrent", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_reader = buf_reader.reader();

    const data = try in_reader.readAllAlloc(testing.allocator, 4_000_000_000);
    defer testing.allocator.free(data);

    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const torrent = try bencode.parseFromSliceLeaky(Torrent, alloc, data, .{ .debug = true });

    const writer = std.io.getStdErr().writer();
    try torrent.displayInfo(alloc, writer);
}
