const std = @import("std");
const http = std.http;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const bencode = @import("bencode.zig");
const Torrent = @import("torrent.zig").Torrent;

pub const Peer = struct {
    @"peer id": []const u8,
    ip: []const u8,
    port: i32,
};

pub const TrackerResponseSuccess = struct {
    interval: i32,
    @"tracker id": []const u8,
    complete: i32,
    incomplete: i32,
    peers: []Peer,
};

pub const TrackerResponseError = struct {
    @"failure reason": []const u8,
};

pub const TrackerResponse = union(enum) {
    success: TrackerResponseSuccess,
    err: TrackerResponseError,
};

fn getResponse(allocator: Allocator, uri: std.Uri) anyerror!TrackerResponse {
    var cli = std.http.Client{ .allocator = allocator };
    defer cli.deinit();

    const server_header_buffer = try allocator.alloc(u8, 4 * 1024);
    defer allocator.free(server_header_buffer);

    std.debug.print("sending request to: {}\n", .{uri});
    var request = try cli.open(.GET, uri, .{
        .server_header_buffer = server_header_buffer,
    });
    defer request.deinit();

    try request.send();
    try request.finish();
    try request.wait();

    const body = try request.reader().readAllAlloc(allocator, 4 * 1024 * 1024);
    if (request.response.status != .ok) {
        return .{
            .err = try bencode.parseFromSliceLeaky(TrackerResponseError, allocator, body, .{}),
        };
    }

    return .{
        .success = try bencode.parseFromSliceLeaky(TrackerResponseSuccess, allocator, body, .{}),
    };
}

pub fn getTrackerResponse(allocator: Allocator, torrent: Torrent) anyerror!TrackerResponse {
    var buf = ArrayList(u8).init(allocator);
    const info_hash = torrent.infoHash();

    if (torrent.announce == null) {
        return error.XD;
    }

    // do i really need to write my own urlencode???
    try buf.writer().print("{s}?info_hash={s}", .{ torrent.announce.?, info_hash });

    const uri = try std.Uri.parse(buf.items);
    return try getResponse(allocator, uri);
}
