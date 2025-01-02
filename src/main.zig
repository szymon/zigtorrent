const std = @import("std");

const Allocator = std.mem.Allocator;

const bencode = @import("bencode.zig");
const tracker = @import("tracker.zig");
const utils = @import("utils.zig");

const Torrent = @import("torrent.zig").Torrent;

fn loadTorrent(allocator: Allocator, path: []const u8) anyerror!Torrent {
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_reader = buf_reader.reader();

    const data = try in_reader.readAllAlloc(allocator, 4 * 1024 * 1024);

    return try bencode.parseFromSliceLeaky(Torrent, allocator, data, .{ .debug = true });
}

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const args = std.os.argv;

    if (args.len < 2) {
        try stdout.print("USAGE: {s} file\n", .{args[0]});
        try bw.flush();
        return;
    }

    const path_to_torrent = args[1];

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const deinit_status = gpa.deinit();
        //fail test; can't try in defer as defer is executed after we return
        if (deinit_status == .leak) std.testing.expect(false) catch @panic("TEST FAIL");
    }

    const alloc = gpa.allocator();
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const arenaAlloc = arena.allocator();

    const torrent = try loadTorrent(arenaAlloc, std.mem.span(path_to_torrent));
    try torrent.displayInfo(arenaAlloc, stdout);
    try stdout.print("peer id: {s}\n", .{utils.peerId()});

    try bw.flush();

    {
        switch (try tracker.getTrackerResponse(arenaAlloc, torrent)) {
            .success => |response| {
                try stdout.print(
                    \\TrackerResponse:
                    \\  interval: {}
                    \\  tracker id: {s}
                    \\  complete: {}
                    \\  incomplete: {}
                    \\  peers:
                    \\
                , .{
                    response.interval,
                    response.@"tracker id",
                    response.complete,
                    response.incomplete,
                });

                for (response.peers) |peer| {
                    try stdout.print("{{ id: {s}, ip: {s}, port: {} }}", .{
                        peer.@"peer id",
                        peer.ip,
                        peer.port,
                    });
                }
            },
            .err => |err| {
                try stdout.print("TrackerResponse\n  failure reason: {s}\n", .{err.@"failure reason"});
            },
        }
    }

    try bw.flush();
}
