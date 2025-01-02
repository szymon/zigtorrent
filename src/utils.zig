const std = @import("std");
const Allocator = std.mem.Allocator;

const peer_id_length = 20;
pub fn peerId() [peer_id_length]u8 {
    return "--this-is-my-new-bit".*;
}

pub fn urlencode(allocator: Allocator, input: []const u8) ![]u8 {
    const hex = "0123456789ABCDEF";
    var encoded = try allocator.alloc(u8, input.len * 3);
    var i: usize = 0;

    for (input) |c| {
        if (std.ascii.isAlphanumeric(c) or c == '-' or c == '_' or c == '.' or c == '~') {
            encoded[i] = c;
            i += 1;
        } else {
            encoded[i] = '%';
            encoded[i + 1] = hex[c >> 4];
            encoded[i + 2] = hex[c & 0xF];
            i += 3;
        }
    }

    return encoded[0..i];
}
