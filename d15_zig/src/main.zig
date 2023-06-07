const std = @import("std");
const stdout = std.io.getStdOut().writer();

const Sensor = struct {
    coordinate: Coordinate,
    distance: i32,
};

const Coordinate = struct {
    x: i32,
    y: i32,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try stdout.print("Usage: zig build run -- <input-path>\n", .{});
        return;
    }

    const filePath = args[1];
    const file = std.fs.cwd().openFile(filePath, .{ .read = true }) catch |err| {
        if (err == error.FileNotFound) try stdout.print("File Not Found: {s}\n", .{filePath}) else try stdout.print("{s}\n", .{err});
        std.os.exit(1);
    };
    defer file.close();
    const stream = std.io.bufferedReader(file.reader()).reader();

    const target_y = 2000000;
    var target_xs = std.AutoHashMap(i32, bool).init(allocator);
    defer target_xs.deinit();
    var buf: [1024]u8 = undefined;
    var lineCount: u8 = 0;
    while (try stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        lineCount += 1;
        const sensor = parseSensor(line) catch |err| {
            try stdout.print("Line {d} invalid format: {s}\n", .{ lineCount, line });
            try stdout.print("Error: {s}\n", .{err});
            std.os.exit(1);
        };
        if (intersectsTargetRow(sensor, target_y)) {
            const xy = sensor.coordinate;
            const effDist = sensor.distance - abs(xy.y - target_y);
            var start = xy.x - effDist;
            const end = xy.x + effDist;
            while (start < end) : (start += 1) {
                try target_xs.put(start, true);
            }
        }
    }
    try stdout.print("Part 1: {d}\n", .{target_xs.count()});
}

pub fn intersectsTargetRow(sensor: Sensor, num: i32) bool {
    const start = sensor.coordinate.y - sensor.distance;
    const end = sensor.coordinate.y + sensor.distance;
    return (num >= start) and (num <= end);
}

pub fn parseSensor(line: []const u8) !Sensor {
    // Find the position of the colon ':'
    const colonPos = if (std.mem.indexOf(u8, line, ":")) |pos|
        pos
    else
        return (error.NoColon);

    // Extract the substring from the start of the line to the colon position
    const sensorInfoSlice = line[0..colonPos];
    const sensorCoordinate = try parseCoordinate(sensorInfoSlice);
    const beaconInfoSlice = line[colonPos..];
    const beaconCoordinate = try parseCoordinate(beaconInfoSlice);

    const x1 = sensorCoordinate.x;
    const x2 = beaconCoordinate.x;
    const y1 = sensorCoordinate.y;
    const y2 = beaconCoordinate.y;
    const manhattanDistance = abs(x1 - x2) + abs(y1 - y2);

    return Sensor{
        .coordinate = sensorCoordinate,
        .distance = manhattanDistance,
    };
}

pub fn parseCoordinate(str: []const u8) !Coordinate {
    const xStart = std.mem.indexOf(u8, str, "x=").? + 2; // Find the index of 'x=' and add 2 to skip it
    const yStart = std.mem.indexOf(u8, str, "y=").? + 2; // Find the index of 'y=' and add 2 to skip it
    const xEnd = std.mem.indexOf(u8, str, ",").?; // Find the index of the comma after x value
    const yEnd = str.len; // Use the length of the line as the end index for y value
    const x = try std.fmt.parseInt(i32, str[xStart..xEnd], 10); // 10 means base/radix
    const y = try std.fmt.parseInt(i32, str[yStart..yEnd], 10);
    return Coordinate{ .x = x, .y = y };
}

fn abs(x: i32) i32 {
    if (x < 0) return -x;
    return x;
}
