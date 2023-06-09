const std = @import("std");
const indexOf = std.mem.indexOf;
const parseInt = std.fmt.parseInt;
const stdout = std.io.getStdOut().writer();

const Sensor = struct {
    point: Point,
    distance: i32,
};

const Point = struct {
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
        try stdout.print("Usage: zig build run -- <input-path> <target-row>\n", .{});
        return;
    }

    const filePath = args[1];
    const file = std.fs.cwd().openFile(filePath, .{ .read = true }) catch |err| {
        if (err == error.FileNotFound)
            try stdout.print("File Not Found: {s}\n", .{filePath})
        else
            try stdout.print("{s}\n", .{err});
        std.os.exit(1);
    };
    defer file.close();

    const target_row = if (args.len > 2) parseInt(i32, args[2], 10) catch |err| {
        if (err == error.InvalidCharacter)
            try stdout.print("Target row should be a number, got: {s}\n", .{args[2]})
        else
            try stdout.print("{s}\n", .{err});
        std.os.exit(1);
    } else 2000000;

    var target_xs = std.AutoHashMap(i32, bool).init(allocator);
    defer target_xs.deinit();

    var sensors = try parseFile(file, allocator);
    defer sensors.deinit();

    for (sensors.items) |sensor| {
        if (intersectsTargetRow(sensor, target_row)) {
            const effDist = sensor.distance - abs(sensor.point.y - target_row);
            var start = sensor.point.x - effDist;
            const end = sensor.point.x + effDist;
            while (start < end) : (start += 1) {
                try target_xs.put(start, true);
            }
        }
    }

    try stdout.print("Part 1: {d}\n", .{target_xs.count()});
}

fn intersectsTargetRow(sensor: Sensor, num: i32) bool {
    const start = sensor.point.y - sensor.distance;
    const end = sensor.point.y + sensor.distance;
    return (num >= start) and (num <= end);
}

fn parseFile(file: std.fs.File, allocator: std.mem.Allocator) !std.ArrayList(Sensor) {
    const stream = std.io.bufferedReader(file.reader()).reader();
    var buf: [1024]u8 = undefined;
    var lineCount: u8 = 0;
    var sensors = std.ArrayList(Sensor).init(allocator);
    while (try stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        lineCount += 1;
        const sensor = parseLine(line) catch |err| {
            try stdout.print("Line {d} invalid format: {s}\n", .{ lineCount, line });
            try stdout.print("Error: {s}\n", .{err});
            std.os.exit(1);
        };
        try sensors.append(sensor);
    }
    return sensors;
}

fn parseLine(line: []const u8) !Sensor {
    const colonPos =
        if (indexOf(u8, line, ":")) |pos| pos else return (error.NoColon);

    const sensorPoint = try parsePoint(line[0..colonPos]);
    const beaconPoint = try parsePoint(line[colonPos..]);

    const x1 = sensorPoint.x;
    const x2 = beaconPoint.x;
    const y1 = sensorPoint.y;
    const y2 = beaconPoint.y;
    const manhattanDistance = abs(x1 - x2) + abs(y1 - y2);

    return Sensor{
        .point = sensorPoint,
        .distance = manhattanDistance,
    };
}

fn parsePoint(str: []const u8) !Point {
    const xStart = indexOf(u8, str, "x=").? + 2; // Find the index of 'x=' and add 2 to skip it
    const yStart = indexOf(u8, str, "y=").? + 2; // Find the index of 'y=' and add 2 to skip it
    const xEnd = indexOf(u8, str, ",").?; // Find the index of the comma after x value
    const yEnd = str.len; // Use the length of the line as the end index for y value
    const x = try parseInt(i32, str[xStart..xEnd], 10); // 10 means base/radix
    const y = try parseInt(i32, str[yStart..yEnd], 10);
    return Point{ .x = x, .y = y };
}

fn abs(x: i32) i32 {
    if (x < 0) return -x;
    return x;
}
