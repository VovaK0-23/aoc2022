const std = @import("std");
const ArrayList = std.ArrayList;
const indexOf = std.mem.indexOf;
const parseInt = std.fmt.parseInt;
const stdout = std.io.getStdOut().writer();

const RADIX10 = 10; // base or the number of unique digits used to represent numbers

const Sensor = struct {
    point: Point,
    distance: i32,
};

const Point = struct {
    x: i32,
    y: i32,
};

const Range = struct {
    s: i32,
    e: i32,
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

    var sensors = try parseFile(file, allocator);
    defer allocator.free(sensors);

    try part1(sensors, args, allocator);
    try part2(sensors, allocator);
}

fn part1(sensors: []Sensor, args: [][]const u8, allocator: std.mem.Allocator) !void {
    const targetRow = if (args.len > 2) parseInt(i32, args[2], RADIX10) catch |err| {
        if (err == error.InvalidCharacter)
            try stdout.print("Target row should be a number, got: {s}\n", .{args[2]})
        else
            try stdout.print("{s}\n", .{err});
        std.os.exit(1);
    } else 2000000;

    var ranges = ArrayList(Range).init(allocator);
    defer ranges.deinit();

    for (sensors) |sensor| {
        if (intersectsRow(sensor, targetRow)) {
            const effDist = sensor.distance - abs(sensor.point.y - targetRow);
            const start = sensor.point.x - effDist;
            const end = sensor.point.x + effDist;
            try ranges.append(Range{ .s = start, .e = end });
        }
    }

    try mergeRanges(&ranges, allocator);

    var count: i32 = 0;
    for (ranges.items) |range| {
        count += range.e - range.s;
    }
    try stdout.print("Part 1: {d}\n", .{count});
}

const MAX_COORDINATE_VALUE: i32 = 4000000;
fn part2(sensors: []Sensor, allocator: std.mem.Allocator) !void {
    var i: i32 = 0;
    var res: i64 = 0;
    var ranges = std.ArrayList(Range).init(allocator);
    defer ranges.deinit();
    while (i < MAX_COORDINATE_VALUE) : (i += 1) {
        ranges.items.len = 0;
        for (sensors) |sensor| {
            if (intersectsRow(sensor, i)) {
                const effDist = sensor.distance - abs(sensor.point.y - i);
                const start = sensor.point.x - effDist;
                const end = sensor.point.x + effDist;
                const s = if (start < 0) 0 else start;
                const e = if (end > MAX_COORDINATE_VALUE) MAX_COORDINATE_VALUE else end;
                try ranges.append(Range{ .s = s, .e = e });
            }
        }
        try mergeRanges(&ranges, allocator);

        if (ranges.items.len > 1) {
            res = (ranges.items[0].e + 1) * @intCast(i64, MAX_COORDINATE_VALUE) + i;
            break;
        }
    }
    try stdout.print("Part 2: {d}\n", .{res});
}

fn mergeRanges(ranges: *ArrayList(Range), allocator: std.mem.Allocator) !void {
    var rangeArray: []Range = ranges.toOwnedSlice();
    defer allocator.free(rangeArray);

    std.sort.sort(Range, rangeArray, {}, compRangeS);

    var currentRange = rangeArray[0];
    for (rangeArray[1..]) |range| {
        if (range.s > currentRange.e) {
            // Ranges don't overlap, append the current merged range and start a new one
            try ranges.append(currentRange);
            currentRange = range;
        } else if (range.e > currentRange.e) {
            // Ranges overlap, update the end of the current merged range
            currentRange.e = range.e;
        }
    }
    // Append the last merged range
    try ranges.append(currentRange);
}

fn compRangeS(_: void, a: Range, b: Range) bool {
    return a.s < b.s;
}

fn intersectsRow(sensor: Sensor, row: i32) bool {
    const start = sensor.point.y - sensor.distance;
    const end = sensor.point.y + sensor.distance;
    return (row >= start) and (row <= end);
}

fn parseFile(file: std.fs.File, allocator: std.mem.Allocator) ![]Sensor {
    const stream = std.io.bufferedReader(file.reader()).reader();
    var buf: [1024]u8 = undefined;
    var lineCount: u8 = 0;
    var sensors = ArrayList(Sensor).init(allocator);
    defer sensors.deinit();
    while (try stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        lineCount += 1;
        const sensor = parseLine(line) catch |err| {
            try stdout.print("Line {d} invalid format: {s}\n", .{ lineCount, line });
            try stdout.print("Error: {s}\n", .{err});
            std.os.exit(1);
        };
        try sensors.append(sensor);
    }
    const sensorsArr = sensors.toOwnedSlice();
    return sensorsArr;
}

fn parseLine(line: []const u8) !Sensor {
    const colonPos =
        if (indexOf(u8, line, ":")) |pos| pos else return (error.NoColon);
    const sensorPoint = try parsePoint(line[0..colonPos]);
    const beaconPoint = try parsePoint(line[colonPos..]);
    const manhattanDistance =
        abs(sensorPoint.x - beaconPoint.x) + abs(sensorPoint.y - beaconPoint.y);

    return Sensor{
        .point = sensorPoint,
        .distance = manhattanDistance,
    };
}

fn parsePoint(str: []const u8) !Point {
    const xStart = indexOf(u8, str, "x=").? + 2;
    const yStart = indexOf(u8, str, "y=").? + 2;
    const xEnd = indexOf(u8, str, ",").?;
    const yEnd = str.len;
    const x = try parseInt(i32, str[xStart..xEnd], RADIX10);
    const y = try parseInt(i32, str[yStart..yEnd], RADIX10);
    return Point{ .x = x, .y = y };
}

fn abs(x: i32) i32 {
    if (x < 0) return -x;
    return x;
}
