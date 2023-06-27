<?php
if (count($argv) > 1) {
  $filepath = $argv[1];
  [$cubes, $min, $max] = parse_file($filepath);
  part1($cubes);
  part2($cubes, $min, $max);
} else {
  echo "Usage: php main.php <input-path>\n";
}

function parse_file($filepath)
{
  echo 'Input file: ', $filepath, "\n";

  $lines = file($filepath, FILE_IGNORE_NEW_LINES);
  $cubes = [];
  $min = [PHP_INT_MAX, PHP_INT_MAX, PHP_INT_MAX];
  $max = [0, 0, 0];

  foreach ($lines as $line) {
    $cubes[$line] = true;
    $cube = explode(',', $line);

    foreach ($cube as $i => $value) {
      $min[$i] = min($min[$i], $value - 1);
      $max[$i] = max($max[$i], $value + 1);
    }
  }

  return [$cubes, $min, $max];
}

function part1($cubes)
{
  $surfaceArea = 0;

  foreach ($cubes as $line => $_) {
    $cube = explode(',', $line);
    $neighbors = build_neighbors($cube);

    foreach ($neighbors as $neighbor) {
      $neighbor = implode(',', $neighbor);
      if (!isset($cubes[$neighbor])) {
        $surfaceArea++;
      }
    }
  }

  echo 'Part 1: ', $surfaceArea, "\n";
}

function part2($lava_cubes, $min, $max)
{
  $q = [$min];
  $water_cubes = [];
  $surfaceArea = 0;

  // bfs
  while (!empty($q)) {
    $cube = array_shift($q);
    $line = implode(',', $cube);
    if (isset($water_cubes[$line])) {
      continue;
    }
    $water_cubes[$line] = true;
    $neighbors = build_neighbors($cube);

    foreach ($neighbors as $neighbor) {
      if (valid_cube($neighbor, $min, $max)) {
        $nline = implode(',', $neighbor);
        if (!isset($lava_cubes[$nline])) {
          $q[] = $neighbor;
        } else {
          $surfaceArea++;
        }
      }
    }
  }

  echo 'Part 2: ', $surfaceArea, "\n";
}

function build_neighbors($cube)
{
  [$x, $y, $z] = $cube;

  return [
    [$x, $y - 1, $z], // Up
    [$x, $y + 1, $z], // Down
    [$x - 1, $y, $z], // Left
    [$x + 1, $y, $z], // Right
    [$x, $y, $z - 1], // Front
    [$x, $y, $z + 1], // Back
  ];
}

function valid_cube($cube, $min, $max)
{
  [$x, $y, $z] = $cube;
  [$maxX, $maxY, $maxZ] = $max;
  [$minX, $minY, $minZ] = $min;

  return $x >= $minX &&
    $x <= $maxX &&
    $y >= $minY &&
    $y <= $maxY &&
    $z >= $minZ &&
    $z <= $maxZ;
}
