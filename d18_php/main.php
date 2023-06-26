<?php
if (count($argv) > 1) {
  $filepath = $argv[1];
  $result = parse_file($filepath);
  $cubes = $result['cubes'];
  $min = $result['min'];
  $max = $result['max'];
  $water_min = array_map(fn($value) => $value - 1, $min);
  $water_max = array_map(fn($value) => $value + 1, $max);

  part1($cubes);
  part2($cubes, $water_min, $water_max);
} else {
  echo "Usage: php main.php <input-path>\n";
}

function parse_file($filepath)
{
  echo 'Input file: ', $filepath, "\n";

  $cubes = [];
  $file = fopen($filepath, 'r');
  $min = [PHP_INT_MAX, PHP_INT_MAX, PHP_INT_MAX];
  $max = [0, 0, 0];

  // Read the file line by line using fgets()
  while (($line = fgets($file)) !== false) {
    $cube = explode(',', str_replace("\n", '', $line));
    $cubes[] = $cube;
    for ($i = 0; $i <= 2; $i++) {
      if ($cube[$i] < $min[$i]) {
        $min[$i] = $cube[$i];
      }
      if ($cube[$i] > $max[$i]) {
        $max[$i] = $cube[$i];
      }
    }
  }

  fclose($file);

  return ['cubes' => $cubes, 'min' => $min, 'max' => $max];
}

function part1($cubes)
{
  $surfaceArea = 0;

  foreach ($cubes as $cube) {
    $neighbors = build_neighbors($cube);

    foreach ($neighbors as $neighbor) {
      if (!in_array($neighbor, $cubes)) {
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

  while (!empty($q)) {
    $cube = array_shift($q);
    if (in_array($cube, $water_cubes)) {
      continue;
    }
    $water_cubes[] = $cube;
    $neighbors = build_neighbors($cube);

    foreach ($neighbors as $neighbor) {
      if (valid_cube($neighbor, $min, $max)) {
        if (!in_array($neighbor, $lava_cubes)) {
          array_push($q, $neighbor);
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
  $x = $cube[0];
  $y = $cube[1];
  $z = $cube[2];

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
  $valid = true;
  for ($i = 0; $i <= 2; $i++) {
    if ($cube[$i] < $min[$i] ||  $max[$i] < $cube[$i]) {
      return false;
    }
  }
  return true;
}
