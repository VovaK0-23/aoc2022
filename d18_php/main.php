<?php
if (count($argv) > 1) {
  $filepath = $argv[1];

  $cubes = parse_file($filepath);

  $surfaceArea = 0;

  foreach ($cubes as $cube) {
    $x = $cube[0];
    $y = $cube[1];
    $z = $cube[2];

    $neighbors = [
      [$x, $y - 1, $z], // Up
      [$x, $y + 1, $z], // Down
      [$x - 1, $y, $z], // Left
      [$x + 1, $y, $z], // Right
      [$x, $y, $z - 1], // Front
      [$x, $y, $z + 1], // Back
    ];

    foreach ($neighbors as $neighbor) {
      if (!in_array($neighbor, $cubes)) {
        $surfaceArea++;
      }
    }
  }

  echo $surfaceArea . "\n";
} else {
  echo "Usage: php main.php <input-path>\n";
}

function parse_file($filepath)
{
  echo 'Input file: ' . $filepath . "\n";

  $cubes = [];
  $file = fopen($filepath, 'r');

  // Read the file line by line using fgets()
  while (($line = fgets($file)) !== false) {
    $cubes[] = explode(',', str_replace("\n", '', $line));
  }

  fclose($file);

  return $cubes;
}
