# You can find a list of CRAN mirrors at
# https://cran.r-project.org/mirrors.html
options(repos = "https://cloud.r-project.org/")
# Install the image magick package if not already installed
if (!require(magick)) install.packages("magick")

library(magick)

### Constants

# nolint start
EMPTY <- 0
ROCK <- 1
SAND <- 2
SOURCE <- 3
# nolint end

### Utils

read_file_lines <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(paste("File", file_path, "does not exist."))
  }

  return(readLines(file_path))
}

prepare_and_clean <- function(fn, cave_matrix, gif) {
  if (gif) {
    build_path <- "build"
    if (!file.exists(build_path)) {
      dir.create(build_path)
    }
    # Move into the build folder
    setwd(build_path)
    # execute main part
    animation <- fn(cave_matrix, gif)
    # Move into the parent folder
    setwd("..")
    image_write(animation, generate_gif_filename())
    unlink(build_path, recursive = TRUE)
  } else {
    fn(cave_matrix, gif)
  }
}

filename_generator <- function(ext) {
  counter <- 1
  # Function to generate a new filename with counter
  return(\() {
    filename <- paste0("image_", sprintf("%03d", counter), ".", ext)
    counter <<- counter + 1
    return(filename)
  })
}

generate_filename <- filename_generator("png")
generate_gif_filename <- filename_generator("gif")

### Parse

parse_slices <- function(lines) {
  slices <- list()
  for (line in lines) {
    points <- strsplit(line, "->")[[1]]
    # Create an empty matrix to store the slice coordinates
    slice <-
      matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c("x", "y")))

    for (point in points) {
      xy <- strsplit(point, ",")[[1]]
      x <- xy[1]
      y <- xy[2]
      # Add the coordinates to the slice matrix
      slice <- rbind(slice, c(as.integer(x), as.integer(y)))
    }
    # Add the slice matrix to the list of slices
    slices <- append(slices, list(slice))
  }
  return(slices)
}

create_cave_matrix <- function(slices) {
  max_y <- max(sapply(slices, function(slice) max(slice[, "y"])))
  max_x <- max(sapply(slices, function(slice) max(slice[, "x"])))
  min_x <- min(sapply(slices, function(slice) min(slice[, "x"])))
  x_size <- max_x - min_x

  # Create an empty matrix with the determined dimensions
  cave_matrix <- matrix(EMPTY, nrow = max_y + 1, ncol = x_size + 1)
  col_names <- min_x:max_x
  row_names <- 0:max_y
  dimnames(cave_matrix) <- list(row_names, col_names)

  # Populate the matrix with lines from slices
  for (slice in slices) {
    prev_x <- slice[1, "x"]
    prev_y <- slice[1, "y"]
    for (i in seq_len(nrow(slice))) {
      x <- slice[i, "x"]
      y <- slice[i, "y"]
      if (x == prev_x) {
        # Vertical line
        y_seq <- as.character(min(y, prev_y):max(y, prev_y))
        cave_matrix[y_seq, as.character(x)] <- ROCK
      } else if (y == prev_y) {
        # Horizontal line
        x_seq <- as.character(min(x, prev_x):max(x, prev_x))
        cave_matrix[as.character(y), x_seq] <- ROCK
      }
      prev_x <- x
      prev_y <- y
    }
  }

  cave_matrix["0", "500"] <- SOURCE
  return(cave_matrix)
}

### Core

source <- function(cave_matrix) {
  list(
    row = which(rownames(cave_matrix) == "0"),
    col = which(colnames(cave_matrix) == "500")
  )
}

simulate_sand <- function(cave_matrix, current, source, sand_units, callback) {
  move_sand <- \(row_diff, col_diff) {
    # Move the sand down by updating the current position
    cave_matrix[current$row, current$col] <<- EMPTY
    current$row <<- current$row + row_diff
    current$col <<- current$col + col_diff
    cave_matrix[current$row, current$col] <<- SAND
  }

  dist_to_empty <- match(
    TRUE,
    cave_matrix[
      (current$row + 1):nrow(cave_matrix),
      current$col
    ] %in% c(ROCK, SAND),
    nomatch = 1
  ) - 1

  # Check the cell below the current position
  if (dist_to_empty) {
    move_sand(dist_to_empty, 0)
  } else if (cave_matrix[current$row + 1, current$col - 1] == EMPTY) {
    move_sand(1, -1)
  } else if (cave_matrix[current$row + 1, current$col + 1] == EMPTY) {
    move_sand(1, 1)
  } else {
    # Sand cannot fall further, so back to source
    current <- source
    cave_matrix[current$row, current$col] <- SOURCE
    sand_units <- sand_units + 1
    callback(cave_matrix)
  }

  list(cave_matrix = cave_matrix, current = current, sand_units = sand_units)
}

part1 <- function(cave_matrix, gif) {
  sand_units <- 0
  imgs <- c()
  if (gif) imgs <- visualize_cave(cave_matrix, imgs)

  source <- source(cave_matrix)
  current <- source
  while (current$row < nrow(cave_matrix) &&
    current$col < ncol(cave_matrix) &&
    current$col > 1) {
    res <- simulate_sand(
      cave_matrix, current, source, sand_units,
      \(cave_matrix) {
        if (gif) imgs <<- visualize_cave(cave_matrix, imgs)
      }
    )
    cave_matrix <- res$cave_matrix
    current <- res$current
    sand_units <- res$sand_units
  }

  cat(paste("Part 1:", sand_units, "\n"))

  if (gif) image_animate(imgs, fps = 25, optimize = TRUE)
}

part2 <- function(cave_matrix, gif) {
  sand_units <- 1
  imgs <- c()
  if (gif) imgs <- visualize_cave(cave_matrix, imgs)

  # Add two rows to matrix
  last_row_name <- as.integer(tail(rownames(cave_matrix), n = 1))
  cave_matrix <- rbind(
    cave_matrix,
    matrix(EMPTY,
      nrow = 1, ncol = ncol(cave_matrix),
      dimnames = list(last_row_name + 1, colnames(cave_matrix))
    ),
    matrix(ROCK,
      nrow = 1, ncol = ncol(cave_matrix),
      dimnames = list(last_row_name + 2, colnames(cave_matrix))
    )
  )

  source <- source(cave_matrix)
  current <- source
  while (
    !(current$row == source$row && current$col == source$col &&
      all(cave_matrix[
        current$row + 1,
        c(current$col, current$col - 1, current$col + 1)
      ] == SAND)
    )
  ) {
    if (current$col == 2) {
      cave_matrix <- cbind(EMPTY, cave_matrix)
      cave_matrix[nrow(cave_matrix), 1] <- ROCK
      colnames(cave_matrix)[1] <-
        as.integer(colnames(cave_matrix)[2]) - 1
      current$col <- current$col + 1
      source$col <- source$col + 1
    }

    if (current$col == ncol(cave_matrix) - 1) {
      cave_matrix <- cbind(cave_matrix, EMPTY)
      cave_matrix[nrow(cave_matrix), ncol(cave_matrix)] <- ROCK
      colnames(cave_matrix)[ncol(cave_matrix)] <-
        as.integer(colnames(cave_matrix)[ncol(cave_matrix) - 1]) + 1
    }

    res <- simulate_sand(
      cave_matrix, current, source, sand_units,
      \(cave_matrix) {
        if (gif && sand_units %% 100 == 0) {
          imgs <<-
            visualize_cave(cave_matrix, imgs)
        }
      }
    )
    cave_matrix <- res$cave_matrix
    current <- res$current
    sand_units <- res$sand_units
  }

  cat(paste("Part 2:", sand_units, "\n"))

  if (gif) image_animate(imgs, fps = 25, optimize = TRUE)
}

visualize_cave <- function(cave_matrix, imgs) {
  nrow <- nrow(cave_matrix)
  ncol <- ncol(cave_matrix)
  filename <- generate_filename()
  # Save the plot as a PNG image
  png(filename)
  # Set up the plot
  plot(1, 1,
    type = "n",
    xlim = c(1, ncol), ylim = c(1, nrow),
    xlab = "x", ylab = "y", axes = FALSE
  )
  # Set the x-axis labels to column names
  axis(1, at = 1:ncol, labels = colnames(cave_matrix))
  # Set the y-axis labels to row names
  axis(2, at = 1:nrow, labels = rev(rownames(cave_matrix)))

  # Add the lines to the plot
  for (i in 1:nrow) {
    for (j in 1:ncol) {
      switch(cave_matrix[i, j],
        ROCK = {
          # Vertical line
          if (i < nrow && cave_matrix[i + 1, j] == ROCK) {
            lines(c(j, j), c(nrow - i + 1, nrow - i), col = "black")
          }
          # Horizontal line
          if (j < ncol && cave_matrix[i, j + 1] == ROCK) {
            lines(c(j, j + 1), c(nrow - i + 1, nrow - i + 1), col = "black")
          }
        },
        SAND = {
          # Draw a sand
          points(j, nrow - i + 1, col = "orange", pch = 24, cex = 0.8)
        },
        SOURCE = {
          # Draw a source
          points(j, nrow - i + 1, col = "red", pch = "+", cex = 1)
        }
      )
    }
  }
  dev.off()
  image <- image_read(filename)
  return(append(imgs, image))
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) stop("Usage: Rscript main.R <input-path> --gif")

file_path <- args[1]
gif <- "--gif" %in% args
lines <- read_file_lines(file_path)
slices <- parse_slices(lines)
cave_matrix <- create_cave_matrix(slices)

prepare_and_clean(part1, cave_matrix, gif)
prepare_and_clean(part2, cave_matrix, gif)
